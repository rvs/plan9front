/*
 * RV64G memory management for 39-, 48-, 57- or 64-bit virtual addresses.
 * specifically, virtual to physical address mappings.
 */
#include "u.h"
#include "../port/lib.h"
#include "mem.h"
#include "dat.h"
#include "fns.h"

#include "riscv.h"

/* see mem.h */
/* page table levels start at the leaves (0) and increase up to the root */
#define PDX(v)		PTLX((v), 1)
#define PTX(v)		PTLX((v), 0)

#define PTEADDR(pt, va, lvl)	&((PTE *)(pt))[PTLX(va, lvl)]

/* memory banks may be scattered about, including after first bank */
#define IDMAPTOP ADDRSPCSZ

enum {
	Debugptpget = 0,
	Nowx = 1,		/* flag: make writable pages unexecutable */
	VMAP =	VTOP - VMAPSZ,	/* general mappings */
	Minmb =	16,
	Kernfract = 10, /* arbitrary, fraction of first memory bank for kernel */
};

/*
 * max kernel memory use within KZERO, multiple of 2 or 4 MB.
 * used in mmu.c and memory.c.  must be below VMAP.
 */
uintptr kernmem;

int printmmuput;

static Lock vmaplock;

PTE	asmwalkalloc(uintptr size);
void	iovmapsoc(void);
void	mmudumplvl(uintptr, uintptr, int lvl);

static PTE*	mmuptpget(uintptr va, int level, int *lvlgotp);

/* set size of memory for page tables & qmalloc */
void
setkernmem(void)
{
	uintptr pmsize;

	pmsize = membanks[0].size;
	if (pmsize < Minmb*MB) {	/* sanity: enforce minimum ram */
		print("mem bank 0 size %,llud; assuming %d MB of ram\n",
			(uvlong)pmsize, Minmb);
		pmsize = Minmb*MB;
		sys->pmend = sys->pmbase + pmsize;	/* assume 1 bank */
	}
	if (pmsize <= ADDRSPCSZ)
		kernmem = 200*MB;		/* arbitrary */
	if (kernmem >= pmsize/Kernfract) {
		kernmem = PGROUND(pmsize/Kernfract);
		if (kernmem < Minmb*MB/3)
			kernmem = Minmb*MB/3;
	}
}

uintptr
topram(void)
{
	/*
	 * leave room for Sys at top of bank 0; exclude last MB.
	 */
	return MB;
}

PTE
va2gbit(uintptr va)
{
	return va >= KZERO? PteG: 0;
}

/* insert a leaf PTE entry for va->pa mapping in Toplvl (root) level pt. */
void
mappage(Page *ptroot, PTE *pte, uintptr va, uintptr pa)
{
	PTE ptebits;

	ptebits = PteR | PteW | PteX | Pteleafvalid | va2gbit(va);
	if (va < KZERO)
		ptroot->daddr++;
	if (*pte)
		panic("mappage: pte for va %#p in use", va);
	*pte = PTEPPN(pa) | ptebits;		/* super page leaf */
}

void
idmappage(Page *ptroot, PTE *pte, uintptr va)
{
	mappage(ptroot, pte, va, va);
}

/*
 * set up id map in lower addresses using only top-level
 * (Npglvl-1) superpages.  called before reboot.
 */
void
mmulowidmap(void)
{
	uintptr pg, pgsz;
	Page *page;
	PTE *pte;

	wbinvd();
	page = m->ptroot;
	/* zero user mapping */
	memset((void *)page->va, 0, sizeof(PTE) * page->daddr);
	page->daddr = 0;

	pgsz = PGLSZ(Toplvl);	/* on Sv39, 1GB */

	/* populate top-level identity page table for low addresses */
	pte = (PTE *)page->va;
	for (pg = 0; pg < HOWMANY(ADDRSPCSZ, pgsz); pg++)
		idmappage(page, pte++, pg * pgsz);
	wbinvd();
}

/*
 * set up id map and upper->lower map using only top-level
 * (Npglvl-1) superpages.  called before mmuinit and after low.
 */
void
pageidmap(void)
{
	uintptr pg, pgsz, pa, va;
	Page *page;
	PTE *pte;
	static Page mach0ptrootpg;

	archmmu();		/* work out page sizes (& levels) available */
	assert(m->npgsz >= 1);

	m->ptroot = page = &mach0ptrootpg;
	page->pa = page->va = (uintptr)sys->pteroot; /* zeroed by zerosyssome */

	if (page->pa & KZERO)		/* already using kernel virtual addr */
		page->pa -= KZERO;
	page->daddr = 0;

	pgsz = PGLSZ(Toplvl);		/* on Sv39, 1GB */
	assert(pgsz >= PGSZ);

	/* populate top-level identity page table; keep below PADDR(VMAP) */
	assert((uint)(IDMAPTOP/pgsz) <= Ptpgptes);
	pte = (PTE *)page->va;
	for (pg = 0; pg < HOWMANY(IDMAPTOP, pgsz); pg++)
		idmappage(page, pte++, pg * pgsz);

	/* map upper addresses to lower, up to upper VMAP start */
	pa = 0;
	pte = PTEADDR(page->va, KZERO, Toplvl);
	/* VMAP was 0xffffffffc0000000 (-1GB) on beagle */
	for (va = KZERO; va < VMAP; va += pgsz) {
		mappage(page, pte++, va, pa);
		pa += pgsz;
	}

	putstvec(strap);		/* high virtual */
/**/	putsatp(pagingmode | (page->pa / PGSZ));
}

/*
 * prepare data structures to start risc-v CPUs other than 0.
 *
 * unlike on x86, the harts may all start at boot time (but SBI can delay them).
 * we still have to allocate memory for the secondary harts, so we make
 * them wait until we're ready for them to proceed.
 */

/*
 * Allocate and minimally populate new page tables & Mach for cpu.
 *
 * The data needed per-processor is the sum of the stack, page table pages,
 * and the Mach page.  The layout (Syspercpu) is similar to that described
 * in dat.h for the bootstrap processor (Sys), but with any unused space
 * elided; see start.s, dat.h.
 *
 * NOTE: for now, share the page tables with the bootstrap processor, until
 * this code is worked out, so only the Mach and stack portions are used below.
 */
Mach *
newcpupages(int machno)
{
	Page *page;
	Mach *mach;
	Syspercpu *nsys;

	nsys = mallocalign(sizeof(Syspercpu), PGSZ, 0, 0);
	if(nsys == nil) {
		print("newcpupages: no memory for cpu%d\n", machno);
		return nil;
	}

	/* populate new Syspercpu, which includes new Mach */
	mach = &nsys->mach;
	mach->machno = machno;
	/* stack must immediately precede ptroot.  see dat.h. */
	mach->stack = (uintptr)nsys->machstk;
	coherence();

	sys->machptr[machno] = mach;	/* make Mach visible to new cpu */
	coherence();

	/*
	 * make a copy of the top-level ptroot page into nsys->pteroot,
	 * and point our mach->ptroot Page at it.  the top-level ptroot table
	 * must have i/o devices vmapped by now.
	 */
	memmove(nsys->pteroot, (void *)m->ptroot->va, PTSZ);
	mach->ptroot = page = &mach->ptrootpriv;
	page->va = (uintptr)ensurelow(nsys->pteroot);	/* to start with */
	page->pa = PADDR(nsys->pteroot);
	page->daddr = m->ptroot->daddr;	/* # of user mappings in ptroot */
	coherence();
	return mach;
}

void
cpusalloc(int ncpus)
{
	int machno;

	USED(ncpus);
	if (MACHMAX == 1)
		return;

	clearipi();

	/* allocate memory for all cpus other than 0 */
	for(machno = 1; machno < ncpus+2 && machno < MACHMAX; machno++)
		newcpupages(machno);
}

/*
 * must be called with id map and upper addresses mapped.
 */
void
gohigh(void)
{
	m = ensurehigh(m);
	if (m->ptroot) {
		m->ptroot = ensurehigh(m->ptroot);
		m->ptroot->va = (uintptr)ensurehigh((void *)m->ptroot->va);
	}
	m->stack = (uintptr)ensurehigh((void *)m->stack);
	jumphigh();

	/* now executing in upper space with high static base. */
	putsscratch((uintptr)m);
	putstvec(strap);		/* high virtual */
	sys->machptr[m->machno] = m;
	/* saved return address on stack should be low */
}

#ifdef unused
/*
 * id map must be in place before calling this
 * and this should be the only cpu running.
 */
void
golow(void)
{
	putstvec(ensurelow(strap));		/* virtual but low */
	if (sys->nonline <= 1)
		sys = ensurelow(sys);
	m = ensurelow(m);
	m->ptroot->va = (uintptr)ensurelow((void *)m->ptroot->va);
	m->ptroot = ensurelow(m->ptroot);
	jumplow();
	/* now executing in lower space with low static base. */
	/* saved return address on stack should be high */
}
#endif

void
mmuflushtlb(Page *)
{
	Mpl s;
	Page *ptroot;

	s = splhi();
	ptroot = m->ptroot;
	if(ptroot->daddr){
		/* zero the top-level user-space PTEs */
		clrreserv();
		memset((char *)ptroot->va, 0, ptroot->daddr*sizeof(PTE));
		ptroot->daddr = 0;
	}
	wbinvd();
	splx(s);
}

void
mmuflush(void)
{
	Mpl s;

	s = splhi();
	up->newtlb = 1;
	mmuswitch(up);
	splx(s);
}

static void
mmuptpfree(Proc* proc, int release)
{
	int l;
	PTE *pte;
	Page **last, *page;

	/*
	 * To do here:
	 *	coalesce the clean and release functionality
	 *	(it's either one or the other, and no need for
	 *	wakeup in mmurelease as not using the palloc pool);
	 *	0-based levels, not 1-based, for consistency;
	 *	fix memset level for PGLSZ(1) super pages;
	 *	use a dedicated datastructure rather than Page?
	 */
	for(l = 1; l < Npglvls; l++){		/* from leaf-1 to root */
		last = &proc->mmuptp[l];
		if(*last == nil)
			continue;
		for(page = *last; page != nil; page = page->next){
			if(!release){
				if(l == 1)		/* leaf-1? */
					memset((char *)page->va, 0, PTSZ);
				pte = (PTE *)page->prev->va;
				pte[page->daddr] = 0;
			}
			last = &page->next;
		}
		*last = proc->mmuptp[0];
		proc->mmuptp[0] = proc->mmuptp[l];
		proc->mmuptp[l] = nil;
	}
	m->ptroot->daddr = 0;
	wbinvd();
}

static Page*
mmuptpalloc(void)
{
	void* va;
	Page *page;

	/*
	 * Do not really need a whole Page structure,
	 * but it makes testing this out a lot easier.
	 * Could keep a cache and free excess.
	 */
	if((page = malloc(sizeof(Page))) == nil){
		print("mmuptpalloc Page\n");
		return nil;
	}
	/* malloc above will have called mallocinit if needed */
	if((va = mallocalign(PTSZ, PTSZ, 0, 0)) == nil){
		print("mmuptpalloc va\n");
		free(page);
		return nil;
	}

	page->va = PTR2UINT(va);
	page->pa = PADDR(va);
	page->ref = 1;
	return page;
}

/*
 * switch user mmu mappings to proc's.
 */
void
mmuswitch(Proc* proc)
{
	PTE *pte;
	Page *page, *ptroot;

	if(proc->newtlb){
		mmuptpfree(proc, 0);		/* 0: don't release pages */
		proc->newtlb = 0;
	}

	mmuflushtlb(m->ptroot);	/* zero the top-level user-space PTEs */

	/* populate the top-level PTEs from proc->mmuptp[Npglvl-1] */
	ptroot = m->ptroot;
	pte = (PTE *)ptroot->va;
	for(page = proc->mmuptp[Toplvl]; page != nil; page = page->next){
		/* next level ptr: no leaf bits (PteNonleaf0) */
		pte[page->daddr] = PTEPPN(page->pa) | PteP;
		if(page->daddr >= ptroot->daddr)
			ptroot->daddr = page->daddr+1;
		page->prev = ptroot;
	}
	wbinvd();
}

void
mmurelease(Proc* proc)
{
	Page *page, *next;

	/*
	 * See comments in mmuptpfree above.
	 */
	mmuptpfree(proc, 1);

	for(page = proc->mmuptp[0]; page != nil; page = next){
		next = page->next;
		if(--page->ref)
			panic("mmurelease: page->ref %d", page->ref);
		free((void *)page->va);
		free(page);
	}
	if(proc->mmuptp[0] && palloc.r.p)
		wakeup(&palloc.r);
	proc->mmuptp[0] = nil;
	wbinvd();
}

/* returns a physical address from pte */
static PTE *
nextpte(PTE pte)
{
	if (PAGEPRESENT(pte))
		return (PTE *)PADDRINPTE(pte);
	else
		return nil;
}

/* alloc page table for level l w/ index x, insert it into pte. */
static Page *
allocpt(int l, int x, PTE *pte, Page *prev)
{
	PTE oldpte;
	Page *page;

	for(page = up->mmuptp[l]; page != nil; page = page->next)
		if(page->prev == prev && page->daddr == x)
			return page;

	/*
	 * no such next-level page.  allocate a new next-level page
	 * table page.
	 */
	if(up->mmuptp[0] == 0)
		page = mmuptpalloc();
	else {
		page = up->mmuptp[0];
		up->mmuptp[0] = page->next;
		/* doesn't page->va need to be zeroed? */
	}
	if (page == nil)
		panic("allocpt: out of memory");
	page->daddr = x;
	page->next = up->mmuptp[l];
	up->mmuptp[l] = page;
	page->prev = prev;

	oldpte = *pte;
	/* next level ptr: no leaf bits (PteNonleaf0) */
	mmuwrpte(pte, PTEPPN(page->pa) | PteP);

//	iprint("l %d mmuput: va %#p pte %#p (pa %#p) -> %#p (was %#p)\n",
//		l, page->va, *pte, (uintptr)PADDRINPTE(*pte), pte, oldpte);
	USED(oldpte);
	if(l == Toplvl && x >= m->ptroot->daddr)
		m->ptroot->daddr = x+1;
	return page;
}

/*
 * returns va of page table that maps `va' at level (the parent page table).
 * level 0 is the farthestmost leaves; Toplvl is the root table.
 * called from mmuput and mmuwalk, so beware recursion filling in tables.
 *
 * there's a trick, called `virtual page table', used on 386 and amd64
 * systems to insert the address of the root page table into the root
 * page table, thus allowing a virtual view of the page table tree as a
 * contiguous array with holes for missing page tables.  this is possible
 * because one can't tell (ignoring the Page Size bit) from looking at a
 * 386/amd64 PTE what level of the tree it corresponds to.  on risc-v,
 * each PTE identifies itself as leaf or interior node, so the trick
 * doesn't seem to be possible.
 */
static PTE*
mmuptpget(uintptr va, int level, int *lvlgotp)
{
	int l, idx, lvlgot;
	Page *page, *prev;
	PTE pte;
	PTE *ptp, *ptnxt, *kptp;

	if (level < 0 || level >= Npglvls)
		panic("mmuptpget level %d", level);

	prev = m->ptroot;
	ptp = (PTE *)m->ptroot->pa;		/* Toplvl */
	if (ptp == nil)
		panic("mmuptpget: no pt root");
	l = Toplvl;
	lvlgot = l;

	idx = PTLX(va, l);
	kptp = KADDR((uintptr)ptp);
	pte = kptp[idx];			/* root page pte */
	ptnxt = nextpte(pte);			/* physical */
	if (Debugptpget)
		iprint("mmuptpget 1: va %#p lvl %d pt is va %#p: %#p\n",
			va, l, ptp, pte);

	/*
	 * walk toward a leaf PTE from the level-after-root until `level',
	 * return the containing page.
	 */
	while (--l >= level){
		if (!PAGEPRESENT(pte)) {	/* parent hole? */
			/*
			 * allocate a zeroed page, point pte at it.
			 */
			page = allocpt(l, idx, &kptp[idx], prev);
			pte = kptp[idx];
			ptnxt = nextpte(pte);		/* physical */
			prev = page;
		}
		if (pte & PteLeaf)		/* not a next-level pointer? */
			/* alternative: replace pte with ptr to new page? */
			break;

		lvlgot = l;

		ptp = ptnxt;			/* advance to level l */
		if (l == 0)			/* level 0 has to be a leaf */
			break;
		if (ptp == nil)
			panic("mmuptpget: nil ptp at level %d for %#p", l, va);

		idx = PTLX(va, l);
		kptp = KADDR((uintptr)ptp);
		pte = kptp[idx];		/* root page pte */
		ptnxt = nextpte(pte);		/* physical */
		if (Debugptpget)
			iprint("mmuptpget 2: va %#p lvl %d pt is va %#p: %#p\n",
				va, l, ptp, pte);
	}
	if (Debugptpget)
		iprint("* mmuptpget return va %#p -> %#p idx %d lvl %d\n",
			va, ptp, idx, lvlgot);
	if (lvlgotp)
		*lvlgotp = lvlgot;
	if (ptp)
		return KADDR((uintptr)ptp);
	return nil;
}

static void
prpte(uintptr va, uintptr pa, PTE *pte, PTE ptebits, PTE oldpte)
{
	PTE ptepa;

	ptepa = (uintptr)PADDRINPTE(ptebits);
	iprint("mmuput va %#p pa %#p: pte %#p", va, pa, ptebits);
	if (pa != ptepa)
		iprint(" (pa %#p)", ptepa);
	iprint(" -> &pte %#p (was %#p)\n", pte, oldpte);
}

/*
 * walk from root to leaf, adding page tables as needed to allow writing
 * PTE for va.  pa may contain PTE attributes in PteAttr bits.
 * only called from fault.c.  requires a user context (up != nil).
 */
void
mmuput(uintptr va, uintmem pa, Page*)
{
	Mpl pl;
	int l, lvlgot, x;
	uintptr ova, opa;
	PTE attrs, oldpte, ptebits;
	PTE *pte, *ptp;
	Page *prev;

	opa = pa;
	ova = va;
	pte = nil;
	pl = splhi();
	prev = m->ptroot;
	attrs = PTEATTRBITS(pa);
	pa = PTEADDRBITS(pa);
	oldpte = 0;

	/* supply any missing intermediate page table levels */
	for(l = Toplvl; l >= 0; l--){
		ptp = mmuptpget(va, l, &lvlgot);
		if (ptp == nil)
			panic("mmuput va %#p pa %#p: ptp %#p level %d",
				va, pa, ptp, l);
		if (l != lvlgot)
			print("mmuput wanted lvl %d for %#p but got lvl %d\n",
				l, va, lvlgot);
		x = PTLX(va, l);
		pte = &ptp[x];
		prev = allocpt(l, x, pte, prev);
	}

	if (pte == 0)
		panic("mmuput: zero pte address for va %#p pa %#p", ova, opa);
	/* overwrite pte written in last iteration of above loop */
	if (oldpte == 0)
		oldpte = *pte;

	/*
	 * ordinary user-mode leaf: PteNonleaf0 allowed.  Pteleafvalid includes
	 * PteA|PteD to avoid faults on first use on hardware.
	 * On non-leaf PTEs, UAD are reserved and should be zero.
	 */
	ptebits = PTEPPN(pa) | attrs | PteU | PteR | Pteleafvalid;
	if (Nowx && attrs & PteW)
		ptebits &= ~PteX;
	else
		ptebits |= PteX;
	mmuwrpte(pte, ptebits);

	if(printmmuput)
		prpte(va, pa, pte, ptebits, oldpte);
	if(ptebits == oldpte) {
		iprint("mmuput va %#p pa %#p: pte %#p (pa %#p) -> &pte %#p "
			"(was %#p)\n", va, pa, ptebits,
			(uintptr)PADDRINPTE(ptebits), pte, oldpte);
		panic("mmuput: rewriting pte @ %#p w same contents %#p; "
			"fixfault stuck", pte, ptebits);
	}
	splx(pl);

	if (PAGEPRESENT(oldpte))
		invlpg(va);
}

typedef struct {
	char	name;
	PTE	bit;
} Bitname;
Bitname bitnames[] = {
	'r',	PteR,
	'w',	PteW,
	'x',	PteX,
//	'v',	PteP,
	'd',	PteD,
	'a',	PteA,
	'u',	PteU,
	'g',	PteG,
	/* extensions */
	'c',	PteNc,			/* non-cacheable */
	'i',	PteIo,			/* strongly-ordered for i/o */
	'n',	PteN,			/* Svnapot */
};

void
indent(int depth)
{
	while (depth-- > 0)
		print("\t");
}

void
mmudumplvl(uintptr ptroot, uintptr base, int lvl)
{
	int lastcont;
	vlong i;
	uintptr va;
	Bitname *bp;
	PTE ent, lastpte;
	PTE *pte;

	if (lvl < 0)
		return;
	pte = (PTE *)ptroot;
	indent(Toplvl - lvl);
	print("page table level %d at %#p:", lvl, pte);
	lastpte = -1;
	lastcont = 0;
	for (i = 0; i < PGSZ/sizeof(PTE); i++, lastpte = ent) {
		ent = pte[i];
		if (PTEATTRBITS(ent) == PTEATTRBITS(lastpte) &&
		    PADDRINPTE(ent) > PADDRINPTE(lastpte)) {
			/* don't print continuations */
			if (!lastcont) {
				print(" ...");
				lastcont = 1;
			}
			continue;
		}
		lastcont = 0;
		if (!PAGEPRESENT(ent))
			continue;

		print("\n");

		indent(Toplvl - lvl);
		va = base + i*PGLSZ(lvl);
		print("pte @ va %#10p idx %3d for va %#10p -> pa %#10p ",
			&pte[i], (int)i, va, (uintptr)PADDRINPTE(ent));
		for (bp = bitnames; bp < bitnames + nelem(bitnames); bp++)
			print("%c", ent & bp->bit? bp->name: '-');
		print(" ");
		if (ent & PteLeaf)
			print("%spage leaf", lvl > 0? "super-": "");
		else {
			print("next level ptr:\n");
			mmudumplvl((uintptr)KADDR(PADDRINPTE(ent)), va, lvl-1);

			indent(Toplvl - lvl);
			print("----");
			lastcont = 0;
		}
		if (PADDRINPTE(ent) & m->pgszmask[lvl] != 0)
			print(", not aligned on (super) page boundary");
	}
	print("\n");
}

/* print cpu's page table given va of root page */
void
mmudump(uintptr ptroot)
{
	mmudumplvl(ptroot, 0, Toplvl);
	print("\n");
}


/*
 * pdmap and support functions
 */

/*
 * allocate a page when malloc may not be available yet.
 * ensure that the resulting page is zeroed (freepage by zerosyssome).
 */
PTE *
earlypagealloc(void)
{
	uintptr pa;
	void *alloc;
	Sys *lowsys;

	lowsys = (Sys *)PADDR(sys);		/* make safe for low.c use */
	if (lowsys->freepage == nil)
		lowsys->freepage = lowsys->pts;
	if (lowsys->freepage >= &lowsys->pts[sizeof lowsys->pts]) {
		pa = asmwalkalloc(PGSZ);
		return KADDR(pa);
	}
	alloc = lowsys->freepage;
	lowsys->freepage += Ptpgptes;
	return alloc;
}

/*
 * Need a PTSZ physical allocator here.
 * Because space will never be given back
 * (see vunmap below), just malloc it.
 *	alloc = pmalloc(PTSZ)|PteRW|PteP;
 */
PTE *
mmunewpage(PTE *pde)
{
	void *alloc;

	if (mallocok)
		alloc = mallocalign(PTSZ, PTSZ, 0, 0);	/* replace this */
	else
		alloc = earlypagealloc();
	if(alloc == nil)
		panic("pdmap: page alloc failed");
	/* both of the above allocators zero the page returned. */
	/* no leaf bits: next level ptr to new l0 page (alloc) */
	mmuwrpte(pde, PTEPPN(PADDR(alloc)) | va2gbit((uintptr)alloc) | PteP);
	return alloc;
}

/* only called from pdmap and its subordinates, so it's okay to call mmunewpage. */
static PTE *
nextpt(PTE *ptva, uintptr va, int lvl)
{
	PTE *ptep;

	ptep = PTEADDR(ptva, va, lvl);
	return *ptep == 0? mmunewpage(ptep): KADDR(PADDRINPTE(*ptep));
}

/* only called from pdmap, so it's okay to call mmunewpage. */
static PTE *
mmuensurel1table(uintptr va)
{
	int lvl, lvlgot;
	PTE *pd, *nxtpt;

	pd = mmuptpget(va, 1, &lvlgot);		/* level 1: super pages */
	if (pd == nil && Npglvls > 2) {
		if (Npglvls > NPGSZ)
			panic("mmuensurel1table: too many page table levels");
		if (Npglvls >= 4) {
			/*
			 * from root down, populate empty PTEs in our path.
			 */
			nxtpt = nextpt((PTE *)m->ptroot->va, va, Npglvls - 2);
			for (lvl = Npglvls - 3; lvl > 0; lvl--)
				nxtpt = nextpt(nxtpt, va, lvl);
		} else
			nxtpt = mmunewpage(PTEADDR(m->ptroot->va, va, Toplvl));
		USED(nxtpt);
		pd = mmuptpget(va, 1, &lvlgot); /* level 1: super pages */
	}
	if (pd == nil)
		panic("pdmap: no level 1 page table for va (%#p)", (uintptr)va);
	if (lvlgot != 1)
		print("pdmap wanted lvl 1 for %#p but got lvl %d\n",
			va, lvlgot);
	return pd;
}

/* call with *pde == 0 */
/* only called from pdmap, so it's okay to call mmunewpage. */
static void
mmupopl1pte(PTE *pde, uintptr va, uintptr pa, PTE attr)
{
	if(*pde == 0) {			/* l1 slot unpopulated? */
		mmunewpage(pde);
		/* super page leaf */
		mmuwrpte(pde, PTEPPN(pa)|attr|va2gbit(va)|Pteleafvalid);
		return;
	}

	/* shouldn't happen, diagnose it at some length */
	print("pdmap: l1 pte @ %#p: *pde %#p now, want leaf %#p\n",
		pde, *pde, PTEPPN(pa)|attr|va2gbit(va)|Pteleafvalid);
	print("pte pa %#p new pa %#p\n", (uintptr)PADDRINPTE(*pde), pa);
	if ((*pde & PteLeaf) == 0)
		print("old pte was ptr, not leaf\n");
	panic("pdmap: *pde %#p != 0 for va %#p lvl 1 -> pa %#p", *pde, va, pa);
}

/* only called from pdmap, so it's okay to call mmunewpage. */
static void
mmuallocl1pt(PTE *pde, uintptr va, uintptr pa)
{
	PTE *nxtpt;

	if(*pde != 0)			/* l1 slot populated? */
		nxtpt = KADDR(PADDRINPTE(*pde));
	else {
		nxtpt = mmunewpage(pde);
		if(*pde == 0)
			panic("pdmap: *pde %#p == 0 for va %#p lvl 0 -> pa %#p",
				*pde, va, pa);
	}
	USED(nxtpt);
}

/* only called from pdmap. */
static void
mmupopl0pte(uintptr va, uintptr pa, PTE attr)
{
	int lvlgot;
	PTE *pt;

	pt = mmuptpget(va, 0, &lvlgot);		/* level 0: leaf */
	if (pt == nil)
		panic("pdmap: no level 0 pt for %#p", (uintptr)va);
	if (lvlgot != 0)
		panic("pdmap: wanted lvl 0 pt for %#p, got %#p lvl %d",
			(uintptr)va, pt, lvlgot);
	/* normal leaf */
	mmuwrpte(&pt[PTX(va)], PTEPPN(pa)|attr|va2gbit(va)|Pteleafvalid);
}

/*
 * Add kernel mappings for va -> pa for a section of size bytes.
 * Called only after the va range is known to be unoccupied.
 * Only level 0 and 1 PTEs (4K and a few megabytes) will be allocated.
 * May allocate page table pages.
 */
static int
pdmap(uintmem pa, PTE attr, uintptr va, uintptr size)
{
	uintmem pae, pgsz;
	PTE *pd, *pde;

//	DBG("pdmap: for va %#p -> pa %#p size %llud\n", va, pa, (uvlong)size);
	pd = mmuensurel1table(va);
	if (0 && attr == 0)		/* paranoia, but not needed */
		attr = PteR|PteW;
	for(pae = pa + size; pa < pae; pa += pgsz, va += pgsz){
		/*
		 * Check if it can be mapped using a big page,
		 * i.e. is big enough and starts on a suitable boundary.
		 * Assume processor can do it.
		 */
		pde = &pd[PDX(va)];
		if(ALIGNED(pa, PGLSZ(1)) && ALIGNED(va, PGLSZ(1)) &&
		    pae - pa >= PGLSZ(1)){
			pgsz = PGLSZ(1);
			mmupopl1pte(pde, va, pa, attr);
		} else {
			mmuallocl1pt(pde, va, pa);
			pgsz = PGLSZ(0);
			mmupopl0pte(va, pa, attr);
		}
	}
	return 0;
}


/* find `count' consecutive empty PTEs in a, there are `n' PTEs to examine */
static int
findhole(PTE* a, int n, int count)
{
	int have, i;

	have = 0;
	assert((uint)n <= Ptpgptes);
	for(i = 0; i < n; i++){
		if(a[i] == 0)
			have++;
		else
			have = 0;
		if(have >= count)
			return i+1 - have;
	}
	return -1;
}

enum {
	Pdsz = VMAPSZ/PGLSZ(1),
	Ptsz = PGLSZ(0)/sizeof(PTE),
};

/*
 * Look for free page-table space of size, page aligned, in the vmap.
 * return base address of space found (or nil on error).
 */
static uintptr
vmapalloc(uintptr size)
{
	int lvlgot;
	long o;
	vlong i, n;
	PTE pte;
	PTE *pd, *pt, *dummypte;

	mmuwalk(VMAP, 0, &dummypte, asmwalkalloc);	/* flesh out tree */
	pd = mmuptpget(VMAP, 1, &lvlgot);		/* level 1: 2MB */
	if (pd == nil)
		panic("vmapalloc: no level 1 page table for VMAP (%#p)",
			(uintptr)VMAP);
	if (lvlgot != 1)
		print("vmapalloc wanted lvl 1 for %#p but got lvl %d\n",
			(uintptr)VMAP, lvlgot);
	pd += PDX(VMAP);
	/* pd is now the last part of a level 1 table, for VMAP */

	/*
	 * Look directly in the PD entries if the size is
	 * larger than the range mapped by a single entry.
	 */
	if(size >= PGLSZ(1)){
		n = HOWMANY(size, PGLSZ(1));
		if((o = findhole(pd, Pdsz, n)) != -1)
			return VMAP + o*PGLSZ(1);
		return 0;
	}

	/*
	 * Size is smaller than that mapped by a single PD entry.
	 * Look for an already mapped PT page that has room.
	 */
	n = HOWMANY(size, PGLSZ(0));
	for(i = 0; i < Pdsz; i++){
		pte = pd[i];
		if(PAGEPRESENT(pte) && !(pte & PteLeaf)) {
			pt = (PTE *)KADDR(PADDRINPTE(pte));
			if((o = findhole(pt, Ptsz, n)) != -1)
				return VMAP + i*PGLSZ(1) + o*PGLSZ(0);
		}
	}

	/*
	 * Nothing suitable, start using a new PD entry.
	 */
	if((o = findhole(pd, Pdsz, 1)) != -1)
		return VMAP + o*PGLSZ(1);
	return 0;
}

/*
 * return uncached mapping in kernel space for pa of size.
 *
 * not usually needed on rv64 due to normally-coherent 64-bit address space:
 * upper addresses can be mapped 1:1 to lower (physical) addresses, as long as
 * physical addresses are under 2³⁸: could just add KZERO to phys addr, as
 * devices may be below RAM.
 *
 * systems with physical addresses above 2³⁸ but Sv39 virtual, such as SG2042
 * will need virtual mappings in VMAP (and, in that case, ignore VMAPSZ at
 * DRAM top of each CHIP, 0 and 1).
 */
void*
vmap(uintmem pa, uintptr size)
{
	uintptr va, o, sz, paend;

	DBG("vmap(%#P, %llud)\n", pa, size);

	if(m->machno != 0)
		panic("vmap");

	/*
	 * This is incomplete; the checks are not comprehensive enough.
	 *
	 * To do this properly will require keeping track of the
	 * mappings; perhaps something like kmap, but kmap probably
	 * can't be used early enough for some of the uses.
	 */

	/*
	 * Might be asking for less than a page.
	 * This should have a smaller granularity if
	 * the page size is large.
	 */
	o = pa & ((1<<PGSHFT)-1);
	pa -= o;
	sz = PGROUND(size+o);

	if(pa == 0){
		DBG("vmap(0, %llud) pc=%#p\n", (uvlong)size, getcallerpc(&pa));
		return nil;
	}

	/* already in kernel space? */
	paend = pa + sz - 1;
	if (iskern(pa) && iskern(paend))
		va = pa;		/* KADDR(pa) will be wrong */
	/* addresses below PHYSMEM are addressible as pa+KZERO */
	// else if (isphys(pa) && isphys(paend))
	else if (PHYSMEM > 0 && pa < PHYSMEM && paend < PHYSMEM)
		va = pa + KZERO;
	else {
		ilock(&vmaplock);
		/* we trust that caching has been suitably arranged by hw. */
		if((va = vmapalloc(sz)) == 0 ||
		    pdmap(pa, PteR|PteW, va, sz) < 0){
			iunlock(&vmaplock);
			DBG("vmap(%#P, %llud) failed\n", pa+o, (uvlong)size);
			return nil;
		}
		iunlock(&vmaplock);
	}
	DBG("vmap(%#P, %llud) => va %#P\n", pa+o, (uvlong)size, va+o);
	return (void *)(va + o);
}

void
vunmap(void *v, uintptr size)
{
	DBG("vunmap(%#p, %llud)\n", v, (uvlong)size);

	if(m->machno != 0)
		panic("vunmap");

	/*
	 * See the comments above in vmap.
	 */

	/*
	 * Here will have to deal with releasing any
	 * resources used for the allocation (e.g. page table
	 * pages).
	 */
	DBG("vunmap(%#p, %llud)\n", v, (uvlong)size);
	USED(v);
}

/*
 * walk page tables for va from root to level.
 * return final PTE* via ret.
 * allocate new page table pages as needed.
 */
int
mmuwalk(uintptr va, int level, PTE** ret, uintptr (*alloc)(uintptr))
{
	int l, lvlgot;
	Mpl pl;
	uintptr pa;
	PTE *pte, *ptp;

//	DBG("mmuwalk cpu%d: va %#p level %d\n", m->machno, va, level);
	pte = 0;
	pl = splhi();
	for(l = Toplvl; l >= 0; l--){
		ptp = mmuptpget(va, l, &lvlgot);
		if (ptp == nil)
			panic("mmuwalk: no page table for %#p at lvl %d", va, l);
		if (lvlgot != l)
			print("mmuwalk wanted lvl %d for %#p but got lvl %d\n",
				l, va, lvlgot);
		pte = &ptp[PTLX(va, l)];
		if(l == level)
			break;

		if(!PAGEPRESENT(*pte)){
			if(alloc == nil)	/* normal case: no allocator */
				break;

			/* allocate a zeroed page for next level pt */
			pa = alloc(PTSZ);
			if(pa == ~(uintptr)0) {
				splx(pl);
				/* haven't seen this message yet */
				print("mmuwalk: can't allocate a pt page\n");
				return -1;
			}
			if(pa & (PTSZ-1))
				print("mmuwalk pa %#p unaligned\n", pa);

			/* map new next-level page: no leaf bits (PteNonleaf0) */
//			DBG("writing pte %#p with %#p\n", pte, PTEPPN(pa)|PteP);
			mmuwrpte(pte, PTEPPN(pa) | PteP);

			if((ptp = mmuptpget(va, l-1, &lvlgot)) == nil)
				panic("mmuwalk: mmuptpget(%#p, %d)", va, l-1);
			if (lvlgot != l-1)
				print("mmuwalk wanted lvl %d for %#p but got "
					"lvl %d\n", l-1, va, lvlgot);
			/*
			 * Needed?  k10 does it.  mmuptpalloc allocates with
			 * malloc, which zeroes, but perhaps we're reusing
			 * an old page instead.
			 */
			memset(ptp, 0, PTSZ);
			wbinvd();
		}
		else if (l > 0 && *pte & PteLeaf)	/* super page leaf? */
			break;
	}
	*ret = pte;
	splx(pl);
	return l;
}

#ifdef unused
int
mmuwalknewpte(uintptr va, uintmem mem, int lvl, PTE attrs)
{
	int l;
	PTE ptebits;
	PTE *pte;

	/* NB: already covered by identity map on pf */
	pte = nil;
	if((l = mmuwalk(va, lvl, &pte, asmwalkalloc)) < 0)
		panic("mmuwalknewpte: mmuwalk failed for asmsizes");
	if (PAGEPRESENT(*pte)) {
		DBG("mmuwalknewpte: pte @ %#p maps %#p -> %#p\n",
			pte, va, (uintptr)PADDRINPTE(*pte));
		return l;
	}

	/* *pte is unpopulated, so set it */
	ptebits = PTEPPN(mem) | va2gbit(va) | attrs | Pteleafvalid;
	DBG("writing pte %#p with %#p for %#p -> %#p\n", pte, ptebits, va, mem);
	mmuwrpte(pte, ptebits);
	return l;
}
#endif

/*
 * Given a VA, find the PA.
 * This is probably not the right interface,
 * but will do as an experiment. Usual
 * question, should va be void* or uintptr?
 */
uintptr
mmuphysaddr(uintptr va)
{
	int l;
	PTE *pte;
	uintptr mask, pa;

	pte = 0;
	l = mmuwalk(va, 0, &pte, nil);
//	DBG("mmuphysaddr: va %#p lvl %d pte %#p\n", va, l, pte);
	if(l < 0) {
		DBG("mmuphysaddr: va %#p mapping failed\n", va);
		return ~0ull;
	}
	if (pte == nil) {
		DBG("mmuphysaddr: va %#p: nil pte address; got lvl %d\n",
			va, l);
		return ~0ull;
	}
	if (!PAGEPRESENT(*pte)) {
		DBG("mmuphysaddr: va %#p: pte @ %#p invalid: %#p; got lvl %d\n",
			va, pte, *pte, l);
		return ~0ull;
	}
	mask = VMASK(PGLSHFT(l));
	pa = (PADDRINPTE(*pte) & ~mask) + (va & mask);
//	DBG("mmuphysaddr: mask %#p pte %#p *pte %#p va %#p\n", mask, pte, *pte, va);

//	DBG("mmuphysaddr: lvl %d va %#p pa %#p\n", l, va, pa);
	return pa;
}

void *
evmap(uintptr pa, uintptr size)
{
	uintptr va;

	va = (uintptr)vmap(pa, size);
	if (va == ~0ull)
		va = pa;			/* and pray */
	/*
	 * don't probe here.  pci, for example, may have to be taken out of
	 * reset first.
	 */
	if (0 && probeulong((ulong *)va, Read) < 0)
		iprint("evmap: va %#p (for pa %#p) faulted on read\n", va, pa);
	return (void *)va;
}

void
mmuinitap(void)
{
	Page *ptroot;

	DBG("mach%d: %#p npgsz %d\n", m->machno, m, Npglvls);

	/*
	 * newcpupages previously created a top-level page table for us
	 * and pointed our mach->ptroot at it.  we just need to use it.
	 */
	ptroot = m->ptroot;
//	DBG("SATP %#p\n", pagingmode | (ptroot->pa / PGSZ));
/**/	putsatp(pagingmode | (ptroot->pa / PGSZ));

	DBG("mach cpu%d: m %#p; pt root va %#p -> pa %#p\n", m->machno, m,
		ptroot->va, ptroot->pa);
	/* gohigh already called by low() */
	usevirtdevaddrs();		/* device vmap mappings in effect */
	mmuflushtlb(m->ptroot);		/* zero user mappings */
}

/*
 * Set up the various kernel memory allocator limits:
 * pmstart is the lowest unused physical memory in bank 0;
 * pmend is the highest ram address + 1 (there can be gaps after pmstart);
 * vmstart/vmend bound the total possible VM used by the kernel;
 * vmunused is the highest virtual address currently mapped and used by
 * the kernel;
 * vmunmapped is the highest virtual address currently mapped by the kernel.
 * Vmunused can be bumped up to vmunmapped before more physical memory needs
 * to be allocated and mapped.  So there can't be gaps in that address range.
 *
 * qmalloc uses the vm* values.
 *
 * These are set up here so meminit can map appropriately.
 */
static void
allocinit(void)
{
	uintptr st, pa, sz, ksize, bankend;

	st = sys->pmstart;
	ksize = st - sys->pmbase;
	DBG("allocinit: pmstart %#p pmbase %#p\n", sys->pmstart, sys->pmbase);
	sz = ROUNDUP(st, PGLSZ(1)) - st; /* kernel end to next super page gap */
	pa = asmalloc(0, sz, AsmMEMORY, 0);
	if(pa == 0)
		panic("mmuinit: asmalloc for gap %lld after kernel failed", sz);
	if(pa != st)				/* sanity check */
		panic("mmuinit: alloc after kernel %#p != pmstart %#p", pa, st);
	sys->pmstart += sz;

	/* set up vm* variables for qmalloc */
	sys->vmstart = KZERO + sys->pmbase;	/* needed for beagle at least */
	/* set vmunused to the page after the kernel */
	sys->vmunused = sys->vmstart + PGROUND(ksize);
	sys->vmunmapped = sys->vmstart + ksize + sz;	/* next super page */
	/*
	 * end of kernel data.  rounded up for meminit on vbox in k10, else vbox
	 * may blow an assertion.
	 */
	sys->vmend = ROUNDUP(sys->vmstart + kernmem, PGLSZ(1));
	/* avoid possible discontiguity */
	bankend = KZERO + membanks[0].addr + membanks[0].size;
	if (sys->vmend > bankend)
		sys->vmend = bankend;
	DBG("mmuinit: KZERO: vmstart %#p vmunused %#p\n"
		"\tvmunmapped %#p vmend %#p\n",
		sys->vmstart, sys->vmunused, sys->vmunmapped, sys->vmend);
}

void
mmuinit(void)
{
	int l, lvl;
	PTE *pte;
	Page *page;

	pageidmap(); /* replicate init mappings w permanent pt, mach0ptrootpg */
	DBG("mach%d: %#p npgsz %d\n", m->machno, m, Npglvls);

	/* we are cpu0; already called archmmu in pageidmap. */
	/* m->ptroot has already been set in pageidmap. */
	page = m->ptroot;

	allocinit();
	DBG("mach cpu%d: m %#p; pt root va %#p -> pa %#p\n", m->machno, m,
		page->va, page->pa);
	USED(page);

	pte = nil;
	for (lvl = Toplvl; lvl >= 0; lvl--)  /* from root to leaves */
		if((l = mmuwalk(KZERO, lvl, &pte, nil)) >= 0) {
			DBG("KZERO lvl %d -> l %d %#p %#p\n", lvl, l, pte,
				(pte? *pte: 0));
			USED(l);
		}
	mmuphysaddr(PTR2UINT(end));

	iovmapsoc();
}
