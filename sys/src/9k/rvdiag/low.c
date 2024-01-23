/*
 * initialise a risc-v RV64G system enough to call main with the low identity
 * map and upper addresses mapped to lower, in supervisor mode.
 * this is traditionally done entirely in assembler, typically called l.s.
 *
 * do not use print nor iprint with '%' in the format here; libc/fmt learns
 * function addresses for verbs, which need to be high addresses.
 *
 * all cpus may be executing this code simultaneously.
 */
#include "u.h"
#include "../port/lib.h"
#include "mem.h"
#include "dat.h"
#include "fns.h"

#include "riscv.h"

#define dbprf if (!Lowdebug) {} else prf

enum {
	Diagnose = 0,		/* self-checking for a new machine */
	Lowdebug = 1,		/* beware: may throw off MP start-up timing */

	Pmpdebug = 0,
	Configpmp = 0,		/* don't need this so far */
	Maxpmpaddrs = 8,	/* ≤8 currently, some systems have more */

	Ptedebug = 0,		/* flag: print initial PTEs */
	Nptes = 12,		/* number of Ptes to dump */
	// Nptes = Ptpgptes / 2,

	Pageexcs = 1<<Instpage | 1<<Loadpage | 1<<Storepage,
	Ptesperpage = Ptpgptes,
};

/* init with anything to force into data seg. to avoid bss zeroing */
int	asids = 0;
int	early = 1;			/* tested in trap.c */
ushort	hartids[MACHMAX] = { 0 };
Rvarch *initarchp;
uintptr	mainpc = 0;
void	*origmtvec;
uintptr satptval, satpepc;
void	(*tlbinvall)(void);

static void mkinitpgtbl(Sys *lowsys);

/*
 * machine mode setup and switch to supervisor mode.
 */

static void
dumppmp(char *desc)
{
	int slot;
	uvlong cfg;

	USED(desc);
	if (!Pmpdebug)
		return;
	/* c910 needs us to write PMPCFG before we can read actual contents. */
	csrset(PMPCFG+0, 0);
	cfg = csrrd(PMPCFG+0);
	dbprf("\n%s: pmpcfg %#p\n", desc, cfg);
	for (slot = 0; slot < Maxpmpaddrs; slot++)
		dbprf("pmpaddr%d %#p\n", slot, csrrd(PMPADDR+slot));
	USED(cfg);
}

/*
 * pmp is optional, and somewhat redundant on systems with MMUs.
 * make sure that the whole of ram is rwx, where possible.
 * pmp csrs are only accessible in machine mode, and
 * pmp grants access only to accesses in S and U modes,
 * but pmp can restrict accesses in M mode.
 * it's likely to be configured correctly already since u-boot ran.
 */
static void
pmpinit(void)
{
	int slot;
	uvlong cfg, pmptype;

	dbprf("pmpinit...");
	m->probebad = 0;
	/* c910 needs us to write PMPCFG before we can read actual contents. */
	csrset(PMPCFG+0, 0);
	cfg = csrrd(PMPCFG+0);		/* see if it faults */
	if (m->probebad) {
		dbprf("no PMP...");
		return;
	}

	dumppmp("initial");
	USED(cfg);
	if (!Configpmp)
		return;

#ifdef BLIND_PMP_SETTING			/* TODO cheap hack */
	pmptype = Pmpnapot;
	slot = 0;
	csrswap(PMPADDR+0, ~0ULL);
#else
	pmptype = Pmptor;
	for (slot = 0; slot < Maxpmpaddrs; slot++)
		if ((cfg & (VMASK(8)<<(slot*8))) == 0)
			break;
	if (slot >= Maxpmpaddrs) {
		dbprf("no free pmpcfg slot in first %d of %#p\n",
			Maxpmpaddrs, cfg);
		return;
	}

	/* hope next slot is free too, leaving this one zero as base */
	if (slot > 0 && slot < Maxpmpaddrs-1)
		slot++;
	dbprf("free pmpcfg slot %d; allowing all\n", slot);
	/* 56 bits max. phys., low 2 implied */
	csrswap(PMPADDR+slot, VMASK(56-2));
#endif
	wbinvd();
	/*
	 * if we add Pmplock, this setting will apply even to M mode,
	 * but can't be changed until reset or power cycle.
	 */
	csrset(PMPCFG+0, (pmptype|Pmpr|Pmpw|Pmpx) << (slot*8));
	wbinvd();		/* sfence.vma is required since priv 1.12 */
	dumppmp("configured");
}

/*
 * delegates M-mode exceptions and interrupts to S-mode, when possible,
 * and allow access to cycle counters.  also configures PMP if present.
 */
static void
delegate(void)
{
	dbprf("deleg...");
	/* expose timers & cycle counters to super */
	csrswap(MCOUNTEREN, ~0ull);
	csrswap(SCOUNTEREN, ~0ull);

	/* set up normal delegations */
//	csrswap(MIDELEG, Superie);	/* old: punt S intrs to super */
	csrswap(MIDELEG, ~0);		/* try to punt S+M intrs to super */
	mideleg = csrrd(MIDELEG);

	/* catch early stray M faults */
	dbprf("mtraps abort...");
	putmtvec(recmtrapalign);		/* low due to low PC */

	/* don't punt any except.s so we can detect PMP presence */
	csrswap(MEDELEG, 0);
	pmpinit();

	/* punt except.s to super except env call from super */
	csrswap(MEDELEG, ~(1LL<<Envcallsup));
	if (TODO && soc.c910)
		csrclr(MEDELEG, Pageexcs);  /* see if it page faults to M */
	medeleg = csrrd(MEDELEG);
	dbprf("mideleg %#p medeleg %#p\n", mideleg, medeleg); /* see what stuck */
	if ((medeleg & Pageexcs) != Pageexcs)
		prf("page fault exceptions not delegated by M mode!\n");
}

#define EXT(let) (1LL << ((let) - 'A'))

static void
loadids(Sys *lowsys)
{
	lowsys->extensions = csrrd(MISA);
	lowsys->archid =   csrrd(MARCHID);
	lowsys->vendorid = csrrd(MVENDORID);
	dbprf("misa %#p marchid %#p mvendorid %#lux...",
		lowsys->extensions, lowsys->archid, lowsys->vendorid);

	if (lowsys->vendorid == Vthead)
		dbprf("xuantie cpu...");
	if (lowsys->extensions & EXT('H')) {
		dbprf("hypervisor enabled...");
		csrswap(MISA, lowsys->extensions & ~EXT('H'));
		dbprf("now disabled...");
	}
}

static int
readwd(ulong *ptr, ulong exp)
{
	ulong val;

	val = readmprv(ptr);
	if (val == exp) {
		dbprf("\nread %#p: got expected %#lux as Super...", ptr, val);
		return 0;
	} else {
		dbprf("\nread %#p: %#lux, expected %#lux as Super...",
			ptr, val, exp);
		return -1;
	}
}

static int
writewd(ulong *ptr, ulong val)
{
	writelmprv(ptr, val);
	return readwd(ptr, val);
}

static void
writelong(ulong *ptr, ulong val)
{
	dbprf(" writing %#p with %#lux as Super: ", ptr, val);
	delay(100);
	writelmprv(ptr, val);
	delay(100);
}

static void
satpprobe(Sys *lowsys)			/* only safe in machine mode */
{
	long r;
	uvlong mode, bestmode, readmode;

	dbprf("\n");
	mkinitpgtbl(lowsys);

	/* probe satp modes */
	dbprf("probing satp modes...");
	bestmode = Sv39;
	asids = 0;
	for (mode = Sv39; mode <= Sv64; mode += 1LL<<Svshft) {
		putsatp(mode | Satpasidmask);
		readmode = getsatp();
		if ((readmode & ~Satpasidmask) == mode)
			bestmode = readmode;
	}
	asids = ((bestmode & Satpasidmask) >> Satpasidshft) + 1;
	dbprf("best available satp mode is %#llud with %d asids...",
		bestmode>>Svshft, asids);
	USED(bestmode);

	/* set up mmu, verify operation */
	dbprf("\nprobe initial page table (satp %#p)...", lowsys->satp);
	putsatp(lowsys->satp);
	if (getsatp() != lowsys->satp) {
		dbprf("can't set satp to %#p!\n", lowsys->satp);
		return;
	}
	dbprf("installed...");

	/*
	 * Mpp is Mppsuper, inherited from start.s.
	 */
	dbprf("\ntest mprv...");
	wbinvd();
	/* _main is low here, because PC is low. */
	/* load as if in S mode */
	r =  readwd((ulong *)_main,    0x100f0ff0000fLL);
	r |= readwd(ensurehigh(_main), 0x100f0ff0000fLL);

	/*
	 * WTF? uart writes do nothing on c910.  is i/o broken in
	 * S mode?  works if we omit beginxt.c, mchxt.s &
	 * -DC910.  maybe we have to enable a zillion clock signals
	 * and take a zillion SoC components out of reset?
	 */
	writelong((ulong *)PAUart0, '*');
	writelong(ensurehigh(PAUart0), '~');

	/* writes via high addresses to normal memory work */
	static ulong testwd = -1;
	writewd(ensurehigh(&testwd), 0x1234);

	dbprf("\npage table seems %s\n", r == 0? "okay": "bad");
	USED(r);
	lowsys->satp = 0;
	putsatp(0);
}

static void
assertrv(char *claim)
{
	prf("** If cpu is actually risc-v, %s.\n", claim);
}

static void
macharchinit(void)
{
	if (initarchp->machexten) {
		dbprf("\nenable M mode extensions...");
		(*(Pvfnv)ensurelow(initarchp->machexten))();
	}
	/* start other rvb-ice harts while we can */
	if (m->machno == 0 && initarchp->machstharts && !soc.c910) { // TODO
		dbprf("start harts via M mode extensions...");
		(*(Pvfnv)ensurelow(initarchp->machstharts))();
	}
}

/*
 * do machine-mode set up, then leave machine mode.
 * set up for supervisor mode, needed for paging in kernel.
 * mostly punting faults to supervisor mode, preparing for clock
 * interrupts, and disabling paging.  mret will switch us to supervisor mode.
 *
 * needs m set, for CSR(MSCRATCH).
 */
static void
mach_to_super(Sys *lowsys)
{
	dbprf(" * machine mode.\n");
	delegate();
	/* see setstkmach0 for # of priv modes (lowsys->nprivmodes) */
	/* little endian, prev mode = user, allow super interrupts */
	putmsts(Defmsts & ~(Mie|Upie|Uie|Tvm|Tw|Tsr));
	if (getmsts() & Uie)
		prf("user interrupts enabled in M; can't be disabled\n");
	dbprf("mstatus %#p...", getmsts());
	loadids(lowsys);

	if (initarchp)
		macharchinit();

	/* set up machine traps on this cpu.  Mach is soon to be populated. */
	dbprf("\nmtraps...");
	csrswap(MSCRATCH, (uintptr)m);	/* mmu will be off; low m & pc */
	/* maybe sbi is present, despite starting us in M mode? */
	/* mtvec may have been from sbi, u-boot or rebootcode */
	putmtvec(mtrap);
	dbprf("original mtvec %#p -> %#p\n", origmtvec, getmtvec());
	putstvec(rectrapalign);
	if (Diagnose && m->machno == 0)
		satpprobe(lowsys); /* find best mode & validate page table */

	/* switch to super mode & enable mach intrs. */
	splhi();
	putmie(getmie() | Machie);	/* needed for clock intrs */
	putmsts(getmsts() | Mie);  /* Mpp is still Mppsuper, from start.s */
	dbprf("to super: mstatus %#p sstatus %#p mret\n", getmsts(), getsts());
	assertrv("switching to supervisor mode");
	mret();	/* change privilege mode to MSTATUS.Mpp's mode (super) */
}

/*
 * the rest is supervisor mode setup.
 */

/* lvl is the level of the page table we are writing */
static void
setptes(PTE *ptep, PTE ptebits, int n, int lvl)
{
	PTE pteincr;

	pteincr = (PGLSZ(lvl)>>PGSHFT) << PTESHFT;
	for (; n > 0; n--) {
		*ptep++ = ptebits;
		ptebits += pteincr;
	}
}

/* create top-level leaf page table entries to phys for phys & KZERO|phys */
static void
dualmap(PTE *ptp, uintptr phys, uint nptes)
{
	PTE ptebits;

	/* force to physical space, round down to nearest superpage */
	phys = ROUNDDN(phys & ~KZERO, PGLSZ(Toplvl));
	ptebits = (phys>>PGSHFT)<<PTESHFT | PteR | PteW | PteX | Pteleafvalid;
	setptes(&ptp[PTLX(phys, Toplvl)], ptebits, nptes, Toplvl);
	setptes(&ptp[PTLX(KZERO+phys, Toplvl)], ptebits | PteG, nptes, Toplvl);
}

static void
dumpptes(PTE *ptp)
{
	int i, st;

	dbprf("initial page table excerpts:\n");
	for (i = 0; i < Nptes; i++)
		if (ptp[i])
			dbprf("ptp[%d] %#p\n", i, ptp[i]);
	st = PTLX(KZERO, Toplvl);
	for (i = st; i < st + Nptes; i++)
		if (ptp[i])
			dbprf("ptp[%d] %#p\n", i, ptp[i]);
}

/*
 * construct minimal initial top-level page table with id & upper->lower maps.
 */
static void
mkinitpgtbl(Sys *lowsys)
{
	uint nptes;
	PTE *ptp;

	if (lowsys->satp & pagingmode)		/* already made? */
		return;
	/*
	 * allocate temporary top-level page table.
	 * in top-level pt for 4 levels, each PTE covers 256GB.
	 * for 3 levels, each covers 1GB.
	 */
	ptp = lowsys->initpt;
	dbprf("creating initial page table at %#p\n", ptp);
	memset(lowsys->initpt, 0, sizeof lowsys->initpt);

	/*
	 * leave last pte of upper and lower ranges free
	 * (for VMAP in upper->lower map at least).
	 */
	nptes = Ptesperpage/2 - 1;		/* for each range */
	USED(nptes);
	if (VMBITS < 39)
		nptes /= 2;	/* upper and lower ranges fit into low addr.s */

	if (TODO && PAGINGMODE == Sv39 && soc.c910) {
		/* alternate strategy: don't map the whole address space */
		dualmap(ptp, KTZERO-KZERO, 8);
		dualmap(ptp, (uintptr)soc.uart, 1);
		dualmap(ptp, (uintptr)soc.clint, 1);
		dualmap(ptp, (uintptr)soc.plic, 1);
	} else
		/* populate id map in lower range & upper->lower, from 0 up. */
		/* works on everything but the c910. */
		dualmap(ptp, 0, nptes);
//	expandpte(lowsys, ptp, PteR|PteW|PteX | Pteleafvalid, PHYSMEM, OFFMEM);
	if (Ptedebug)
		dumpptes(ptp);
	lowsys->satp = pagingmode | ((uintptr)ptp / PGSZ);
}

static void
newmach(Mach *mp, uint cpu)
{
	/* initialise enough of Mach to get to main */
	m = mp;
	m->machmode = bootmachmode;
	fakecpuhz();
	usephysdevaddrs();
	m->machno = cpu;
	m->hartid = hartids[cpu];

	/*
	 * assumes a standard clint.  m->timecmp is mainly for mtrap.s,
	 * use low addr.
	 */
	if (!bootmachmode)
		putsscratch((uintptr)m);	/* ready for straps now */
	if (probeulong((ulong *)m->clint, Read) < 0)
		/* we won't get far without a working clint */
		prf("cpu%d: no response from clint at %#p\n", cpu, m->clint);
	else
		dbprf("cpu%d: clint at %#p\n", cpu, m->clint);
	m->mtimecmp = &m->clint->mtimecmp[m->hartid];

	putsscratch((uintptr)m);		/* ready for straps now */
	if(cpu == 0)
		dbprf("RISC-V64 diagnostic on hart %d\n", m->hartid);
	clrstie();
	if (nosbi)
		wrcltimecmp(VMASK(63));		/* no clock intrs on m */
}

/*
 * sys->machptr[cpu] must be set (thus a Mach allocated)
 * before calling this from low addresses.  on secondary cpus,
 * the MMU may be configured, but we're still in low addresses, and
 * could be in machine mode.
 */
static void
setmach(Sys *lowsys, uint cpu)
{
	Mach *m0;

	m = lowsys->machptr[cpu];	/* m must be a high address */
	if (m == nil)
		panic("setmach: nil sys->machptr[%d] before mallocinit", cpu);
	if (!bootmachmode)
		m = ensurelow(m);
	newmach(m, cpu);
	m0 = ensurelow(lowsys->machptr[0]);
	m->cpuhz  = m0->cpuhz;
	m->cpumhz = m0->cpumhz;
}

static void
alignchk(void)
{
	static int align = 0x123456;

	if (align != 0x123456)
		panic("data segment misaligned");
}

static void
zerosyssome(Sys *lowsys)
{
	char *aftreboot;

	/*
	 * zero Syspercpu, including machstk before use, for usage measurements.
	 */
	memset(&lowsys->Syspercpu, 0, sizeof(Syspercpu));

	/* zero Sys from syspage after Reboot to before kmesg */
	aftreboot = (char *)&lowsys->Reboot + sizeof(Reboot);
	memset(aftreboot, 0, (char *)lowsys->sysextend - aftreboot);
}

/*
 * On cpu0, initialise m & sys, allocate a permanent stack in sys->machstk,
 * return stack top in low addresses.
 */
static uintptr
setstkmach0(Sys *lowsys)
{
	int i;
	ulong ktzerophys;
	uintptr lowktzero;

	prf("\nRISC-V64 diagnostic for %s\n\n", RVARCH);
	lowsys->satp = 0;		/* don't use previous kernel's map */
	lowktzero = PADDR((void *)KTZERO);
	sys = KADDR((uintptr)lowsys);
	if (PPN(mainpc) != lowktzero)
		dbprf("_main pc %#p too far from PADDR(KTZERO) %#p, "
			"kernel loaded at wrong address!\n", mainpc, lowktzero);
	ktzerophys = ROUNDDN((ulong)KTZERO, GB);
	if (PHYSMEM != ktzerophys)
		prf("physmem %#p != base of ktzero %#lux!\n",
			(uintptr)PHYSMEM, ktzerophys);
	alignchk();
	zerosyssome(lowsys);
	/* see loadids, which notes extensions */
	lowsys->nprivmodes = 2;	/* for plic: Machine & Super can take intrs */
	/* if hyper extension present, lowsys->nprivmodes++ */
	/* if user-interrupt extension present, lowsys->nprivmodes++ */
	newmach(&lowsys->mach, 0);	/* sets m to cpu0's Mach */

	if (initarchp) {
		initarchp = ensurelow(initarchp);
		dbprf("for %s cpus...", ensurelow(initarchp->name));
	}

	if (!bootmachmode && !nosbi) {
		/* dies on rvb-ice: no sbi, i guess */
		dbprf("cpu0: sbiget*...");
		lowsys->archid = sbigetmarchid();
		lowsys->vendorid = sbigetmvendorid();
	}

	/* try to detect a watchdog */
	if (TODO && Diagnose && Lowdebug) {
		dbprf("watchdog hanging over our heads? ");
		for (i = 0; i < 20; i++) {
			delay(500);
			dbprf(".");
		}
		dbprf("apparently not.\n");
	}
	return (uintptr)&lowsys->machstk[MACHSTKSZ];
}

/*
 * the c910 doesn't like something about the way that I enable paging,
 * but its diagnostic is obscure (wandering off into the weeds).  other cpus
 * and tinyemu think it's fine for Sv39 and Sv48, and I'm tired of chasing this.
 */
vlong
putsatp(uintptr satp)
{
	vlong ret;
	uintptr oldsatp;
	Mpl s;

	oldsatp = getsatp();
	if (oldsatp == satp)
		return 0;			/* no op */
	s = splhi();
	if (!m->machmode)
		if (oldsatp && satp == 0)
			assertrv("turning off mmu");
		else if (satp) {
			if (TODO && tlbinvall)
				tlbinvall();	// TODO fatal on c910
			if (soc.c910 && asids > 1)
				satp |= 1LL << Satpasidshft;
			assertrv("turning on mmu");
		}
dbprf("_putsatp...");
	ret = _putsatp(satp);
dbprf("done...");
	if (ret != 0)
		panic("cpu (or its configuration) is broken.  setting satp: "
			"exc cause %lld tval %#p epc %#p",
			ret, satptval, satpepc);
	if (Lowdebug && satp != 0) {
		coherence();
		delay(1);
		*(ulong *)PAUart0 = '|';	/* works in s mode */
		coherence();
		delay(1);
		if (!m->machmode) {
			/* fails in s mode */
			*(ulong *)ensurehigh(PAUart0) = '#';
			coherence();
			delay(1);
		}
	}
	splx(s);
	return ret;
}

static void
prfpc(int dummy)
{
	dbprf("pc %#p...", getcallerpc(&dummy));
	USED(dummy);
}

/*
 * start with the shared initial page table in lowsys->satp,
 * which should be lowsys->initpt.
 * enabling paging also enables speculative execution on xuantie.
 *
 * a netbsd porter says setting satp always causes a fault (S or M?);
 * it seems to make only the xuantie cpus lose their minds.
 *
 * this must not cause a page fault since up is still nil
 * (i.e., there is no user context).
 */
static void
pagingon(Sys *lowsys)
{
	uvlong satp;

	dbprf("set stvec for exten bugs %#p...", rectrapalign);
	putstvec(rectrapalign);	/* rectrapalign is low here when PC is low */
	if (initarchp && initarchp->supermppaging) {
		dbprf("cpu%d paging extension...", m->machno);
		(*(Pvfns)ensurelow(initarchp->supermppaging))(lowsys);
	}
	prfpc(0);
	dbprf("set stvec for kernel %#p...", ensurelow(strap));
	putstvec(ensurelow(strap));		/* va; needed? */
	if (m->machno == 0)
		mkinitpgtbl(lowsys);

	assertrv("switch to initial page tbl:");
	dbprf(" writing satp %#p\n", lowsys->satp);
/**/	if (putsatp(lowsys->satp) < 0) {
		dbprf("satp %#p\n", getsatp());
		prfpc(0);
		panic("putsatp faulted on %#p", lowsys->satp);
	}
	dbprf("set stvec for kernel %#p...", ensurehigh(strap));
	putstvec(ensurehigh(strap));		/* va; needed? */
	prfpc(0);
	dbprf("\n");
	satp = getsatp();		/* asid field will be (# asids) - 1 */
	dbprf("read satp back as %#p\n", satp);
	if ((satp & ~Satpasidmask) != lowsys->satp)
		panic("couldn't enable paging with satp %#p", lowsys->satp);
}

/* must be called with high addresses mapped */
static uintptr
prepstk(uintptr stktop)
{
	ulong wd;

	dbprf("high uart @ %#p: ", ensurehigh((void *)soc.uart));
	if (Lowdebug)
		/* dies if we started in S mode */
		*(ulong *)ensurehigh((void *)soc.uart) = '!';

	dbprf("\nlow stack %#p: writing...", stktop);
	*(ulong *)stktop = 0;
	coherence();

	stktop = (uintptr)ensurehigh((void *)stktop);
	dbprf("high stack %#p: reading...", stktop);
	wd = *(ulong *)stktop;
	coherence();

	dbprf("writing...");
	*(ulong *)stktop = 0;		/* 0 was: wd + 1 */
	USED(wd);
	coherence();

	dbprf("ok\n");
	return stktop;
}

/* tell other cpus to wait in secstall() */
static void
secstallset(Sys *lowsys)
{
	static int stallnext = RBFLAGSTALL;

	lowsys->secstall |= stallnext;
	stallnext = 0;				/* harmless race here */
	coherence();
}

void
setsts(void)
{
	/* S mode: little endian, prev mode = user, allow intrs in U mode */
	putsts((getsts() & ~(Ube|Spp|Uxl|Uie|Mpp)) | Spie | Defssts | Mppuser);
}

/*
 * We are called with all interrupts disabled.
 *
 * A temporary stack (in initstks) must be in use when we are called.
 * BSS has already been zeroed.  Don't use any high addresses until we are
 * definitely in supervisor mode with paging on.
 *
 * Main goals are: establish a permanent stack, m, sys, [sm]scratch, [sm]tvec,
 * [sm]status, page table, ensure cpu is in super mode, call main with machno.
 * Other cpus may be doing the same.
 *
 * sys is a shared global ptr, always a high address until shutdown, but our
 * use here will be of its (low) physical address, lowsys.  It's 1MB at the
 * top of the first memory bank.
 */
void
low(uint cpu)
{
	uintptr stktop;
	Sys *lowsys;

	setsts();
	lowsys = (Sys *)(PHYSMEM + membanks[0].size - MB);
	secstallset(lowsys);
	if (cpu >= MACHMAX) {
		prf("low: cpu%d out of range, >= %d; ignoring\n", cpu, MACHMAX);
		wfi();
	}

	/* set m & sys, work out new stack top. */
	stktop = setstkmach0(lowsys);

	/* get out of machine mode if necessary, and start paging. */
	if (bootmachmode)
		mach_to_super(lowsys);	/* enables any M extensions too */
	m->machmode = 0;

	dbprf("\n * supervisor mode.  sbi %s\n", nosbi? "not used": "assumed");
	pagingon(lowsys);

	/*
	 * switch to new, permanent stack for this cpu at upper addresses.
	 * All extant automatic variables are on the old (current) stack, thus
	 * unpredictable after the switch.  The initstk can be reclaimed then.
	 */
	dbprf("new stack...");
	putstvec(rectrapalign);
/**/	setsp(prepstk(stktop - SBIALIGN));

	/*
	 * jump into high (kernel) addresses, thus vacating the lower range
	 * for user processes.  adjust pointers and registers as needed.
	 * gohigh calls jumphigh but may return to low saved return address
	 * on stack.
	 */
	dbprf("jump to kernel space...");
	gohigh();	/* sets up high straps, but returns to low address */
	jumphigh();	/* call asm directly: force regs high before main */
	prfpc(0);
	early = 0;
	exit(0);
}
