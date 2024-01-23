/*
 * mapping twixt kernel-virtual and physical addresses.
 * trivial mapping on riscv64, except very large Sv39 systems;
 * on those systems, the mapping functions won't work on
 * phys addrs ≥ 2³⁸-VMAPSZ.
 */
#include "u.h"
#include "../port/lib.h"
#include "mem.h"
#include "dat.h"
#include "fns.h"

#ifndef ensurehigh
/* make addr a kernel virtual address if not already one.  mainly for low.c. */
void *
ensurehigh(void *addr)
{
	return (void *)((uintptr)addr | KZERO);
}

/* make addr a physical address if not already one.  mainly for low.c. */
void *
ensurelow(void *addr)
{
	return (void *)((uintptr)addr & ~KZERO);
}
#endif

/* convert physical address pa to a kernel virtual pointer */
void*
KADDR(uintptr pa)
{
	if (iskern((void *)pa))
		panic("KADDR: pa %#p already in kernel space, from %#p",
			pa, getcallerpc(&pa));
	return ensurehigh(pa);		/* but is it mapped? */
}

/* convert kernel virtual pointer va to a physical address */
uintmem
PADDR(void* va)
{
	if (isphys(va))
		panic("PADDR: va %#p not in kernel space, from %#p",
			va, getcallerpc(&va));
	return (uintmem)ensurelow(va);
}

/*
 * KMap is used to map individual pages into virtual memory.
 * It is rare to have more than a few KMaps at a time (in the
 * absence of interrupts, only two at a time are ever used,
 * but interrupts can stack).  The mappings are local to a process,
 * so we can use the same range of virtual address space for
 * all processes without any coordination.
 */
/* return a working virtual address for page->pa */
KMap*
kmap(Page* page)
{
	void *va;
	uintptr pa;

	if (page == nil)
		panic("kmap: nil page");
	if (page->pa == 0)
		panic("kmap: nil page->pa");
	va = KADDR(page->pa);
//	print("kmap(%#p) @ %#p: %#p %#p\n",
//		page->pa, getcallerpc(&page), page->pa, va);
	/* apparently va need not be page->va */
	pa = mmuphysaddr((uintptr)va);
	if (pa == ~0ull)
		print("kmap: no mapped pa for va %#p (from pa %#p)\n",
			va, page->pa);
	else if (pa != page->pa)
		print("kmap: pa %#p != page->pa %#p; va %#p\n",
			pa, page->pa, va);
	else if (0)
		print("kmap: va %#p maps to pa %#p\n", va, pa);
	return va;
}

#ifdef unused
/*
 * Return the number of bytes that can be accessed above KADDR(pa)
 * in the first memory bank.
 * If pa is not a valid argument to KADDR, return 0.
 */
uintptr
cankaddr(uintptr pa)
{
	uintptr paend;

	paend = PHYSMEM + membanks[0].size;
	if(pa >= PHYSMEM && pa < paend)
		return paend - pa;
	return 0;
}
#endif

/* primarily while booting or shutting down (i.e., low identity map in place) */
void
usephysdevaddrs(void)
{
	m->virtdevaddrs = 0;
	m->clint = (Clint *)PAClint;
	m->consuart = PAUart0;
}

/* assumes iovmapsoc has run (on cpu0) */
void
usevirtdevaddrs(void)
{
	m->virtdevaddrs = 1;
	m->clint = (Clint *)soc.clint;
	m->consuart = (uintptr)soc.uart;
}
