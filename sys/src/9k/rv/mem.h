/*
 * Memory and machine-specific definitions.  Used in C and assembler.
 */

#define TOS(stktop)	((stktop)-sizeof(Tos))

/*
 * parameters & devices needed early during boot.
 */
#include "addrconf.h"

#define TODO 0		/* for "if (TODO)" */
#define CRASH_OK 0	/* for "if (CRASH_OK)" */

/*
 * max. # of cpus; costs 1 byte/cpu in sizeof(Page)
 */
#define MACHMAX		8
#define HARTSLOP	2	/* max. ids in gaps in hartid space */
#define HARTMAX	(MACHMAX + HARTSLOP)

/* "a" must be power of 2 */
#define ALIGNED(p, a)		(((uintptr)(p) & ((a)-1)) == 0)
#define PTEADDRBITS(pte)	((pte) &  (VMASK(56-PGSHFT) << PTESHFT))
#define PTEATTRBITS(pte)	((pte) & ~(VMASK(56-PGSHFT) << PTESHFT))
#define PADDRINPTE(pte)		((PTEADDRBITS(pte) >> PTESHFT) * PGSZ)
#define PAGEPRESENT(pte)	((pte) & PteP)

#define KB		1024u			/* 2^10 0x0000000000000400 */
#define MB		1048576u		/* 2^20 0x0000000000100000 */
#define GB		1073741824u		/* 2^30 000000000040000000 */
#define TB		1099511627776ull	/* 2^40 0x0000010000000000 */
#define PB		1125899906842624ull	/* 2^50 0x0004000000000000 */
#define EB		1152921504606846976ull	/* 2^60 0x1000000000000000 */

#define KHZ		1000
#define MHZ		(1000*1000)
#define GHZ		(1000*MHZ)

#define HOWMANY(x, y)	(((x)+((y)-1))/(y))
#define ROUNDUP(x, y)	(HOWMANY((x), (y))*(y))
#define ROUNDDN(x, y)	(((x)/(y))*(y))

#define MIN(a, b)	((a) < (b)? (a): (b))
#define MAX(a, b)	((a) > (b)? (a): (b))

#define MASK(w)	 ((1u  <<(w)) - 1)
#define VMASK(w) ((1ull<<(w)) - 1)

/*
 * Sizes
 */
#define BI2BY		8		/* bits per byte */
#define BY2V		8		/* bytes per vlong */
/*
 * a stack element is nominally the minimum width that the compiler will push.
 * on risc-v, that's actually 4, but we want to align stacks to 8.
 */
#define BY2SE		8		/* bytes per stack element (uintptr) */
#define BLOCKALIGN	8
#define CACHELINESZ	64
#define SBIALIGN	16LL		/* stack alignment for SBI calls */
/* old names */
#define BY2WD		4		/* bytes per int */
#define BI2WD		(BI2BY*BY2WD)
#define BY2PG		PGSZ
#define PGROUND(s)	ROUNDUP(s, PGSZ)

#define PGSZ		(4*KB)			/* smallest page size */
#define PGSHFT		12			/* log2(PGSZ) */
#define PTSZ		PGSZ			/* page table page size */
/* log(ptes per PTSZ), also virt address bits per PT level */
#define PTSHFT		9
#define PTESHFT		10			/* bits right of PPN in PTE */

#define MACHSZ		PGSZ			/* Mach size, excluding stack */
/*
 * Greatest Mach stack use observed is 1928 bytes (2504 in a rebooted kernel).
 * Note that a single Ureg is 296 bytes and each enabled plic priority level
 * may push one, so they could all be on the stack at once.
 * Changing MACHSTKSZ requires recomputing -T for reboottramp; see kzmkfile.
 */
#define MACHSTKSZ	(3*PGSZ)		/* Mach stack size */
/* greatest use seen is 408 bytes. */
#define INITSTKSIZE	2048

/* Greatest kstack (Proc stack) use observed is 4320 bytes. */
#define KSTACK		(3*PGSZ)		/* Proc kernel stack size */
#define STACKALIGN(sp)	((sp) & ~((uintptr)BY2SE-1)) /* bug: assure with alloc */
/*
 * a system call pushes a Ureg at top of kstack, then a PC below it.
 * this skips past the PC to produce the address of the Ureg.
 */
#define SKIPSTKPC(sp)	((uintptr)(sp) + sizeof(uintptr))

/*
 * Time
 */
#ifndef HZ
#define HZ		100			/* clock frequency */
#endif
#define MS2HZ		(1000/HZ)		/* millisec per clock tick */
#define TK2SEC(t)	((t)/HZ)		/* ticks to seconds */

/*
 *  Address spaces
 *
 *  So far, hardware implements only 39 bits of virtual addresses.
 *  Tinyemu implements both 39 and 48 bits.
 *
 *  Kernel is typically loaded at (or just above) 2GB (PHYSMEM, start of ram);
 *  3GB-1MB is used to hold the Sys and Syspercpu data structures.
 *
 *  User is at low addresses; kernel vm (high addresses) starts at KZERO.
 */

/*
 * the choice of address size has to be compiled in (it affects KZERO),
 * so we can't in practice choose it at run time.
 */
#ifdef SV64
#define PAGINGMODE	Sv64
#define VMBITS		64		/* KZERO is 0x8000000000000000 */
#define Npglvls		6
#else
#ifdef SV57
#define PAGINGMODE	Sv57
#define VMBITS		57		/* KZERO is 0xff00000000000000 */
#define Npglvls		5
#else
#ifdef SV48
#define PAGINGMODE	Sv48
#define VMBITS		48		/* KZERO is 0xffff800000000000 */
#define Npglvls		4
#else
#define PAGINGMODE	Sv39
#ifdef LOWKERN
#define VMBITS		38		/* KZERO is 0x2000000000 */
#else
#define VMBITS		39		/* KZERO is 0xffffffc000000000 */
#endif					/* LOWKERN */
#define Npglvls		3
#endif					/* SV48 */
#endif					/* SV57 */
#endif					/* SV64 */

#define Toplvl		(Npglvls - 1)	/* -1 for zero-origin level numbers */

/* VMAPSZ must be multiple of top-level super-page; using PGLSZ ensures that */
#define VMAPSZ		PGLSZ(Toplvl)	/* for Sv39, 1 GB */
#define ADDRSPCSZ	(1ull<<(VMBITS-1)) /* -1 for high bit, is user vs kernel */

#define	UZERO		0			/* user segment */
#define	UTZERO		(UZERO+PGSZ)		/* first address in user text */
#define UTROUND(t)	ROUNDUP((t), PGSZ)
/* leave a guard page above stack to catch stack underflow */
#define USTKTOP		(ADDRSPCSZ - PGSZ)
#define USTKSIZE	(16*MB)			/* max. size of user stack */
#define TSTKTOP		(USTKTOP-USTKSIZE)	/* end of new stack in sysexec */

/*
 * PHYSMEM is physical start of RAM; membanks[0].size is the usable memory
 * directly above that.  this base address is likely when a risc-v
 * machine has M & S modes (due to Unix conventions).
 */
#ifndef PHYSMEM
#define PHYSMEM		0x80000000u		/* cached */
#endif

/*
 *  kernel vm (high addresses) starts at KZERO;
 *  KZERO maps the first kernmem bytes, one to one, high to low
 *  (i.e., KZERO -> 0), for the kernel's use, rounded up to the nearest GB.
 *
 *  See mmu.c and memory.c.
 */

/*
 * KZERO is the kernel virtual address that maps to physical 0 and the kernel
 * typically begins a little above KZERO.
 * The kernel's text, data and bss segments must fit between KZERO
 * and the top of kernel address space (VZERO).  KZERO must be a contiguous
 * mask in the highest-order address bits.
 *
 * The width of virtual addresses determines KZERO, to allow simple mappings.
 * thus the size of virtual addresses is chosen at compile time.
 */
#ifdef LOWKERN
#define VTOP		(2*ADDRSPCSZ)
#else
#define VTOP		0ull		/* highest virtual address used + 1 */
#endif
#define KZERO		(VTOP - ADDRSPCSZ)

/*
 * kernel usually sits at start of ram (KZERO+PHYSMEM) but some systems are odd
 * and the kernel has to start a little higher.
 */
#define KTZERO ((uintptr)ensurehigh(_main)) /* kernel text base; see mkfile jl -T */

#define isuseraddr(ptr)	((uintptr)(ptr) <= ADDRSPCSZ)

/*
 *  virtual MMU
 */
#define	PTEMAPMEM	(4*MB)	/* memory mapped by a Pte map (arbitrary) */
#define	PTEPERTAB	(PTEMAPMEM/PGSZ) /* pages per Pte map */
#define SSEGMAPSIZE	16		/* initial Ptes per Segment */
/* maximum Segment map size in Ptes. */
#define SEGMAPSIZE (ROUNDUP(USTKTOP, PTEMAPMEM) / PTEMAPMEM) /* <= 262,144 (Sv39) */

/*
 * This is the interface between fixfault and mmuput.
 * Should be in port.  But see riscv.h Pte*.
 */
#define PTEVALID	(1<<0)
#define PTERONLY	(1<<1)
#define PTEWRITE	(1<<2)
#define PTEUSER		(1<<4)		/* unused */
#define PTEUNCACHED	0		/* no such bit on riscv */

#define getpgcolor(a)	0

/*
 * Hierarchical Page Tables.
 * For example, traditional Intel IA-32 paging structures have 2 levels,
 * level 1 is the PD, and level 0 the PT pages; with IA-32e paging,
 * level 3 is the PTROOT, level 2 the PDP, level 1 the PD,
 * and level 0 the PT pages. The PTLX macro gives an index into the
 * page-table page at level 'l' for the virtual address 'v'.
 */
#define NPGSZ		Npglvls	/* max. # of page sizes & page table levels */
#define PGLSHFT(l)	((l)*PTSHFT + PGSHFT)
#define PGLSZ(l)	(1ull << PGLSHFT(l))
#define PTLX(v, l)	(uintptr)(((uvlong)(v) >> PGLSHFT(l)) & VMASK(PTSHFT))

/* this can go when the arguments to mmuput change */
#define PPN(x)		((uintptr)(x) & ~((uintptr)PGSZ-1))
#define PTEPPN(x)	(((uintptr)(x) / PGSZ) << PTESHFT)

#define SYSEXTEND	(64*KB)		/* size of Sys->sysextend */
#define KMESGSIZE	(16*KB)

/* pages allocated by vmap, before malloc is ready */
#define EARLYPAGES	128	/* used: 0 temu, 33 icicle, 32 beaglev */

/* offsets in Sys struct, from dat.h.  originally at start of ram, thus LOW*. */
#define LOW0SYS		(MACHSTKSZ + PTSZ + MACHSZ)
#define LOW0SYSPAGE	(LOW0SYS + PGSZ)

/* verify assertion (cond) at compile time */
#define CTASSERT(cond, name) struct { char name[(cond)? 1: -1]; }
