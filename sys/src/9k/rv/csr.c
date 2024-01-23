/*
 * risc-v rv64gc control and status registers access.
 *
 * mostly for infrequently-used CSRs; for others, see mch.s.
 * synthesises instructions on the fly.  assumes stack is executable.
 */
#include "u.h"
#include "../port/lib.h"
#include "mem.h"
#include "dat.h"
#include "fns.h"
#include "riscv.h"

enum {
	Csrdebug = 0,
};

typedef uintptr Csrfn(uintptr);

static void
puttvec(void *fn)
{
	(m->machmode? putmtvec: putstvec)(fn);
}

/*
 * attempt uncompressed instructions at insts with argument new and optional
 * pointer to success/failure status.  insts must include a RET.
 * probeinstr returns the result and copes with illegal instructions
 * via trapillinst.
 */
uintptr
probeinstr(void *insts, uintptr new, int *failedp)
{
	uintptr res;
	void *otvec;
	Csrfn *fn;
	Intrstate is;

	nointrs(&is);		/* no interrupts with modified trap vector */
	if (m->machmode)
		otvec = swapmtvec(badminst);
	else
		otvec = swapstvec(badsinst);

	m->probing = 1;			/* set probebad on any exception */
	m->probebad = 0;
	cacheflush();

	fn = (Csrfn *)insts;
	/* new and res will be in R(ARG) */
	res = (*fn)(new);		/* a fault will set m->probebad */

	cacheflush();
	m->probing = 0;
	if (failedp)
		*failedp = m->probebad;

	puttvec(otvec);
	restintrs(&is);
	return res;
}

/*
 * cope with bad CSRs before trapinit is called.
 * we don't use docsr for [MS]STATUS nor [MS]TVEC to avoid recursion.
 */
uintptr
docsr(uchar op, ushort csrno, uchar rs, uintptr new)
{
	int failed;
	uintptr old;
	ulong insts[3];

	if (Csrdebug)
		prf("csr %#ux new %#p", csrno, new);
	switch (csrno) {
	case SSTATUS:
	case MSTATUS:
	case STVEC:
	case MTVEC:
		prf("docsr can't handle that csr no.: %#ux!\n", csrno);
		return new;
	}

	insts[0] = SYSTEM | op<<12 | csrno<<20 | rs<<15 | ARG<<7;
	insts[1] = 0x67 | 1<<15;		/* JAL 0(R1) */
	insts[2] = SYSTEM | 0x105<<20;		/* WFI: deter prefetcher */

	old = probeinstr(insts, new, &failed);
	if (Csrdebug) {
		if (failed)
			prf(" failed.");
		prf(" old %#p\n", old);
	}
	return old;
}
