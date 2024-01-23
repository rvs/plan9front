/*
 * assembly-language machine assist for arm64
 */
#include "mem.h"
#include "arm64l.h"

/*
 * Read/write various system registers.
 */

TEXT rdtsc(SB), 1, $-4				/* Time Stamp Counter */
	FENCE
//	MOV	CSR(CYCLO), R(ARG)
	RET

/* machine-mode registers */
TEXT getsatp(SB), 1, $-4
//	MOVW	CSR(SATP), R(ARG)
	RET
TEXT putsatp(SB), 1, $-4
	FENCE
	FENCE_I
//	SFENCE_VMA(0, 0)			/* flush TLB */
//	MOV	R(ARG), CSR(SATP)		/* switch to the new map */
//	SFENCE_VMA(0, 0)			/* flush TLB */
	FENCE
	FENCE_I
	RET

TEXT getsb(SB), 1, $-4
	MOV	R28, R(ARG)
	RET
TEXT putsb(SB), 1, $-4
	MOV	R(ARG), R28
	RET
TEXT setsb(SB), 1, $-4
	MOV	$setSB(SB), R28
	RET

TEXT getsp(SB), 1, $-4
	MOV	RSP, R(ARG)
	RET
TEXT putsp(SB), 1,  $-4
	MOV	R(ARG), RSP
TEXT putspalign(SB), 1, $-4
	RET

TEXT clockenable(SB), 1, $-4
//	MOV	$Stie, R9			/* super timer intr enable */
//	CSRRS	CSR(SIE), R9, R0
	RET

/*
 * jump to upper address range and adjust addresses in registers to match.
 * upper->lower map must already be in effect.
 */
TEXT jumphigh(SB), 1, $-4
	FENCE
	FENCE_I
	MOV	$KSEG0, R(TMP)		/* lowest upper address */
	ORR	R(TMP), LR
	ORR	R(TMP), RSP
	ORR	R(TMP), R28		/* sb */
TEXT highalign(SB), 1, $-4
	ORR	R(TMP), R(MACH)		/* m */
	CMP	ZR, R(USER)
	BEQ	jumpdone
	ORR	R(TMP), R(USER)		/* up */
jumpdone:
	FENCE
	FENCE_I
	RET				/* jump into upper space */

/*
 * jump to lower (id) address range and adjust addresses in registers.
 * identity map must already be in effect.
 */
TEXT jumplow(SB), 1, $-4
	FENCE
	MOV	$~KSEG0, R(TMP)
	AND	R(TMP), LR
	AND	R(TMP), RSP
	AND	R(TMP), R28		/* sb */
TEXT lowalign(SB), 1, $-4
	AND	R(TMP), R(MACH)		/* m */
	AND	R(TMP), R(USER)		/* up */
	FENCE
	FENCE_I
	RET				/* jump into lower space */

/*
 * cache or tlb invalidation
 */
TEXT invlpg(SB), 1, $-4
	FENCE
	FENCE_I
	/* sifive u74 erratum cip-1200: can't do a non-global SFENCE */
////	SFENCE_VMA(0, ARG)	/* only fences pt leaf accesses for va in ARG */
//	SFENCE_VMA(0, 0)
	RET

/* approximate intel's wbinvd instruction: i & d fences, flush mmu state */
TEXT wbinvd(SB), 1, $-4
	FENCE
	FENCE_I
//	SFENCE_VMA(0, 0)
	RET

TEXT cacheflush(SB), 1, $-4
	FENCE
	FENCE_I
	RET

/*
 * Serialisation.
 */
TEXT pause(SB), 1, $-4
	FENCE
//	PAUSE				/* can be a no-op */
TEXT coherence(SB), 1, $-4
	FENCE
	RET

TEXT splhi(SB), 1, $-4
_splhi:
	FENCE
//	MOV	CSR(SSTATUS), R(ARG)
//	AND	$Sie, R(ARG)		/* return old state */
//	CSRRC	CSR(SSTATUS), $Sie, R0	/* disable intrs */
//	MOV	LR, SPLPC(R(MACH)) 	/* save PC in m->splpc */
_spldone:
	RET

TEXT spllo(SB), 1, $-4			/* marker for devkprof */
_spllo:
//	MOV	CSR(SSTATUS), R(ARG)
//	AND	$Sie, R(ARG)
	FENCE
	CMP	ZR, R(ARG)
	BNE	_splgolo		/* Sie=1, intrs were enabled? */
	/* they were disabled */
//	MOV	ZR, SPLPC(R(MACH))	/* clear m->splpc */
_splgolo:
	/* leave SIE bits alone */
//	CSRRS	CSR(SSTATUS), $Sie, R0	/* enable intrs */
	RET

/* assumed between spllo and spldone by devkprof */
TEXT splx(SB), 1, $-4
//	MOV	CSR(SSTATUS), R9
//	AND	$Sie, R9
//	AND	$Sie, R(ARG), R10
	CMP	R9, R10
	BEQ	_spldone		/* already in desired state? */
	CMP	ZR, R10
	BEQ	_splhi			/* want intrs disabled? */
	B	_spllo			/* want intrs enabled */

TEXT spldone(SB), 1, $-4			/* marker for devkprof */
	RET

TEXT islo(SB), 1, $-4
//	MOV	CSR(SSTATUS), R(ARG)
//	AND	$Sie, R(ARG)
	RET

/*
 * atomic operations
 */

TEXT aincnonneg(SB), 1, $XLEN			/* int aincnonneg(int*); */
	BL	ainc(SB)
	CMP	ZR, R(ARG)
	BGT	_aincdone
	/* <= 0 after incr, so overflowed, so blow up */
	MOV	(ZR), ZR			/* over under sideways down */
_aincdone:
	RET

TEXT adecnonneg(SB), 1, $XLEN			/* int adecnonneg(int*); */
	BL	adec(SB)
	CMP	ZR, R(ARG)
	BGE	_aincdone
	/* <0, so underflow.  use own copy of XORQ/MOVQ to disambiguate PC */
	MOV	(ZR), ZR			/* over under sideways down */
	RET

/* see libc/riscv64/atom.s for ainc & adec */
TEXT aadd(SB), 1, $-4			/* int aadd(int*, int addend); */
	MOVW	addend+XLEN(FP), R9
//	AMOW(Amoadd, AQ|RL, 9, ARG, 10)
	ADDW	R9, R10, R(ARG)		/* old value + addend */
//	SEXT_W(R(ARG))
	RET

TEXT amoswapw(SB), 0, $(2*XLEN)		/* ulong amoswapw(ulong *a, ulong nv) */
	MOVWU	nv+XLEN(FP), R9		/* new value */
	FENCE
//	AMOW(Amoswap, AQ|RL, 9, ARG, 10) /* *R(ARG) += R9; old *R(ARG) -> R10 */
	FENCE
	MOVW	R10, R(ARG)		/* old value */
	RET

#ifdef unused
TEXT amoorw(SB), 0, $(2*XLEN)		/* ulong amoorw(ulong *a, ulong bits) */
	MOVWU	bits+XLEN(FP), R9
	FENCE
	AMOW(Amoor, AQ|RL, 9, ARG, 10) /* *R(ARG) |= R9; old *R(ARG) -> R10 */
	FENCE
	MOVW	R10, R(ARG)		/* old value */
	RET
#endif

/*
 * idle until an interrupt.  call it splhi().
 *
 * idlepause != 0 will bypass this and use PAUSE.
 * WFI will be awakened by an intr, even if disabled.
 * tinyemu is less busy & much more responsive without WFI, but it burns
 * power.  Using WFI makes even console echoing take seconds.
 */
TEXT halt(SB), 1, $-4
	FENCE			/* make our changes visible to other harts */
	WFI			/* WFI may be a no-op or hint */
	FENCE			/* make other harts' changes visible to us */
	RET

TEXT reset(SB), 1, $-4
bragain:
//	EBREAK
	B	bragain

/*
 * Label consists of a stack pointer and a programme counter.
 */
TEXT gotolabel(SB), 1, $-4			/* gotolabel(Label *) */
	MOV	0(R(ARG)), RSP			/* restore sp */
	MOV	XLEN(R(ARG)), LR		/* restore return pc */
	MOV	$1, R(ARG)
	RET

TEXT setlabel(SB), 1, $-4			/* setlabel(Label *) */
	MOV	RSP, 0(R(ARG))			/* store sp */
	MOV	LR, XLEN(R(ARG))		/* store return pc */
	MOV	ZR, R(ARG)
	RET

/*
 * mul64fract(uvlong *r, uvlong a, uvlong b)
 * Multiply two 64-bit numbers, return the middle 64 bits of the 128-bit result.
 *
 * use 64-bit multiply with 128-bit result
 */
TEXT mul64fract(SB), 1, $-4
	/* r, the address of the result, is in RARG */
	MOV	a+XLEN(FP), R9
	MOV	b+(2*XLEN)(FP), R11

//	MULHU	R9, R11, R13			/* high unsigned(al*bl) */
	MUL	R9, R11, R14			/* low unsigned(al*bl) */

	LSR	$32, R14			/* lose lowest long */
	LSL	$32, R13  /* defeat sign extension & position low of hi as hi */
	ORR	R13, R14

	MOV	R14, (R(ARG))
	RET

TEXT clz(SB), 1, $-4				/* int clz(Clzuint) */
	CLZ	R(ARG), R(ARG)
	RET

// TODO: insert trap entry code

TEXT sysrforkret(SB), 1, $-4
	MOV	ZR, R(ARG)
//	JMP	_syscallreturn	// TODO
	RET
