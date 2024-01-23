/*
 *	RISC-V atomic operations
 *	assumes A extension
 *	LR/SC only work on cached regions
 */
#include "/riscv64/include/atom.h"

#define ARG	8

#define MASK(w)	((1<<(w))-1)

TEXT adec(SB), 1, $-4			/* long adec(long*); */
	MOV	$-1, R9
	JMP	ainctop
TEXT ainc(SB), 1, $-4			/* long ainc(long *); */
	MOV	$1, R9
ainctop:
	FENCE_RW	/* flush changes to ram in case releasing a lock */
	/* after: value before add in R10, value after add in memory */
	AMOW(Amoadd, AQ|RL, 9, ARG, 10)
	FENCE_RW
	ADDW	R9, R10, R(ARG)		/* old value ±1 for ainc/adec */
	RET

/*
 * int cas(uint* p, int ov, int nv);
 *
 * compare-and-swap: atomically set *addr to nv only if it contains ov,
 * and returns the old value.  this version returns 0 on failure, 1 on success
 * instead.
 */
TEXT cas(SB), 1, $-4
	MOVWU	ov+XLEN(FP), R12
	MOVWU	nv+(XLEN+4)(FP), R13
	MOV	R0, R11		/* default to failure */
	FENCE
spincas:
	LRW(ARG, 14)		/* (R(ARG)) -> R14 */
	SLL	$32, R14
	SRL	$32, R14	/* don't sign extend */
	BNE	R12, R14, fail
	FENCE
	SCW(13, ARG, 14)	/* R13 -> (R(ARG)) maybe, R14=0 if ok */
	BNE	R14, spincas	/* R14 != 0 means store failed */
ok:
	MOV	$1, R11
fail:
	FENCE_RW
	MOV	R11, R(ARG)
	RET
