/*
 *	RV32A atomic operations
 *	LR/SC only work on cached regions
 */
#include <atom.h>

#define ARG	8

TEXT ainc(SB), 1, $-4			/* long ainc(long *); */
	MOV	$1, R15
	JMP	start
TEXT adec(SB), 1, $-4			/* long adec(long*); */
	MOV	$-1, R15
start:
	MOV	R(ARG), R12		/* address of counter */
	FENCE
loop:
	MOV	R15, R13
	LRW(12, ARG)			/* (R12) -> R(ARG) */
	ADD	R(ARG), R13
	MOVW	R13, R(ARG)		/* return new value */
	SCW(13, 12, 14)			/* R13 -> (R12) maybe, R14=0 if ok */
	BNE	R14, loop
	FENCE
	RET

/*
 * int cas(uint* p, int ov, int nv);
 *
 * compare-and-swap: atomically set *addr to nv only if it contains ov,
 * and returns the old value.  this version returns 0 on success, -1 on failure
 * instead.
 */
TEXT cas(SB), 1, $-4
TEXT casp(SB), 1, $-4
	MOV	ov+XLEN(FP), R12
	MOV	nv+(2*XLEN)(FP), R13
	FENCE
spincas:
	LRW(ARG, 14)			/* (R(ARG)) -> R14 */
	BNE	R12, R14, fail
	SCW(13, ARG, 14)		/* R13 -> (R(ARG)) maybe, R14=0 if ok */
	BNE	R14, spincas		/* R14 != 0 means store failed */
	MOV	$1, R(ARG)
	RET
fail:
	MOV	R0, R(ARG)
	FENCE
	RET
