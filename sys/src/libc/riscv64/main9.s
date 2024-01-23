/* normal startup following exec.  assume vlong alignment of SP */
#define NPRIVATES	16

GLOBL	_tos(SB), $XLEN
GLOBL	_privates(SB), $XLEN
GLOBL	_nprivates(SB), $4

TEXT	_main(SB), 1, $(4*XLEN + NPRIVATES*XLEN)
	MOV	$setSB(SB), R3
	/* _tos = arg */
	MOV	R8, _tos(SB)

	MOV	$p-(NPRIVATES*XLEN)(SP), R9
	MOV	R9, _privates(SB)
	MOV	$NPRIVATES, R9
	MOVW	R9, _nprivates(SB)

	MOV	inargc-XLEN(FP), R8
	MOV	$inargv+0(FP), R10
	MOV	R8, XLEN(R2)		/* R2 -> SP? */
	MOV	R10, (2*XLEN)(R2)
	JAL	R1, main(SB)
loop:
	MOV	$_exitstr<>(SB), R8
	MOV	R8, XLEN(SP)
	JAL	R1, exits(SB)
	JMP	loop

DATA	_exitstr<>+0(SB)/4, $"main"
GLOBL	_exitstr<>+0(SB), $5
