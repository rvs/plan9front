	TEXT	memccpy(SB), $0
MOVW R8, 0(FP)
	MOVW	n+(3*XLEN)(FP), R8
	BEQ	R8, ret
	MOVW	s1+0(FP), R10
	MOVW	s2+XLEN(FP), R9
	MOVBU	c+(2*XLEN)(FP), R11
	ADD	R8, R9, R12

l1:	MOVBU	(R9), R13
	ADD	$1, R9
	MOVB	R13, (R10)
	ADD	$1, R10
	BEQ	R11, R13, eq
	BNE	R9, R12, l1
	MOVW	R0, R8
	RET

eq:	MOVW	R10, R8
ret:	RET
