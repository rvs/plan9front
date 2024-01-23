TEXT	main(SB), 0, $(8*4)
	MOV	$setSB(SB), R28
	MOV	R0, argv0+0(FP)
	MOV	$argv0+0(FP), R9
	MOV	R9, argv+0(SP)
	BL	startboot(SB)
	RETURN
