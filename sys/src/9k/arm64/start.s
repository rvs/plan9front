/*
 * assembler start up for arm64 (v8-a architecture and later)
 */
#include "mem.h"
#include "arm64l.h"

#define INITSTKSIZE	8192

/*
 * only cpu0 starts here, assumed to be in aarch64 secure mode at EL1
 * and mmu off or identity-mapped
 */
TEXT _main(SB), 1, $-4
	MOV	$(IMASK>>DAIFSHIFT), DAIFSet	/* mask interrupts */
	MOV	$setSB(SB), R28
	BARRIERS
	MRSN1(SCTLR_EL1, 8)
	BIC	$(MMUON|DCACHEON|ICACHEON), R8	/* mmu & caches disabled */
	MSRN1(8, SCTLR_EL1)
	MOV	$initstk+(INITSTKSIZE-8)(SB), RSP	/* SP_EL1 */
	MOV	ZR, RARG
	BL	main(SB)
loop:
	WFI
	B	loop

	GLOBL	initstk(SB), $INITSTKSIZE
