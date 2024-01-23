/*
 * Fake trap catcher for machine mode of RISC-V.
 * must be linked in when we expect to start in super mode.
 */
#include "riscvl.h"

	GLOBL	bootmachmode(SB), $4	/* flag: boot in machine mode */
	DATA	bootmachmode(SB)/4, $0	/* is super; init to avoid bss */

TEXT mtrap(SB), 1, $-4
	MRET
