#ifdef unused
/*
 * void monitor(void* address, u32int extensions, u32int hints);
 * void mwait(u32int extensions, u32int hints);
 *
 * Note: extensions and hints are only 32-bits.
 * There are no extensions or hints defined yet for MONITOR,
 * but MWAIT can have both.
 * These functions and prototypes may change.
 */
TEXT monitor(SB), 1, $-4
	MOVQ	RARG, AX			/* address */
	MOVL	extensions+8(FP), CX		/* (c|sh)ould be 0 currently */
	MOVL	hints+16(FP), DX		/* (c|sh)ould be 0 currently */
	MONITOR
	RET

TEXT mwait(SB), 1, $-4
	MOVL	RARG, CX			/* extensions */
	MOVL	hints+8(FP), AX
	MFENCE
	MWAIT	/* an interrupt or any store to monitored word will resume */
	MFENCE
	RET
#endif
