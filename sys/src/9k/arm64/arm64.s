/*
 * arm64 (from nvidia tegra 2) machine assist, definitions
 * MP cortex-a72 processor
 *
 * R9 and R10 are used for `extern register' variables.
 * R11 is used by the loader as a temporary, so avoid it.
 */

#include "mem.h"
#include "arm64.h"

#define KADDR(pa)	(KZERO    | ((pa) & ~KSEGM))
#define PADDR(va)	(PHYSDRAM | ((va) & ~KSEGM))

/* produces byte offset in L1, unlike the C macro */
#define L1X(va)		(((((va))>>20) & 0x0fff)<<2)

/* steps on R0 */
#define DELAY(label, mloops) \
	MOV	$((mloops)*1000000), R0; \
label: \
	SUB.S	$1, R0; \
	BNE	label

/* print a byte on the serial console; clobbers R0 & R6; needs R12 (SB) set */
#define PUTC(c) \
	BARRIERS; \
	MOVW	$(c), R0; \
	MOV	$PHYSCONS, R6; \
	MOVW	R0, (R6); \
	BARRIERS

/*
 * new instructions
 */

#define SMC	WORD	$0xe1600070	/* low 4-bits are call # (trustzone) */
/* flush branch-target cache */
#define FLBTC  MTCP CpSC, 0, PC, C(CpCACHE), C(CpCACHEinviis), CpCACHEflushbtc
/* flush one entry of the branch-target cache, va in R0 (cortex) */
#define FLBTSE MTCP CpSC, 0, R0, C(CpCACHE), C(CpCACHEinvi), CpCACHEflushbtse

/*
 * ARM v7-a arch. ref. man. §B1.3.3 seems to say that we don't need ISBs
 * around writes to CPSR.  extend that to all barriers.
 */
#define CPSIE	WORD	$0xf1080080;	/* intr enable: zeroes I bit */ \
		BARRIERS
#define CPSID	WORD	$0xf10c00c0;	/* intr disable: sets I,F bits */ \
		BARRIERS
#define CPSAE	WORD	$0xf1080100	/* async abt enable: zeroes A bit */
#define CPSMODE(m) WORD $(0xf1020000 | (m)); /* switch to mode m (PsrM*) */ \
		BARRIERS

/* floating point control/status, coproc 10 */
#define VMRS(fp, cpu) WORD $(0xeef00a10 | (fp)<<16 | (cpu)<<12) /* FP → arm */
#define VMSR(cpu, fp) WORD $(0xeee00a10 | (fp)<<16 | (cpu)<<12) /* arm → FP */

/*
 * Execute all the barriers (except FLBTC which is only needed before enabling
 * branch prediction).
 * v7-a ARM says DSB performs a limited form of ISB: no following instr.
 * executes until DSB finishes, but there are no guarantees of previous instr.
 * completion.
 */
#define	BARRIERS	FLBTC; DSB; ISB

/*
 * set kernel SB for zero segment (instead of usual KZERO segment).
 * NB: the next line puts rubbish in R12:
 *	MOVW	$setR12-KZERO(SB), R12
 */
#define SETZSB \
	MOV	$setR12(SB), R12;		/* load kernel's SB */ \
	SUB	$KZERO, R12; \
	ADD	$PHYSDRAM, R12

/*
 * note that 5a's RFE is not the v6/7 arch. instruction (0xf8900a00),
 * which loads CPSR from the word after the PC at (SP), but rather
 * the pre-v6 simulation `MOVM.IA.S.W (SP), [PC]' (0xe8fd8000 since
 * MOVM is LDM in this case), which loads CPSR not from memory but
 * from SPSR due to `.S'.
 */
#define OP_WB 0x00200000	/* .W: `write-back updated register' bit */
#define RFEV7(r)    WORD $(0xf8900a00 | (r) << 16)
#define RFEV7W(r)   WORD $(0xf8900a00 | (r) << 16 | OP_WB)	/* RFE.W */
#define RFEV7DB(r)  WORD $(0xf9100a00 | (r) << 16)		/* RFE.DB */
#define RFEV7DBW(r) WORD $(0xf9100a00 | (r) << 16 | OP_WB)	/* RFE.DB.W */

/* return with cpu id in r and condition codes set from "r == 0" */
#define CPUID(r) \
	MFCP	CpSC, 0, r, C(CpID), C(CpIDidct), CpIDmpid; \
	AND.S	$(MAXMACH-1), r			/* mask out non-cpu-id bits */
