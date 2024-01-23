/*
 * minimal risc-v assembly-language initialisation to enable
 * calling low() in C.  this is the first kernel code executed.
 * only one cpu is started.
 */
#include "mem.h"
#include "riscv64l.h"
#include "start.h"

#define Z(n)	MOV R0, R(n)

	/* data segment, not bss, variables, due to initialisation */
	GLOBL	initstks(SB), $INITSTKSIZE
	DATA	initstks(SB)/8, $0

/*
 * this may be entered in machine or super mode.
 * stack pointer is unknown at entry, so use $-4 to not touch it
 * until we can establish a new stack.
 * First, disable all interrupts.
 *
 * N.B.: all cpus may be executing this code simultaneously.
 * also, if we're rebooting, the secondary cpus may be stalled on sys->secstall.
 */
TEXT _main(SB), 1, $-4			/* _main avoids libc's main9.s */
	FENCE
	FENCE_I
	SPLHI
	MOV	R0, CSR(SIE)
	MOV	R0, CSR(SIP)
	MOV	R10, R(HARTID)	/* save likely sbi hartid in a safe place */
	/* likewise, R11 may contain a device tree pointer */
	/*
	 * stop paging, if it's on.  must be in the identity map, but that's
	 * likely on risc-v systems intended to run unix.  otherwise, traps
	 * to machine mode (with no virtual memory) would fault endlessly.
	 */
	LOADSATP(R0)

	/*
	 * Prepare the static base before use.
	 * SB will be in the physical (low) address range because the PC is
	 * (setting SB is an LUI off(PC) and an ADD).
	 * This eliminates the need for "-KZERO" in machine mode or
	 * when otherwise executing in low addresses, except when using
	 * addresses in static initializers (e.g., an Rvarch struct).
	 */
	MOV	$setSB(SB), R3

	MOV	$panicstk+(INITSTKSIZE)(SB), R2
TEXT pstkalign(SB), 1, $-4		/* reset SP, FP for new R2 */

	MOV	$PAUart0, R(UART0)	/* now safe to print on PAUart0 */
	MOVW	bootmachmode(SB), R(MACHMODE)
	BEQ	R(MACHMODE), super

	/*
	 * we're supposed to be in machine mode here.
	 * risc-v makes it hard to tell without trapping
	 * and possibly losing control of the CPU.
	 */
	CSRRC	CSR(MSTATUS), $(Sie|Mie), R0
	MOV	R0, CSR(MIE)
	MOV	R0, CSR(MIP)
	MOV	CSR(MHARTID), R(HARTID)

	MOV	$setSB(SB), R3	/* again, in case M traps happened above */
	MOV	R0, CSR(MSCRATCH)	/* m for early mtrap */

	MOV	$recmtrapalign(SB), R9	/* catch early stray M faults */
	CSRRW	CSR(MTVEC), R9, R9
	MOV	R9, origmtvec(SB)	/* stash initial mtvec for later */

	MOV	$Defmsts, R(TMP)
	MOV	R(TMP), CSR(MSTATUS)

	MOV	$'M', R12
	JMP	mach
super:
	MOV	$'S', R12
mach:
	/* interrupts are now off, in M or S mode */
	CONSPUT(R12)

	/*
	 * zero most registers to avoid possible non-determinacy.
	 * R2 is stack pointer, R3 is static base,
	 * R29 is UART0, R30 is MACHMODE, R31 is HARTID.
	 */
	Z(1); Z(4); Z(5); Z(6); Z(7); Z(8); Z(9); Z(10); Z(11); Z(12); Z(13)
	Z(14); Z(15); Z(16); Z(17); Z(18); Z(19); Z(20); Z(21); Z(22); Z(23)
	Z(24); Z(25); Z(26); Z(27); Z(28)

	/* save PC as approx. PADDR(KTZERO) for mainpc below */
	JAL	R12, 1(PC)
	MOV	R12, mainpc(SB)

	MOV	$dummysc(SB), R12
	SCW(0, 12, 0)	/* discharge any lingering reservation we hold */

	MOV	R0, CSR(SSCRATCH)	/* m for early strap */
	MOV	$rectrapalign(SB), R9	/* catch early stray S faults */
	MOV	R9, CSR(STVEC)

	MOV	$Defssts, R(TMP)	/* prev S mode is user */
	MOV	R(TMP), CSR(SSTATUS)
	CONSPUT($'*')

	/*
	 * assign machnos sequentially from zero.
	 * after Amoadd: old hartcnt in MACHNO, updated hartcnt in memory.
	 */
	MOV	$hartcnt(SB), R9
	MOV	$1, R10
	AMOW(Amoadd, AQ|RL, 10, 9, MACHNO)
	BNE	R(MACHNO), notzero
	/*
	 * we are cpu0, so zero bss while secondaries wait.
	 * other system-wide set up could be done here too.
	 */
	MOV	$edata(SB), R(TMP)
	MOV	$end(SB), R(TMP2)
zerobss:
	MOV	R0, (R(TMP))
	ADD	$XLEN, R(TMP)
	BLTU	R(TMP2), R(TMP), zerobss
	FENCE

	/* store hart id in hartids[machno] for Mach->hartid */
	MOV	$hartids(SB), R12
	MOV	$2, R13			/* sizeof(short) */
	MUL	R(MACHNO), R13
	ADD	R13, R12
	MOVH	R(HARTID), (R12)
	FENCE

	/* set up a temporary stack for this cpu, based on machno. */
	MOV	$initstks+(INITSTKSIZE)(SB), R(TMP)
	MOV	$INITSTKSIZE, R10
	MUL	R(MACHNO), R10, R11
	ADD	R11, R(TMP), R2		/* just past my init stack */
	SUB	$SBIALIGN, R2		/* put sp within stack */

	MOV	R(MACHNO), R(ARG)
	JAL	LINK, low(SB)		/* low(machno); no return */

	CONSPUT($'?');	CONSPUT($'r');	CONSPUT($'e');	CONSPUT($'t')
	FENCE

notzero:
	/*
	 * we are a secondary, so wait here until cpu0 finishes zeroing bss.
	 */

/* WFI may pause the core cycle counter */
TEXT wfi(SB), 1, $-4
	WFI
	JAL	R0, wfi(SB)
