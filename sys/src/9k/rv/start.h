/*
 * definitions for assembly-language startup
 */
#undef DEBUG

/* dedicated registers during start-up */
SYSRB	= 23
LOCK	= 24
TMP2	= 25
TMP	= 26
MACHNO	= 27
UART0	= 29
MACHMODE= 30
HARTID	= 31

/* spinlock from unpriv. isa spec., figure a.7 */
/* callers assume that R(LOCK) contains &printlck, but any R(LOCK) will work. */
#define ACQLOCK	\
	MOV	$1, R(TMP2); \
	FENCE; \
	/* after: old printlck in R(TMP), updated printlck in memory */ \
	AMOW(Amoswap, AQ|RL, TMP2, LOCK, TMP); \
	BNE	R(TMP), -3(PC);		/* was non-0? lock held by another */ \
	FENCE
#define RELLOCK \
	FENCE; \
	MOVW	R0, (R(LOCK)); \
	FENCE

#ifdef DEBUG
#define CONSPUTLCK(c) \
	ACQLOCK; \
	CONSPUT(c); \
	RELLOCK
#else					/* DEBUG */
#define CONSPUTLCK(c)
#endif					/* DEBUG */
