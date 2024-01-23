typedef struct Tos Tos;
typedef struct Plink Plink;

#pragma incomplete Plink

struct Tos {
	struct			/* Per process profiling */
	{
		Plink	*pp;	/* known to be 0(ptr) */
		Plink	*next;	/* known to be 4(ptr) [8(ptr) on 64-bit systems] */
		Plink	*last;
		Plink	*first;
		ulong	pid;
		ulong	what;
	} prof;
	uvlong	cyclefreq;	/* cycle clock frequency if there is one, else 0 */
	vlong	kcycles;	/* cycles spent in kernel */
	vlong	pcycles;	/* cycles spent in process (kernel + user) */
	ulong	pid;		/* might as well put the pid here */
	ulong	clock;
#ifdef _RISCV64			/* avoid unneeded recompilation on others */
	union {
		/* scratch space for kernel use (e.g., mips fp delay-slot */
		/* execution) */
		ulong	kscr[4];
		uvlong	cpucap;	/* cpu capabilities bit vector */
	};
#else
	/* scratch space for kernel use (e.g., mips fp delay-slot execution) */
	ulong	kscr[4];
#endif
	/* top of stack is here */
};

extern Tos *_tos;
