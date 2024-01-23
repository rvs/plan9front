typedef struct Asm Asm;
typedef struct Clint Clint;
typedef struct Cpu Cpu;
typedef struct Intrstate Intrstate;
typedef struct Ioconf Ioconf;
typedef struct ISAConf ISAConf;
typedef struct Kmesg Kmesg;
typedef struct Label Label;
typedef struct Lock Lock;
typedef struct Mach Mach;
typedef struct MCPU MCPU;
typedef struct Membank Membank;
typedef struct MFPU MFPU;
typedef struct MMMU MMMU;
typedef uvlong Mpl;
typedef uvlong Mreg;
typedef struct Msi Msi;
typedef struct Page Page;
typedef struct Pcidev Pcidev;
typedef struct PFPU PFPU;
typedef struct Plic Plic;
typedef struct PMMU PMMU;
typedef struct PNOTIFY PNOTIFY;
typedef struct Proc Proc;
typedef uvlong PTE;
typedef struct Reboot Reboot;
typedef struct Rvarch Rvarch;
typedef struct Sbiret Sbiret;
typedef struct Soc Soc;
typedef struct Sys Sys;
typedef struct Syspercpu Syspercpu;
typedef uintptr uintmem;		/* physical address.  horrible name */
typedef struct Ureg Ureg;
typedef struct Vctl Vctl;

#pragma incomplete Cpu
#pragma incomplete Ureg

#define SYSCALLTYPE	8	/* nominal syscall trap type (not used) */
#define MAXSYSARG	5	/* for mount(fd, afd, mpt, flag, arg) */

#define INTRSVCVOID
#define Intrsvcret void

typedef Intrsvcret (*Intrsvc)(Ureg*, void*);

/*
 *  parameters for sysproc.c
 */
#define AOUT_MAGIC	Y_MAGIC

/*
 *  machine dependent definitions used by ../port/portdat.h
 */
struct Lock
{
	int	key;
	ulong	noninit;		/* must be zero */
	Mreg	sr;
	uintptr	pc;
	Proc*	p;
	Mach*	m;
	uvlong	lockcycles;
	char	isilock;
};

struct Label
{
	uintptr	sp;
	uintptr	pc;
};

/*
 *  FPU stuff in Proc
 */
struct PFPU {
	int	fpustate;		/* per-process logical state */
	int	fcsr;
	double	fpusave[32];
	int	fptraps;		/* debugging count */
};

/*
 *  MMU stuff in Proc
 */
#define NCOLOR 1
struct PMMU
{
	Page*	mmuptp[NPGSZ];		/* page table pages for each level */
};

/*
 *  things saved in the Proc structure during a notify
 */
struct PNOTIFY
{
	void*	emptiness; /* "void emptiness" generates an acid syntax error */
};

/*
 * Address Space Map.
 * Low duty cycle.
 */
struct Asm
{
	uintmem	addr;
	uintmem	size;
	int	type;
	int	location;
	Asm*	next;
	uintmem base;	/* used by port; ROUNDUP(addr, PGSZ) */
	uintmem	limit;	/* used by port; ROUNDDN(addr+size, PGSZ) */
	uintptr	kbase;	/* used by port; kernel for base, used by devproc */
};
extern Asm* asmlist;

#include "../port/portdat.h"

/*
 *  CPU stuff in Mach.
 */
struct MCPU {
	char	empty;
};

/*
 *  per-cpu physical FPU state in Mach.
 */
struct MFPU {
	int	turnedfpoff;		/* so propagate to Ureg */
	uvlong	fpsaves;		/* counters */
	uvlong	fprestores;
};

/*
 *  MMU stuff in Mach.
 */
struct MMMU
{
	Page*	ptroot;			/* ptroot for this processor */
	Page	ptrootpriv;		/* we need a page */
//	PTE*	pmap;			/* unused so far */

	/* page characteristics are actually per system */
	int	npgsz;	/* # sizes in use; also # of levels in page tables */
	uint	pgszlg2[NPGSZ];
	uintptr	pgszmask[NPGSZ];
};

/*
 * Per processor information.
 *
 * The offsets of the first elements may be known
 * to low-level assembly code, so do not re-order nor channge type:
 *	machno, bootmachmode - rebootcode
 *	splpc		- splhi, spllo, splx
 *	proc, regsave	- strap, mtrap
 *	online, hartid	- start
 * changes here must be synchronised with byte offsets in riscv64l.h.
 */
struct Mach
{
	int	machno;		/* 0 logical id of processor */
	uintptr	splpc;		/* 8 pc of last caller to splhi */
	Proc*	proc;		/* 16 current process on this cpu */
	uintptr	regsave[2*5+1];	/* 24 (super mach)×(r2 r3 r4 r6 r9), mach r5 */
				/* saved @ trap entry */
	union {
		uvlong	*mtimecmp; /* 112 clint's mtimecmp for this hart */
		uvlong	*stimecmp; /* 112 clint's stimecmp for this hart */
	};
	int	online;		/* 120 flag: actually enabled */
	int	hartid;		/* 124 riscv cpu id; often not machno */
	Clint	*clint;		/* 128 currently-valid address */
	uintptr	consuart;	/* 136 " */
	uchar	bootmachmode;	/* 144 flag: machine mode at boot time */
	uchar	probing;	/* 145 flag: probing an address */
	uchar	probebad;	/* 146 flag: probe failed: bad address */

	/* safe to re-order or change from here down */
	uintptr	stack;		/* non-syscall stack base */
	ulong	ipiwait;	/* count as flag: in wfi, waiting for ipi */
	short	plicctxt;	/* base plic M context: f(hartid) */
	uchar	waiting;	/* flag: waiting for m->online */

	MMMU;

	ulong	ticks;		/* of the clock since boot time */
	Label	sched;		/* scheduler wakeup */
	Proc*	readied;	/* for runproc */
	ulong	schedticks;	/* next forced context switch */

	int	color;

	int	tlbfault;	/* per-cpu stats */
	int	tlbpurge;
	int	pfault;
	int	cs;
	int	syscall;
	int	load;
	int	intr;
	Perf	perf;		/* performance counters */

	int	mmuflush;	/* make current proc flush its mmu state */
	int	ilockdepth;
	uintptr	ilockpc;

	int	lastintr;	/* debugging: irq of last intr */
	/* flag: use virtual device addresses (user proc is mapped) */
	uchar	virtdevaddrs;
	uchar	machmode;	/* flag: currently in machine mode? */
	uchar	clockintrsok;	/* flag: safe to call timerintr */
	int	clockintrdepth;

	vlong	cpuhz;	/* also frequency of user readable cycle counter */
	int	cpumhz;
	uvlong	boottsc;	/* my tsc's value at boot */
	/* current value of this hart's clint->mtimecmp, in case unreadable */
	uvlong	timecmp;

	MFPU;
	MCPU;
};

struct Kmesg {
	struct  Kmesghdr {
		ulong	magic;
		uint	size;	/* of buf; could change across reboots */
		uint	n;	/* bytes in use */
	};
	char	buf[KMESGSIZE - sizeof(struct Kmesghdr)];
};

/*
 * Sys is system-wide (not per-cpu) data at a fixed address.
 * It is located there to allow fundamental datastructures to be
 * created and used before knowing where free memory begins
 * (e.g. there may be modules located after the kernel BSS end) or ends.
 *
 * The layout is known in the bootstrap code in start.s, mmu.c, and mem.h
 * via the LOW0* definitions.
 * It is logically two parts: the per-processor data structures
 * for the bootstrap processor (stack, Mach, and page tables),
 * and the global information about the system (syspage, ptrpage).
 * Some of the elements must be aligned on page boundaries, hence
 * the unions.
 */

/*
 * parameters to reboot trampoline code; layout is known to riscv64.h, mtrap.s
 * and rebootcode.s
 */
struct Reboot {
	void	(*tramp)(Reboot *);	/* tramp pa */
	uintptr	phyentry;
	uintptr	phycode;
	uintptr	rebootsize;
	uintptr	secstall;		/* flag: secondaries loop while set */
	uintptr	satp;			/* initial satp contents for sec's */
	ushort	rebooting;	/* flag: don't use sbi hsm to start cpus */
};

enum {
	Ptpgptes = PTSZ / sizeof(PTE),	/* PTEs per page */
};

/*
 * per-cpu portion common to all cpus; cpus other than 0 allocate their own in
 * newcpupages(); that code assumes pteroot follows machstk and that the
 * address of either can be derived from the other, so keep them together and
 * in order.  on cpu0, this is zeroed by zerosyssome.  currently 20K.
 */
struct Syspercpu {
	uchar	machstk[MACHSTKSZ];		/* must be multiple of 4K */
	PTE	pteroot[Ptpgptes];		/* initial page tables */
	union {
		Mach	mach;
		uchar	machpage[MACHSZ];
	};
};

/*
 * Sys lives at the top of the first memory bank minus 1MB
 * (see lowsys computation in low.c).  sizeof(Sys) is 612K currently.
 */
struct Sys {
	Syspercpu;
	/* rest is statically allocated once for cpu0 to share with all cpus */

	uchar reboottramp[PGSZ];	/* copy reboot trampoline here */
	union {
		uchar	syspage[PGSZ];
		struct memory_timing_cpus {
			Reboot;	/* do not zero; location known to assembler */

			/* zero from here to before kmesg */
			uintptr	pmbase;		/* start of physical memory */
			uintptr	pmstart;	/* available physical memory */
			uintptr	pmoccupied;	/* how much is occupied */
			/* pmend is little used in rv; see memory.c */
			uintptr	pmend;		/* total span, including gaps */

			uintptr	vmstart;	/* base address for malloc */
			uintptr	vmunused;	/* 1st unused va   " " */
			uintptr	vmunmapped;	/* 1st unmapped va " " */
			uintptr	vmend;		/* 1st unusable va " " */

			PTE	*freepage;	/* next free PT in pts */

			uvlong	timesync;	/* crude time synchronisation */
			/* computed constants to avoid mult. & div. */
			uvlong	clintsperµs;
			uvlong	clintsperhz;	/* clint ticks per HZ */
			uvlong	nsthresh;  /* ipi: min ns to next clock intr */
			uvlong	minclints;	/* min. interval until intr */
			uvlong	epoch;		/* clint->mtime at boot */
			uint	ticks;		/* shared cpu0 m->ticks (type?) */

			int	nmach;		/* how many machs */
			int	nonline;	/* how many machs are online */
			uint	copymode;	/* 0 is COW, 1 is copy on ref */

			ulong	vendorid;	/* smp procs are identical */
			uintptr	archid;
			uintptr	extensions;	/* bits per CSR(MISA) */
			uvlong	cpucap;		/* capability bits for Tos */
			Cpu	*cpu;
			ushort	maxplicpri;
			ushort	nprivmodes;	/* 2 — 4 */

			Lock	kmsglock;	/* manually zero this lock */
		};
	};

	union {
		Mach*	machptr[MACHMAX];
		uchar	ptrpage[PGSZ];
	};

	PTE	initpt[Ptpgptes];	/* cpu0's first root page table */
	PTE	pts[EARLYPAGES*Ptpgptes]; /* see earlypagealloc */
	/* we stop zeroing here */

	union {
		uchar	sysextend[SYSEXTEND];	/* includes room for growth */
		struct {
			uchar	gap[SYSEXTEND - sizeof(Kmesg)];
			/* must be last so we can avoid zeroing it at start */
			Kmesg	kmsg;		/* do not zero */
		};
	};
};
CTASSERT(sizeof(Sys) <= MB, Sys_bigger_than_megabyte);
CTASSERT(sizeof(Syspercpu) == LOW0SYS, Syspercpu_c_as_offsets_differ);
CTASSERT(sizeof(Kmesg) == KMESGSIZE, Kmesg_wrong_size);

extern Sys* sys;
extern uintptr kernmem;			/* was used by KADDR, PADDR */
extern int mallocok;			/* flag: mallocinit() has been called */

/*
 * KMap
 */
typedef void KMap;
extern KMap* kmap(Page*);

#define kunmap(k)
#define VA(k)		(uintptr)(k)

struct
{
	Lock;
	ulong	machsmap[(MACHMAX+BI2WD-1)/BI2WD]; /* bitmap of active CPUs */
	char	exiting;		/* shutdown */
	char	ispanic;		/* shutdown in response to a panic */
	char	thunderbirdsarego; /* let added CPUs continue to schedinit */
	char	rebooting;		/* just idle cpus > 0 */
} active;

typedef void (*Pvfnv)(void);
typedef void (*Pvfns)(Sys *lowsys);

struct Rvarch {				/* this is likely to change */
	char	*name;
	void	(*machexten)(void);
	void	(*machstharts)(void);
	void	(*supermppaging)(Sys *lowsys);
//	void	(*reset)(void);
};

/*
 *  a parsed plan9.ini line
 *  only still used by pnp, uarts, ether.
 */
#define NISAOPT		8

struct ISAConf {		/* only used by usb & ether now */
	char	*type;
	uintptr	port;
	uintptr	mem;
	uintptr	size;
	/* irq is now in Intrcommon */
	ulong	dma;
	ulong	freq;

	int	nopt;
	char	*opt[NISAOPT];
};

/*
 * The Mach structures must be available via the per-processor
 * MMU information array machptr, mainly for disambiguation and access to
 * the clock which is only maintained by the bootstrap processor (0).
 */
extern register Mach* m;			/* R7 */
extern register Proc* up;			/* R6 */

/*
 * Horrid.
 */
#ifdef _DBGC_
#define DBGFLG		(dbgflg[_DBGC_])
#else
#define DBGFLG		0
#endif /* _DBGC_ */

#define DBG(...)	if(!DBGFLG){}else dbgprint(__VA_ARGS__)

extern char dbgflg[256];

#define dbgprint	print		/* for now */

#pragma	varargck	type	"L"	Mpl
#pragma	varargck	type	"P"	uintmem
#pragma	varargck	type	"R"	Mreg

/* acpi/multiboot memory types */
enum {
	AsmNONE		= 0,
	AsmMEMORY	= 1,
	AsmRESERVED	= 2,
	AsmDEV		= 5,		/* device registers */
};

enum {
	Wdogms = 200,			/* interval to restart watchdog */
};

struct Soc {
	/* physical addresses, vmapped to virtual addresses during start-up */
	uchar	*clint;		/* local intr ctlr also a timer */
	uchar	*plic;		/* external intr ctlr */
	uchar	*l2cache;
	uchar	*wdog0;		/* watchdog timer */
	uchar	*uart;		/* console */
	uchar	*ether[2];
	uchar	*pci;		/* ecam config space */
	uchar	*pcictl;	/* bridge & ctrl regs */
	uchar	*pciess;	/* soft reset, etc. */
	uchar	*sdmmc;
	uchar	*sdiosel;	/* for icicle */
	uchar	*kramend;	/* if non-0, max kernel ram end. */
				/* when kern addr spc for ram < ram size */

	/* hardware and firmware choices and bugs */
	uchar	dmaincoherent;	/* flag: el-cheapo hardware? */
	/*
	 * plic contexts are machine-dependent, but are usually hartid*2 +
	 * mode.  if there's a cut-down management hart at hartid 0 (i.e.,
	 * it's hobbled), they may instead be 1 + 2*(hartid-1) + mode, as
	 * on the icicle.
	 */
	uchar	hobbled; /* count of puny mgmt cpus with only 1 context (M) */
	uchar	sbilies;	/* flag: sbi lies about hart ids? */
	uchar	tinyemuuart;	/* flag: workaround tinyemu 8250 bug(s) */
	uchar	dwuart;		/* flag: workaround synopsys 8250 */
	uchar	c910;		/* flag: workaround xuantie c910 bugs? */

	/* software choices */
	uchar	idlewake;	/* flag: wake idling cpus with ipis */
	uchar	ipiclint;	/* flag: use clint to send IPIs */
	uchar	allintrs;	/* flag: enable all interrupts */
	uchar	poll;		/* flag: continual polling for i/o done */
};

Soc soc;

int	bootmachmode; /* flag: machine mode at boot? same on all non-hobbled harts? */
uvlong	cpuhz;		/* from kernel config */
int	early;		/* flag: not ready for traps yet */
int	gotipi;
int	hartcnt;
int	havesbihsm;
int	havesbisrst;
uintptr	memtotal;	/* sum of all banks */
uintptr	mideleg, medeleg;
uintptr	misa;
int	nosbi;
int	nuart;	/* from kernel config; number of uarts in use in uartregs */
uvlong	pagingmode; /* set from PAGINGMODE, not really selectable at run time */
int	probehartid;
int	probingharts;
void	(*prstackmax)(void);
void	(*rvreset)(void);
uvlong	timebase;	/* from kernel config */
vlong	uartfreq;	/* from kernel config */
uintptr	uartregs[]; /* from kernel config; not yet used for anything significant */

/* these are register layouts, so must not have bogus padding inserted */

/*
 * CLINT is Core-Local INTerrupt controller
 */

/* Clint arrays are indexed by hart (cpu) id */
struct Clint {
	/* non-zero lsb generates sw intr. unless SBI has neutered it */
	ulong	msip[4096];		/* aclint mswi */
	/* aclint mtimer */
	uvlong	mtimecmp[4095];		/* < mtime generates clock intr. */

	/*
	 * the XuanTie duplicates the above for supervisor mode access when the
	 * Clintee bit is set, which it is initially on the c910.
	 * also, mtime is missing and one has to instead read CSR(TIMELO).
	 */
	uvlong	mtime;

#ifdef C910
	/* S mode clint extension */
	/* aclint sswi */
	ulong	ssip[1024];		/* 4K long says the manual */
	uvlong	stimecmp[4095];		/* < mtime generates clock intr. */
#define mtimecmp	stimecmp	/* also renames Mach->mtimecmp */
#define msip		ssip
#endif
};

/*
 * PLIC is Platform-Level Interrupt Controller.
 * Any interrupt connected here also shows up as Seie in SIP CSR.
 */

enum {
	Ncontexts = 31*512,
};
enum Cpumodes {
	Machine, Super,		/* order matters, see plic contexts */
	Hyper, User,		/* these are optional */
};

typedef struct Plictxt Plictxt;
struct Plic {				/* 64 MB */
	union {
		struct {
			/* intr source index. WARL in priv-1.9 */
			ulong	prio[1024];
			ulong	pendbits[1024];	/* 1st 32: bitmap of sources */
			/* index by context, then 1st 6 are bitmap of sources */
			ulong	enabits[Ncontexts][32];
		};
		struct {
			uchar	_1_[2*MB - sizeof(ulong)];
			/*
			 * c910 has privilege control at 2MB-4,
			 * needed to allow S mode access
			 */
			ulong	c910privctl;
		};
	};
	struct Plictxt {		/* at +2MB */
		ulong	priothresh; /* (lower bound of accepted prios)-1 WARL */
		ulong	claimcompl;	/* claim/complete */
		uchar	_2_[4096 - 2*sizeof(ulong)];
	} context[Ncontexts];		/* index by context */
};

struct Ioconf {
	char	*name;		/* e.g., "ether" */
	uintptr	regssize;	/* e.g., PGSZ */
	uchar	**socaddr;	/* e.g., soc.ether array; va(s) stored here */
	short	irq;		/* 0 means none */
	ushort	baseunit;	/* e.g., 0 */
	ushort	consec;		/* e.g., 2; exception: 0 means 1 */
	uintptr	ioregs;		/* e.g., PAMac0 */
};

/* a read/write region of ram */
struct Membank {
	uintptr	addr;
	uintptr	size;
};
extern Membank membanks[];

struct Sbiret {
	uvlong	status;
	uvlong	error;
};

struct Intrstate {
	uintptr	osts;		/* mach mode state to restore */
	Mpl	pl;		/* super " " " " */
	uchar	machmode;	/* flag: cpu in machine mode? */
};

enum Riscv_vendorid {
	Vsifive	= 0x489,
	Vthead	= 0x5b7,
};
