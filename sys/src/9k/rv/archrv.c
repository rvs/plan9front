/*
 * risc-v rv64gc dependencies: clint, clock, ipis, harts, l2 cache, idling,
 *	delays, clz
 */
#include "u.h"
#include "../port/lib.h"
#include "mem.h"
#include "dat.h"
#include "fns.h"
#include "riscv.h"
#include "cpucap.h"

#define EXT(let) (1LL << ((let) - 'A'))
#define EXTIMA	(EXT('I') | EXT('M') | EXT('A'))
#define EXTFD	(EXT('F') | EXT('D'))
#define EXTSU	(EXT('S') | EXT('U'))

/* so far, only SBI lies, CSR(MHARTID) doesn't */
#define TRUSTHARTIDS	(nosbi || !soc.sbilies)
#define RDTIMES(tsc, mtime) coherence(); mtime = rdcltime(); /* ticks */ \
	tsc = rdtsc()			/* cycles */

enum {
	Clockdebug = 0,
#ifdef LONG_CLINT
	Clintlongs = 1,
#else
	Clintlongs = 0,
#endif
	/*
	 * sending ipis in idlewake() takes more system time (~11-21%) than
	 * not sending, but somewhat less elapsed time (~7-14%).  Increasing
	 * Minns tends to increase system time and decrease elapsed time.
	 * Perhaps this should be cycles instead of ns.
	 */
// TODO	Minns = 2500,		/* ns from now to avoid sending ipi */
//	Minns = 250,		/* ns from now to avoid sending ipi */
	Minns = 0,		/* ns from now to avoid sending ipi */

	Livedangerously = 0,	/* flag: don't reset when sbi lies */
	Reservedways = 1,	/* we need one way not enabled for cache */
};

struct Cpu {
	char	*name;
	ulong	vendorid;
	uvlong	archid;
};
typedef struct {
	char	havestate;
	schar	state;		/* sbi's notion of state; dubious */
	char	running;	/* set at start up; trustworthy */
} Hart;

/*
 * usual l2 cache control.  just set and forget on most systems.
 * from the sifive u74 core complex manual.
 */
typedef struct L2cache L2cache;
struct L2cache {
	union {
		uchar _0_[0x200];
		struct {
			/* le bytes: banks, ways, lg sets, lg cb bytes (ro) */
			ulong	config;
			ulong	_1_;
			ulong	wayenable; /* low byte: ways enabled - 1 */
		};
	};
	uvlong	flush64;	/* write phys addr of cache line to flush here */
};

typedef struct Pagingmode Pagingmode;
struct Pagingmode {
	uchar	bits;
	uchar	levels;
	uvlong	satpmode;
};

enum Sbihartsts {			/* results of sbihartstatus */
	Hstarted, Hstopped, Hstartpend, Hstoppend,
	Hsuspended, Hsuspendpend, Hresumepend,
};

char *hartstates[] = {
	"started", "stopped", "start_pending", "stop_pending",
	"suspended", "suspend_pending", "resume_pending",
};

extern int nrdy;			/* from proc.c */

int	portclz(Clzuint n);

uvlong pagingmode = PAGINGMODE;
int (*archclz)(Clzuint) = portclz;

static Hart harts[HARTMAX];

Cpu cpus[] = {
	{ "sifive e3/s5", Vsifive, 1, },			/* 3/5-series */
	{ "sifive u7",	Vsifive, 0x80000007u, },		/* 7-series */
	{ "sifive u7",	Vsifive, 0x8000000000000007ull, },	/* 7-series */
//	{ "xuantie",	0x401,	0, },
	{ "xuantie",	Vthead,	0, },
	{ "tinyemu",	0xbe11a5d, 0x564, },
	{ "zero vendor & arch (emulation?)", 0, 0, },
	0
};

Cpu *
ourcpu(void)
{
	Cpu *cpu;

	if (sys->cpu)
		return sys->cpu;
	for (cpu = cpus; cpu->name; cpu++)
		if (cpu->vendorid == sys->vendorid &&
		    cpu->archid == sys->archid)
			return sys->cpu = cpu;
	return nil;
}

static uint
umod(uint a, uint b)			/* unused */
{
	if (ISPOW2(b))
		return a & (b - 1);
	return a % b;
}

/* prevent interrupts in M or S mode */
void
nointrs(Intrstate *is)
{
	is->machmode = m->machmode;
	if (is->machmode) {
		is->osts = getmsts();
		putmsts(is->osts & ~Mie);
	} else
		is->pl = splhi();
}

void
restintrs(Intrstate *is)
{
	if (is->machmode)
		putmsts(is->osts);
	else
		splx(is->pl);
}

void
l2init(void)
{
	int enabways, maxways;
	L2cache *l2c = (L2cache *)soc.l2cache;

	if (l2c == nil)
		return;
	enabways = (l2c->wayenable & MASK(8)) + 1;
	maxways = (l2c->config>>8) & MASK(8);
	if (enabways < maxways - Reservedways) {
		iprint("%d of %d l2 cache ways enabled at entry; setting to %d\n",
			enabways, maxways, maxways - Reservedways);
		coherence();
		/* index of last way enabled for cache */
		l2c->wayenable = maxways - Reservedways - 1;
		coherence();
	}
}

/*
 * flush l2 cache from virtual address `vaddr' for `len' bytes.
 * "flush" means "write back the cache line if dirty, then invalidate it".
 * not needed on most (that is, risc-v conforming) systems.  only
 * pre-release beagle v needed it.
 */
void
l2cacheflush(void *vaddr, uintptr len)
{
	uintptr addr, last;
	L2cache *l2c = (L2cache *)soc.l2cache;

	coherence();
	if (l2c == nil || !soc.dmaincoherent)
		return;
	addr = PADDR(vaddr);
	last = (addr + len + CACHELINESZ-1) & ~((uintptr)CACHELINESZ-1);
	addr &= ~((uintptr)CACHELINESZ-1);	/* start of first cache line */
	for (; addr < last; addr += CACHELINESZ) {
		l2c->flush64 = addr;
		coherence();
	}
}

/* cpu-specific setup for this cpu */
void
cpuinit(int cpu)
{
	Hart *hartst;

	up = nil;
	usephysdevaddrs();		/* device vmaps not yet in effect */
	trapsclear();
	trapvecs();
	sys->machptr[cpu] = m;		/* publish for other cpus */
	aadd(&sys->nmach, 1);

	hartst = &harts[m->hartid];
	hartst->state = Hstarted;
	hartst->running = 1;
	coherence();
	hartst->havestate = 1;
	coherence();
	DBG("[cpu%d hart%d]\n", cpu, m->hartid);
	m->boottsc = rdtsc();

	m->plicctxt = mach2context(m);	/* base context without priv mode */
	clockoff();
}

/*
 * Clintlongs produces 32-bit little-endian accesses for unmodified tinyemu
 * (not ours) and xuantie.  Also implies reading the time csr instead of mtime.
 */

uvlong
rdcltime(void)
{
	if (!Clintlongs && nosbi)
		return m->clint->mtime;
	else
		return rdtime();		/* needs our tinyemu */
}

static void
setcltime(ulong *p, uvlong v)
{
	if (Clintlongs) {
		p[1] = VMASK(31);	/* don't trigger premature clock intr */
		p[0] = ~0ul;
		coherence();
		p[1] = v>>32;
		p[0] = v;
	} else
		*(uvlong *)p = v;
	coherence();
}

void
wrcltime(uvlong v)
{
	if (!soc.c910 && nosbi)	/* on c910, mtime is a csr, not memory-mapped */
		setcltime((ulong *)&m->clint->mtime, v);
}

uvlong
rdcltimecmp(Mach *mp)
{
	ulong *p;

	/* sbi provides no way to read mtimecmp, so see cached value */
	if (!nosbi)
		return mp->timecmp;

	/* seems not to work under OpenSBI, though undocumented */
	p = (ulong *)&m->clint->mtimecmp[mp->hartid];
	if (Clintlongs)
		return p[0] | (uvlong)p[1] << 32;
	else
		return *(uvlong *)p;
}

void
wrcltimecmp(uvlong v)
{
	if (m->clint == nil)
		panic("wrcltimecmp: nil m->clint");
	setcltime((ulong *)&m->clint->mtimecmp[m->hartid], v);
	m->timecmp = v;		/* remember for rdcltimecmp() under sbi */
}

void
setclinttmr(uvlong clticks)
{
	if (nosbi)
		wrcltimecmp(clticks);
	else
		sbisettimer(clticks);	/* how long does this take? */
	m->timecmp = clticks;	/* remember for rdcltimecmp() under sbi */
	coherence();		/* Stip might not be extinguished immediately */
}

static uvlong tscperclintglob;

/* are cycles for an interval consistent with clint ticks? */
static int
areclockswonky(uvlong difftsc, uvlong diffclint)
{
	vlong difftscperclint;

	if (diffclint == 0)
		diffclint = 1;
	difftscperclint = difftsc / diffclint;
	if (difftscperclint < tscperclintglob*2/3 ||
	    difftscperclint > tscperclintglob*3) {
		/* can't trust clocks to pop us out of wfi soon */
		if (Clockdebug)
			print("clocks are wonky or emulated.\n");
		return 1;
	}
	return 0;
}

static void
timeop(char *name, void (*op)(void))
{
	int i;
	uvlong tsc, mtime, tsc2, mtime2, diffsumtsc, diffsumclint;

	RDTIMES(tsc, mtime);
	for (i = 0; i < 1000; i++)
		op();
	RDTIMES(tsc2, mtime2);

	diffsumtsc = tsc2 - tsc;
	diffsumclint = mtime2 - mtime;

	if (Clockdebug)
		print("op %s:\t%d took %,lld tsc cycles and %,lld clint ticks\n",
			name, i, diffsumtsc, diffsumclint);
	USED(name);
	areclockswonky(diffsumtsc, diffsumclint);
}

static void
nullop(void)
{
}

static void
rdtimeop(void)
{
	vlong junk;

	junk = rdcltime();
	USED(junk);
}

static void
wrtimeop(void)
{
	if (nosbi)
		wrcltime(rdcltime());
}

static void
wrtimecmpop(void)
{
	if (nosbi)
		wrcltimecmp(VMASK(63));
}

/*
 * compute timing parameters, turn off WFI use if clocks are inconsistent.
 */
void
calibrate(void)
{
	vlong tsc, mtime, tsc2, mtime2, tscperclint, clintpertsc;

	if (sys->clintsperhz != 0)
		return;
	RDTIMES(tsc, mtime);
	delay(100);
	RDTIMES(tsc2, mtime2);

	clintpertsc = tscperclint = 1;
	mtime2 -= mtime;
	tsc2 -= tsc;
	if (mtime2 > 0) {
		tscperclint = tsc2 / mtime2;
		if (tscperclint < 1)
			tscperclint = 1;
	}
	if (tsc2 > 0) {
		clintpertsc = mtime2 / tsc2;
		if (clintpertsc < 1)
			clintpertsc = 1;
	}
	tscperclintglob = tscperclint;
	if (Clockdebug) {
		print("%,llud clint ticks is %,llud rdtsc cycs, or ",
			mtime2, tsc2);
		if (tscperclint > clintpertsc)
			print("%lld cycs/clint\n", tscperclint);
		else
			print("%lld clints/cyc\n", clintpertsc);
	}
	USED(clintpertsc);
	areclockswonky(tsc2, mtime2);

	if (Clockdebug)
		print("timebase given as %,llud Hz\n", timebase);

	/* compute constants for use by timerset & idle code */
	sys->clintsperhz = timebase / HZ;	/* clint ticks per HZ */
	sys->clintsperµs = timebase / 1000000;
	sys->nsthresh = (Minns * sys->clintsperµs) / 1000;

	/*
	/* min. interval until intr; was /(100*HZ) but made too many intrs.
	 * Does minclints need to be less than timebase/HZ?  It allows
	 * shorter and more precise sleep intervals, e.g., for clock0link
	 * polling.  To keep the interrupt load and interactive response
	 * manageable, it needs to be somewhat > 0.
	 */
	sys->minclints = timebase / (5*HZ);

	/* time various clock-related operations.  primarily debugging. */
	timeop("null", nullop);
	timeop("read time", rdtimeop);
	timeop("set time", wrtimeop);
	timeop("set timecmp", wrtimecmpop);
}

void
clockoff(void)
{
	clrstie();
	/* ~0ull makes sense, but looks negative on some machines */
	setclinttmr(VMASK(63));
}

/*
 *  set next timer interrupt for time next, in fastticks (clint ticks).
 *  we won't go longer than 1/HZ s. without a clock interrupt.
 *  as a special case, next==0 requests a small interval (e.g., 1/HZ s.).
 */
void
timerset(uvlong next)
{
	Mpl pl;
	vlong fticks;

	pl = splhi();
	if (sys->clintsperhz == 0)
		panic("timerset: sys->clintsperhz not yet set");
	if (next == 0)
		fticks = sys->clintsperhz;
	else {
		fticks = next - fastticks(nil);
		/* enforce sane bounds: 1/(5*HZ) ≤ s. ≤ 1/HZ */
		if (fticks < (vlong)sys->minclints)
			/* don't interrupt immediately */
			fticks = sys->minclints;
		else if (fticks > (vlong)sys->clintsperhz)
			fticks = sys->clintsperhz;
	}

	/* extinguish current intr source and set new deadline */
	setclinttmr(rdcltime() + fticks);
	clrsipbit(Stie);		/* dismiss current intr */
	clockenable();
	splx(pl);
}

long ticktock;				/* set by M clock intr */

int
clocksanity(void)
{
	int i;
	uvlong omtime;

	assert(timebase >= 1000*1000);
	clockoff();
	m->clockintrsok = 1;

	omtime = rdcltime();
	clockenable();
	setclinttmr(omtime + 20*(timebase/1000000));

	delay(1);
	if (rdcltime() <= omtime)
		panic("clint clock is not advancing");

	for (i = 100; (getsip() & (Mtie|Stie)) == 0 && i > 0; i--)
		delay(1);
	if ((getsip() & (Mtie|Stie)) == 0 && ticktock == 0)
		panic("clint clock not interrupting");
	timerset(0);

	if (m->machno == 0)
		if (timebase >= 1000000 && timebase % 1000000 == 0)
			print("clint timebase: %,lld MHz\n", timebase/1000000);
		else
			print("clint timebase: %,lld Hz\n", timebase);
	return 1;
}

static int mwords[1];
static int *mwaitwd = mwords;		/* make safe from the start */

void countipi(void);

void
clintipitohart(int hart)
{
	m->clint->msip[hart] = 1;
}

int
ipitohart(int hart)
{
	uvlong hartbm;

	if (nosbi || soc.ipiclint) {
		clintipitohart(hart);
		return 0;
	} else {			/* do it the slow and stupid way */
		hartbm = 1ull << hart;
		return sbisendipi(&hartbm) == 0? 0: -1;
	}
}

/*
 * wait for an interrupt, which conserves power, thus heat.
 * an interrupt will resume after WFI.  assume
 * individual interrupt bits of interest are set in SIE.
 * currently on risc-v, we can only wait for an interrupt.
 * see the privileged ISA spec for WFI; it's a bit subtle.
 * in particular, WFI may pause the core's cycle counter,
 * and it can be implemented as a NOP.
 *
 * note that halt, ainc, adec, and the spl* functions contain fences.
 */
static ulong
idlenowakehands(void)
{
	ulong ip;

	while (((ip = getsip()) & Superie) == 0)
		halt();
	/* can't call intrclknotrap: needs non-nil Ureg* */
	if (ip & Seie)
		intrnotrap();
	return ip;
}

static void
idlewakehands(void)
{
	ulong ip;

	/*
	 * ask for an ipi from another cpu if work becomes possibly
	 * available.  if no other devices or processors interrupt,
	 * the clock will.
	 */
	amoswapw(&m->ipiwait, 1);		/* avoid coherence call */
	ip = idlenowakehands();

	/*
	 * some interrupt popped us out of wfi at splhi, if we were in
	 * wfi.  if we popped out of wfi due to an ipi, clear it.  as
	 * a side-effect, if we waited in wfi, the ipi won't have been
	 * automatically counted for irqalloc since it wasn't serviced,
	 * but ipiwait will have been zeroed when sending the ipi.
	 */
	if (amoswapw(&m->ipiwait, 0) == 0)
		countipi();			/* were still waiting */
	if (ip & (Ssie|Msie)) {
		clearipi();
		gotipi = 1;
	}
}

/*
 *  put the processor in the halt state if we've no processes to run.
 *  an interrupt will get us going again.  the clock interrupt every 1/HZ s.
 *  puts an upper bound on the wait time.
 *  called at spllo from proc.c/runproc, but also in infinite loops elsewhere.
 */
void
idlehands(void)
{
	Mpl pl;

	if (nrdy != 0 || active.exiting || active.rebooting)
		return;
	pl = splhi();
	if (soc.idlewake && sys->nonline > 1)
		idlewakehands();
	else
		idlenowakehands();
	splx(pl);
}

#define WRAPCPU(cpu, nonline) ((cpu) >= (nonline)? 0: (cpu))

/*
 * wake some cpus in wfi state.  an interrupt will make one cpu break
 * out of wfi, but a qunlock probably should wake any idlers too.
 *
 * idlewake is called from the locking primitives, among others, so we
 * can't use them within it.  however, note that on riscv64, ainc and adec
 * use atomic memory operations, not locks.
 *
 * try to spread the load around fairly, taking into account that there may
 * be other cpus also running idlewake.  assume there's no point in waking
 * more cpus than currently-runnable processes.
 * only interrupt cpus advertising that they are waiting for an ipi.
 *
 * if we use sbi, this currently only works for first 64 cpus.  by the time
 * we have that many cpus, we should have the sbi HSM functions available.
 */
void
idlewake(void)
{
	int left;
	uint cpu, startcpu, nonline;
	uvlong hartbm, imminent;
	Mach *mp;
	Mach **machptr;
	static int waking;		/* lock with _tas */
	static uint currcpu;

	if (0)				/* we don't emulate x86 mwait *.
		++*mwaitwd;		/* need not be atomic increment */
	if (!soc.idlewake || nrdy == 0 || sys->nonline <= 1 ||
	    TAS(&waking) != 0)
		return;			/* another cpu is waking */

	/*
	 * we can run one runnable process on this cpu, so wake up
	 * enough waiting cpus to run the others, if available.
	 * this is somewhat approximate as the other cpus may change
	 * state underfoot.
	 */
	imminent = rdcltime() + sys->nsthresh;
	nonline = sys->nonline;
	cpu = currcpu;
	startcpu = cpu = WRAPCPU(cpu, nonline);

	left = nrdy;
	if (left == 0)
		left = 1;
	if (left >= nonline)
		left = nonline - 1;

	/*
	 * if a cpu is awaiting an ipi (or other interrupt) and doesn't
	 * have an imminent clock interrupt, wake it.
	 * assume on-line cpus are 0 — nonline-1.
	 */
	hartbm = 0;
	machptr = sys->machptr;
	while (left > 0) {
		mp = machptr[cpu++];
		cpu = WRAPCPU(cpu, nonline);
		if (mp != nil && mp->ipiwait && rdcltimecmp(mp) > imminent &&
		    amoswapw(&mp->ipiwait, 0) != 0) {
			/* next clock intr not soon & no ipi yet sent */
			if (nosbi)
				clintipitohart(mp->hartid);
			else
				hartbm |= 1ull << mp->hartid;
			left--;
			currcpu = cpu;
		}
		if (cpu == startcpu)		/* wrapped around? */
			break;
	}
	if (hartbm)				/* only set if using sbi */
		sbisendipi(&hartbm);		/* wake them all at once */
	waking = 0;
}

static int
pollipis(void)
{
	int myhart;

	myhart = -1;
	if (TRUSTHARTIDS || !probingharts)
		return myhart;
	if (getsip() & Ssie || m->clint->msip[m->hartid]) {
		myhart = probehartid;
		if (myhart == m->hartid)
			iprint("cpu%d: ipi, hart id %d is correct\n",
				m->machno, m->hartid);
		else {
			iprint("cpu%d: ipi, old hart id %d, actual %d\n",
				m->machno, m->hartid, myhart);
			m->hartid = myhart;
		}
		clrsipbit(Ssie);
		gotipi = 1;
		coherence();
	}
	return myhart;
}

int
pollsechartids(void)
{
	int myhart;

	myhart = -1;
	if (TRUSTHARTIDS)
		return myhart;
	setsie(Ssie);
	while (!probingharts)
		pause();
	clrsipbit(Ssie);

	/* thunderbirdsarego set on cpu0 in main before schedinit (3) */
	while (!active.thunderbirdsarego || probingharts /* || myhart < 0 */) {
		delay(1);
		myhart = pollipis();
	}
	clrsipbit(Ssie);
	return myhart;
}

/*
 * map cpu->hartid via ipis (in case sbi lies or we're stuck with uefi):
 * send an ipi to each non-hobbled hart, wait a short time;
 * upon receipt of an ipi, note m->machno and current hartid.
 */
void
probeallhartids(void)
{
	if (TRUSTHARTIDS)
		return;
	probingharts = 1;
	coherence();
	delay(500);
	iprint("sbi sometimes lies, probing harts for ids..");
	for (probehartid = soc.hobbled; probehartid <= hartcnt + HARTSLOP;
	     probehartid++){
		coherence();
		iprint(".");
		ipitohart(probehartid);
		delay(500);		/* let target cpu respond */
		pollipis();		/* see any ipi to ourselves */
	}
	probingharts = 0;
	if (!gotipi)
		iprint("IPIs not working!");
	iprint("\n");
}

static vlong
gethartstate(int hart)
{
	vlong state;
	Hart *hartst;

	/*
	 * BUG? SBI sometimes reports one or two incorrect, random
	 * hart(s) as started.  Could just start them all; will that
	 * work if status is wrong?  It would be good not to have
	 * renegade harts running random code, yet we currently
	 * have no way to suspend them.
	 */
	state = sbihartstatus(hart);
	if (state >= 0) {
		hartst = &harts[hart];
		hartst->state = state;
		hartst->havestate = 1;
	}
	return state;
}

enum {
	Hartcols = 3,
};

static void
getallhartstates(void)
{
	int hart;

	for (hart = 0; hart < HARTMAX; hart++)
		gethartstate(hart);
}

static void
startagain(void)
{
	iprint("resetting the system.\n");
	delay(5);
	/*
	 * we are cpu0 and unsure of the states of other cores,
	 * but a system reset will fix that.
	 */
	archreset();
	notreached();
}

static int
stophart(int hart, int state)
{
	/*
	 * hart is probably started but not running our code, at least
	 * not at our start address.  sbi or u-boot bug?
	 * if only there were a way to reset a single hart...
	 */
	iprint("hart %d in state %s on sbi hsm system; not sending it an IPI.\n",
		hart, hartstates[state]);
	if (!Livedangerously)
		/* there's really nothing we can do but start over and hope. */
		startagain();

	state = gethartstate(hart);
	if (state < 0)
		iprint("can't get hart %d status\n", hart);
	else if (state != Hstopped) {
		iprint("can't stop hart %d in state %s\n",
			hart, hartstates[state]);
		return -1;
	}
	return state;
}

/*
 * start any stopped cpus at _main.  some or all may already be started,
 * but we are going to make the unwarranted assumption that systems
 * with SBI HSM calls only start one cpu initially (the only other
 * legitimate configuration is to start them all at once).
 * if sbi started multiple harts, what address would it start them at?
 * stopped cpus will not be waiting but need to be started.
 *
 * simulate the non-hsm near-simultaneous start up of all harts
 * except 0 if it's hobbled (for running and managing the others).
 */
void
hsmstartall(void)
{
	int hart, state, col;
	Hart *hartst;

	getallhartstates();

	/* print incorrect sbi states, ensure other harts are stopped */
	for (hart = 0; hart < HARTMAX; hart++) {
		hartst = &harts[hart];
		state = (hartst->havestate? hartst->state: -1);
		if (state < 0)
			continue;
		/* cpu0's sbi hart state doesn't matter; we don't start it. */
		if (hart != m->hartid &&
		    (state == Hstarted) != hartst->running) {
			iprint("sbi status for hart %d (%s) is wrong; "
				"it's %srunning\n", hart, hartstates[state],
				hartst->running? "": "not ");
			if (!Livedangerously)
				startagain();
		}
	}

	/* start any stopped harts, and they should all be stopped except me */
	col = 0;
	for (hart = soc.hobbled? 1: 0; hart < HARTMAX; hart++) {
		if (hart == m->hartid)
			continue;
		hartst = &harts[hart];
		if (hartst->running)		/* trust hartstate over sbi */
			continue;

		/* we think hart is not running; does sbi agree? */
		state = (hartst->havestate? hartst->state: -1);
		if (state < 0)
			continue;
		if (state != Hstopped && state != Hsuspended)
			if (stophart(hart, state) < 0)
				continue;

		iprint("sbi starting hart %d%s", hart,
			col++ % Hartcols == Hartcols-1? "\n": "\t");
		sbihartstart(hart, PADDR(_main), hart);
		/* give time to start & bump hartcnt; stagger startups */
		delay(250);
	}
	if (col % Hartcols != 0)
		iprint("\n");
}

static ulong
prifexts(ulong isa, ulong exts, char *name)
{
	if ((isa & exts) == exts) {
		print(name);
		isa &= ~exts;
	}
	return isa;
}

vlong
archhz(void)
{
	if(m->machno != 0)
		return sys->machptr[0]->cpuhz; /* cpus have to be ~identical */
	l2init();		/* meanwhile, other cpus are spinning */
	return cpuhz;
}

int
haveinstr(void *fn, uintptr arg)
{
	int failed;

	failed = 1;
	probeinstr(fn, arg, &failed);
	return !failed;
}

static void
prcpuchar(void)
{
	print("hart %d plic M context %d", m->hartid, m->plicctxt);
}

static void
prcpuhartctxt(void)
{
#ifdef unused
	static Lock idlock;

	ilock(&idlock);
	prflush();
	print("cpu%d: ", m->machno);
	prcpuchar();
	print("\n");
	prflush();
	iunlock(&idlock);
#endif
}

void
cpuidprint(void)
{
	int bit;
	ulong isa, misa;	/* misa extension bits fit in low long */
	Cpu *cpu;

	if(m->machno != 0) {
		prcpuhartctxt();
		return;			/* cpus have to be ~identical */
	}

	print("cpu%d: risc-v RV%d", m->machno, BI2BY*(int)sizeof(uintptr));
	if (bootmachmode) {
		misa = sys->extensions;
		isa = prifexts(misa, EXTIMA|EXTFD, "G");
		isa = prifexts(isa, EXTIMA, "IMA");
		isa = prifexts(isa, EXTFD, "FD");
		isa = prifexts(isa, EXT('C'), "C");
		isa &= MASK('z'+1-'a');		/* isolate extensions */
		for (bit = 0; bit < 'z'+1-'a'; bit++)
			if (isa & (1<<bit))
				print("%c", 'A' + bit);
		if ((misa & (EXTIMA|EXTFD)) != (EXTIMA|EXTFD))
			panic("cpu is not RV64G: misa %#lux", misa);
		if ((misa & EXTSU) != EXTSU)
			panic("don't have super & user modes; hopeless.");
		if ((misa & EXT('C')) == 0)
			print(" no compression, many binaries won't run.\n");
	} else
		print("GC");				/* can't easily tell */
	if (haveinstr(clzzbb, 0)) {
		print("Zbb");
		archclz = clzzbb;
		sys->cpucap |= Capclz;
	}
	if (haveinstr(sync_is, 0))
		print(" sync_is");

	sys->cpu = cpu = ourcpu();
	if (cpu)
		print(" %s", cpu->name);
	else {
		print(" unknown make");
		if (sys->vendorid != 0 || sys->archid != 0)
			print(" vendor %#lux arch %#p",
				sys->vendorid, sys->archid);
	}
	print(" ");
	prcpuchar();
	print(" at %d MHz\n", (uint)(cpuhz / (1000*1000)));
}

static void
addpgsz(int lg2)
{
	int npg;

	assert(m->npgsz < NPGSZ);
	npg = m->npgsz++;
	m->pgszlg2[npg] = lg2;
	m->pgszmask[npg] = VMASK(lg2);
}

int
archmmu(void)
{
	int lvl;

	if (m->npgsz)
		return Npglvls;
	CTASSERT(PGSZ == 4*KB, PGSZ_4K);
	for (lvl = 0; lvl < Npglvls; lvl++)
		addpgsz(PGLSHFT(lvl));
	return Npglvls;
}

static int
fmtP(Fmt* f)
{
	uintmem pa;

	pa = va_arg(f->args, uintmem);

	if(f->flags & FmtSharp)
		return fmtprint(f, "%#16.16p", pa);

	return fmtprint(f, "%llud", pa);
}

static int
fmtL(Fmt* f)
{
	return fmtprint(f, "%#16.16p", va_arg(f->args, Mpl));
}

static int
fmtR(Fmt* f)
{
	return fmtprint(f, "%#16.16p", va_arg(f->args, Mreg));
}

void
archfmtinstall(void)
{
	/*
	 * Architecture-specific formatting. Not as neat as they
	 * could be (e.g. there's no defined type for a 'register':
	 *	L - Mpl, mach priority level
	 *	P - uintmem, physical address
	 *	R - register
	 * With a little effort these routines could be written
	 * in a fairly architecturally-independent manner, relying
	 * on the compiler to optimise-away impossible conditions,
	 * and/or by exploiting the innards of the fmt library.
	 */
	fmtinstall('P', fmtP);

	fmtinstall('L', fmtL);
	fmtinstall('R', fmtR);
}

/* delay for at least microsecs, placating the watchdog if necessary */
void
microdelay(vlong microsecs)
{
	uvlong t, now, nxtdog, mhz, dogincr;

	if (microsecs <= 0)
		return;
	mhz = (m? m->cpumhz: 1000);
	if (mhz > 10000)		/* sanity */
		mhz = 1000;
	now = rdtsc();
	t = now + microsecs*mhz;
	dogincr = Wdogms * 1000LL * mhz;
	/* when islo(), clock interrupts will restart the dog */
	if (watchdogon && watchdog && m->machno == 0 && !islo()) {
		nxtdog = 0;
		for (now = rdtsc(); now < t; now = rdtsc()) {
			if (now > nxtdog) {
				watchdog->restart();
				nxtdog = now + dogincr;
			}
			pause();
		}
	} else
		while (rdtsc() < t)
			pause();
}

void
millidelay(int millisecs)
{
	if (millisecs)
		microdelay(1000LL * millisecs);
}

/* the Zbb extension provides a CLZ instruction */
int
portclz(Clzuint n)			/* count leading zeroes */
{
	/* (u)vlong makes jc generate better code than (u)int */
	uvlong cnt, hibits;
	Clzuint mask;

	if (n == 0)
		return Clzbits;
	cnt = 0;
	mask = VMASK(Clzbits/2) << (Clzbits/2);
	/* this will take at most log2(Clzbits) iterations */
	for (hibits = Clzbits/2; hibits > 0; ) {
		if ((n & mask) == 0) {
			/* highest bits are zero; count and toss them */
			cnt += hibits;
			n <<= hibits;
		}
		/* halve mask width for next iteration */
		hibits /= 2;
		mask <<= hibits;
	}
	return cnt;
}

int
clz(Clzuint n)
{
	return (*archclz)(n);
}
