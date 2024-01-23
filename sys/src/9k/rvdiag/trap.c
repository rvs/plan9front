/*
 * traps, faults, interrupts, system call entry
 */
#include	"u.h"
#include	"../port/lib.h"
#include	"mem.h"
#include	"dat.h"
#include	"fns.h"
#include	"../port/error.h"

#include	"/sys/src/libc/9syscall/sys.h"	/* for EXEC w Trapdebug only */
#include	<tos.h>
#include	"ureg.h"
#include	"riscv.h"
#include	"io.h"

/* instruction decoding */
#define UNCOMPINST(pc)	(*(ushort *)(pc) | *(ushort *)((pc) + 2) << 16)
#define BASEOP(inst)	((inst) & MASK(7))

enum {
	Trapdebug = 0,
	Probedebug = 0,
	Intrdebug = 0,

	Ntimevec = 20,		/* number of time buckets for each intr */
	Ncauses = Ngintr + Nlintr + Nexc,
};
enum Faultypes {
	Unknownflt, Exception, Localintr, Globalintr, Nfaulttypes,
};

/* syscall stack frame (on riscv at least); used for notes */
typedef struct NFrame NFrame;
struct NFrame
{
	uintptr	pc;
	Ureg*	arg0;
	char*	arg1;
	char	msg[ERRMAX];
	Ureg*	old;
	Ureg	ureg;
};

typedef struct {
	uchar	cause;
	uchar	user;
	uchar	type;	/* exception or interrupt code.  ushort some day? */
	short	vno;
} Cause;
typedef int (*Traphandler)(Ureg *, Cause *);
typedef void (*Exchandler)(Ureg *, Cause *);

static void debugbpt(Ureg*, Cause*);
static void dumpgpr(Ureg* ureg);
static void faultriscv64(Ureg*, Cause *);

static Lock vctllock;
static Vctl *vctl[Ncauses];
static Vctl *pollvecs;
static ulong trapcnt[Ncauses];

ulong	intrtimes[Ncauses][Ntimevec];
ulong	mchexcepts;			/* M fault count */

int	intr(Ureg* ureg, Cause *cp);

static void trapsyscall(Ureg *ureg, Cause *);

/* base plic context for mach (M mode). dependent upon system configuration */
uint
mach2context(Mach *mach)
{
	if (mach->hartid < soc.hobbled)		/* hobbled mgmt hart? */
		return mach->hartid;
	return soc.hobbled + sys->nprivmodes*(mach->hartid - soc.hobbled);
}

/* this is a potential constant expression */
#define vctlidx(intrno, type) ((type) == Globalintr? (intrno): \
	(type) == Localintr? Ngintr+(intrno): \
	(type) == Exception? Ngintr+Nlintr+(intrno): -1)

static uint
intrtype(uint vno)
{
	if (vno < Ngintr)
		return Globalintr;
	else if (vno < Ngintr + Nlintr)
		return Localintr;
	else if (vno < Ngintr + Nlintr + Nexc)
		return Exception;
	else {
		panic("intrtype %d: unknown type, called from %#p",
			vno, getcallerpc(&vno));
		return Unknownflt;
	}
}

Vctl *
newvec(void (*f)(Ureg*, void*), void* a, int tbdf, char *name)
{
	Vctl *v;

	v = malloc(sizeof(Vctl));
	v->tbdf = tbdf;
	v->f = f;
	v->a = a;
	v->cpu = -1;			/* could be any cpu until overridden */
	strncpy(v->name, name, KNAMELEN);
	v->name[KNAMELEN-1] = 0;
	return v;
}

void
trapsclear(void)
{
	clockoff();
	coherence();
	clearipi();
}

void
trapvecs(void)
{
	putsscratch((uintptr)m);	/* high virtual */
	putstvec(strap);		/* " */
	setsie(Superie);		/* individual enables; still splhi */
}

/* map cpu (machno) to plic context for Super mode */
int
cpu2context(uint cpu)
{
	Mach *mach;

	if (cpu >= MACHMAX)
		return -1;
	mach = sys->machptr[cpu];
	if (mach == nil || !mach->online)
		return -1;
	return mach->plicctxt + Super;
}

enum {
	Nopri,
	Lowpri,
	Highpri = 1,		/* empirically, 7 on icicle */
};

/* probe WARL register by writing wr to it, reading and restoring */
ulong
warl(ulong *addr, ulong wr)
{
	ulong old, new;

	old = *addr;		/* save current value */
	*addr = wr;		/* probe WARL register */
	coherence();

	new = *addr;		/* read legal value back */
	*addr = old;		/* restore */
	coherence();
	return new;
}

void
plicinit(void)
{
	uint irq, maxpri, newpri;
	Mpl s;
	Plic *plic = (Plic *)soc.plic;

	if (plic == nil)
		return;
	if (probeulong((ulong *)plic, Read) < 0) {
		prf("no response from plic at %#p\n", plic);
		soc.plic = 0;
		return;
	}
	maxpri = warl(&plic->context[cpu2context(0)].priothresh, ~0);
	if (maxpri == 0) {
		s = splhi();
		/* interrupt id 0 is "no interrupt" */
		for (irq = 1; irq < Ngintr && maxpri == 0; irq++) {
			newpri = warl(&plic->prio[irq], ~0);
			if (newpri > maxpri)
				maxpri = newpri;
		}
		splx(s);
	}
	if (maxpri == 0) {
//		maxpri = Lowpri;
		plicoff();
		print("plic at %#p: max priority 0, ignoring plic\n", plic);
		soc.plic = 0;
		return;
	}
	sys->maxplicpri = maxpri;
	print("plic at %#p: max prio %#ux\n", plic, maxpri);

	/* don't leave any irqs enabled */
	/* interrupt id 0 is "no interrupt" */
	for (irq = 1; irq < Ngintr; irq++)
		plicdisable(irq);
}

/* enabits[ctxt][wd] & bit corresponds to the irq & cpu context in question */
static void
plicenactxtirq(Plic *plic, int cpu, uint wd, uint bit)
{
	int ctxt;

	ctxt = cpu2context(cpu);
	if (ctxt >= 0 && (plic->enabits[ctxt][wd] & bit) == 0) {
		if (Intrdebug)
			iprint("plic enabling context %d wd %d bit %#ux\n",
				ctxt, wd, bit);
		/* PL: allow all intrs of all priorities on this cpu */
		plic->context[ctxt].priothresh = Nopri;
		coherence();
		amoorw(&plic->enabits[ctxt][wd], bit);
	}
}

static void
plicdisctxtirq(Plic *plic, int cpu, uint wd, uint bit)
{
	int ctxt;

	ctxt = cpu2context(cpu);
	if (ctxt >= 0)
		amonandw(&plic->enabits[ctxt][wd], bit);
		/* leave plic->context[ctxt].priothresh alone */
}

/*
 * we enabled the interrupt in all S contexts, and let the fastest cpu
 * service it.  routing them all to cpu0 seems to be about the same speed.
 */
int
plicenable(uint irq)
{
	int cpu, wd, bit;
	Mpl s;
	Plic *plic = (Plic *)soc.plic;

	if (plic == nil || irq >= Ngintr)
		return -1;
	wd = BITMAPWD(irq);
	bit = BITMAPBIT(irq);
	drainuart();			/* in case we are enabling the uart */
	s = splhi();
	/* sys->nonline will often be 1 */
	cpu = 0;
	plicenactxtirq(plic, 0, wd, bit);
	coherence();
	plic->prio[irq] = sys->maxplicpri? sys->maxplicpri: Highpri;
	splx(s);
	if (Trapdebug || Intrdebug)
		iprint("plic enabled for irq %d at prio %ld for cpu %d\n",
			irq, plic->prio[irq], cpu);
	USED(cpu);
	return 0;
}

/* enable or disable all global interrupts on secondary cpus */
void
secintrs(int on)
{
	int irq, cpu, wd;
	uint bit;
	Mpl s;
	Plic *plic = (Plic *)soc.plic;

	if (plic == nil)
		return;
	if (Intrdebug)
		iprint("secintrs on=%d\n", on);
	s = splhi();
	/* interrupt id 0 is "no interrupt" */
	for (irq = 1; irq < Ngintr; irq++) {
		if (plic->prio[irq] == Nopri)
			continue;
		wd = BITMAPWD(irq);
		bit = BITMAPBIT(irq);
		for (cpu = 1; cpu < sys->nonline; cpu++)
			(on? plicenactxtirq: plicdisctxtirq)(plic, cpu, wd, bit);
	}
	splx(s);
}

void
intrall(void)
{
	secintrs(1);
}

void
intrcpu0(void)
{
	secintrs(0);
}

void
plicdisable(uint irq)
{
	int cpu, wd, bit;
	Plic *plic = (Plic *)soc.plic;

	if (plic == nil || irq >= Ngintr)
		return;
	plic->prio[irq] = Nopri;
	wd = BITMAPWD(irq);
	bit = BITMAPBIT(irq);
	for (cpu = 0; cpu < sys->nonline; cpu++)
		plicdisctxtirq(plic, cpu, wd, bit);
}

void
plicnopend(uint irq)			/* unused */
{
	Plic *plic = (Plic *)soc.plic;

	if (plic == nil || irq >= Ngintr)
		return;
	amonandw(&plic->pendbits[BITMAPWD(irq)], BITMAPBIT(irq));
}

void
plicoff(void)
{
	int irq;
	Plic *plic = (Plic *)soc.plic;

	if (plic == nil)
		return;
	if (Intrdebug)
		iprint("plic off\n");
	/* interrupt id 0 is "no interrupt" */
	for (irq = Ngintr-1; irq > 0; irq--)
		plic->prio[irq] = Nopri;	/* don't interrupt on irq */
}

/* returns interrupt id (irq); 0 is none */
uint
plicclaim(uint ctxt)
{
	Plic *plic = (Plic *)soc.plic;

	return (plic? plic->context[ctxt].claimcompl: 0);
}

void
pliccompl(uint irq, uint ctxt)
{
	Plic *plic = (Plic *)soc.plic;

	if (plic)
		plic->context[ctxt].claimcompl = irq;
}

void
intrenableall(void)
{
	int avno;
	static int first = 1;

	/* first time, turn them all on and see what interrupts */
	if (first) {
		first = 0;
		iprint("enabling all global interrupts...");
		for (avno = 0; avno < nelem(vctl); avno++)
			if(intrtype(avno) == Globalintr)
				plicenable(avno);
		iprint("\n");
	}
}

/* old 9k interface */
void*
intrenable(int irq, void (*f)(Ureg*, void*), void* a, int tbdf, char *name)
{
	int vno;
	Vctl *v;

	if(f == nil){
		print("intrenable: nil handler for %d, tbdf %#ux for %s\n",
			irq, tbdf, name);
		return nil;
	}
	if (irq >= Ngintr) {
		print("intrenable: irq %d >= Ngintr (%d)\n", irq, Ngintr);
		return nil;
	}

	vno = vctlidx(irq, Globalintr);
	if(vno == -1){
		print("intrenable: couldn't enable irq %d, tbdf %#ux for %s\n",
			irq, tbdf, name);
		return nil;
	}
	if((uint)vno >= nelem(vctl))
		panic("intrenable: vector %d out of range", vno);
	ilock(&vctllock);
	if(vctl[vno])
		panic("intrenable: vector %d for %s already allocated by %s",
			vno, name, vctl[vno]->name);
	plicenable(irq);
	if (soc.allintrs)
		intrenableall();

	/* we assert that vectors are unshared, though irqs may be */
	v = newvec(f, a, tbdf, name);
	v->isintr = 1;
	v->irq = irq;
	v->vno = vno;
	v->type = Globalintr;
	vctl[vno] = v;
	v->pollnxt = pollvecs;		/* add to poll chain */
	pollvecs = v;
	iunlock(&vctllock);
	if (Trapdebug || Intrdebug)
		iprint("intrenable %s irq %d vector %d for cpu%d\n",
			name, irq, vno, m->machno);

	/*
	 * Return the assigned vector so intrdisable can find
	 * the handler.
	 */
	return v;
}

/* new, portable interface (between 9 and 9k) */
int
enableintr(Intrcommon *ic, Intrsvcret (*f)(Ureg*, void*), void *ctlr, char *name)
{
	ic->vector = intrenable(ic->irq, f, ctlr,
		(ic->pcidev? ic->pcidev->tbdf: BUSUNKNOWN), name);
	if (ic->vector)
		ic->intrenabled = 1;
	return ic->vector? 0: -1;
}

int
intrdisable(void* vector)
{
	Vctl *v;

	ilock(&vctllock);
	v = vector;
	if(v == nil || vctl[v->vno] != v)
		panic("intrdisable: v %#p", v);
	plicdisable(v->vno);
	vctl[v->vno] = nil;
	iunlock(&vctllock);
//	free(v);		/* can't free while poll chain exists */
	return 0;
}

/* new, portable interface (between 9 and 9k) */
int
disableintr(Intrcommon *ic, Intrsvcret (*)(Ureg*, void*), void *, char *)
{
	if (ic->vector == nil || ic->intrenabled == 0)
		return 0;
	if (intrdisable(ic->vector) < 0)
		return -1;
	ic->intrenabled = 0;
	ic->vector = nil;
	return 0;
}

static char *
vctlseprint(char *s, char *e, Vctl *v, int vno)
{
	s = seprint(s, e, "%3d %3d %10llud %3s %.*s", vno, v->irq, v->count,
		v->ismsi? "msi": "- ", KNAMELEN, v->name);
	if (v->unclaimed || v->intrunknown)
		s = seprint(s, e, " unclaimed %lud unknown %lud", v->unclaimed,
			v->intrunknown);
	if(v->cpu >= 0)
		s = seprint(s, e, " cpu%d", v->cpu);
	return seprint(s, e, "\n");
}

static long
irqallocread(Chan*, void *vbuf, long n, vlong offset)
{
	char *buf, *p, *e, str[90];
	int ns, vno;
	long oldn;
	Vctl *v;

	if(n < 0 || offset < 0)
		error(Ebadarg);

	oldn = n;
	buf = vbuf;
	e = str + sizeof str;
	for(vno=0; vno<nelem(vctl); vno++)
		for(v=vctl[vno]; v; v=v->next){
			/* v is a trap, not yet seen?  adjust to taste */
			if (v->isintr == 0 && v->count == 0)
				continue;
			ns = vctlseprint(str, e, v, vno) - str;
			if(ns <= offset) { /* if do not want this, skip entry */
				offset -= ns;
				continue;
			}
			/* skip offset bytes */
			ns -= offset;
			p = str+offset;
			offset = 0;

			/* write at most min(n,ns) bytes */
			if(ns > n)
				ns = n;
			memmove(buf, p, ns);
			n -= ns;
			buf += ns;

			if(n == 0)
				return oldn;
		}
	return oldn - n;
}

void
trapenable(int vno, void (*f)(Ureg*, void*), void* a, char *name)
{
	Vctl *v;

	if((uint)vno >= nelem(vctl))
		panic("trapenable: vector %d too big", vno);
	v = newvec(f, a, BUSUNKNOWN, name);
	v->isintr = 0;

	ilock(&vctllock);
	v->next = vctl[vno];
	v->type = Exception;
	vctl[vno] = v;
	iunlock(&vctllock);
}

void
zerotrapcnts(void)
{
	memset(trapcnt, 0, sizeof trapcnt);
}

void
enablezerotrapcnts(void)
{
	if (Trapdebug)
		addclock0link(zerotrapcnts, 1000);
}

static void
vecacct(Vctl *v)
{
	for (; v != nil; v = v->next)
		ainc(&v->count);
}

void
trapclock(Ureg *ureg, void *)
{
	timerintr(ureg, 0);
	iprint("trapclock called\n");
}

/* clear any ipi to this cpu */
void
clearipi(void)
{
	/* extinguish my ipi source */
	if (nosbi || soc.ipiclint)
		m->clint->msip[m->hartid] = 0;
	else
		sbiclearipi();		/* slow and stupid */
	coherence();
	clrsipbit(Ssie|Msie);
}

/*
 * IPI: a wakeup from another cpu.
 * will break out of interruptible wfi if resumed.
 * if wfi is not interruptible, the ipi will resume it and we won't get here.
 * thus these ipis are probably due to races: we thought the target wanted
 * an ipi, but it then changed that signal.
 */
void
trapipi(Ureg *, void *v)
{
	iprint("trapipi called\n");
	m->intr++;
	clearipi();
	vecacct(v);
}

void
trapfpu(Ureg *, void *)
{
	panic("trapfpu called");
}

void
countipi(void)
{
	vecacct(vctl[vctlidx(Supswintr, Localintr)]);
}

void
trapinit(void)
{
	int vno;

	/* allocate these vectors for irqalloc counters at least */
	trapenable(vctlidx(Suptmrintr, Localintr), trapclock, nil,
		"clint clock");
	trapenable(vctlidx(Mchtmrintr, Localintr), trapclock, nil,
		"clint clock (mach)");
	vno = vctlidx(Supswintr, Localintr);
	/* vctl[vno] may be nil, but that's okay */
	trapenable(vno, trapipi, vctl[vno], "ipi");

	/* use trapclock as we don't expect it to be called via trapenable */
	trapenable(vctlidx(Instpage, Exception), trapclock, nil,
		"page faults (instr)");
	trapenable(vctlidx(Loadpage, Exception), trapclock, nil,
		"page faults (load)");
	trapenable(vctlidx(Storepage, Exception), trapclock, nil,
		"page faults (store)");
	vno = vctlidx(Illinst, Exception);
	/* vctl[vno] may be nil, but that's okay */
	trapenable(vno, trapfpu, vctl[vno], "fpu (ill inst)");

	plicinit();
	addarchfile("irqalloc", 0444, irqallocread, nil);
}

static char* excname[] = {
	"instruction address alignment",
	"instruction access",
	"illegal instruction",
	"breakpoint",
	"load address alignment",
	"load access",
	"store address alignment",
	"store access",
	"system call",
	"environment call from super mode",
	"#10 (reserved)",
	"environment call from machine mode",
	"instruction page fault",
	"load page fault",
	"#14 (reserved)",
	"store page fault",
};

/*
 *  keep histogram of interrupt service times
 */
void
intrtime(Mach*, int vno)
{
	ulong diff;
	ulong x;

	x = perfticks();
	diff = x - m->perf.intrts;
	m->perf.intrts = x;

	m->perf.inintr += diff;
	if(up == nil && m->perf.inidle > diff)
		m->perf.inidle -= diff;

	diff /= m->cpumhz*100;		/* quantum = 100µsec */
	if(diff >= Ntimevec)
		diff = Ntimevec-1;
	if ((uint)vno >= nelem(intrtimes))
		vno = nelem(intrtimes)-1;
	intrtimes[vno][diff]++;
}

/*
 * prepare to go back to user space:
 * update profiling cycle counts at stack top.
 */
void
kexit(Ureg *)
{
	uvlong t;
	Tos *tos;

	if(up == nil)
		return;

	/* precise time accounting, kernel exit */
	tos = (Tos*)TOS(USTKTOP);
	cycles(&t);
	if (t >= up->kentry)
		tos->kcycles += t - up->kentry;
	tos->pcycles = up->pcycles;
	tos->pid = up->pid;
	tos->cpucap = sys->cpucap;
}

/*
 * Craft a return frame which will cause the child to pop out of
 * the scheduler in user mode with the return register zero.
 */
void
sysrforkchild(Proc* child, Proc* parent)
{
	Ureg *cureg;

	cureg = (Ureg*)STACKALIGN((uintptr)child->kstack+KSTACK - sizeof(Ureg));
	child->sched.sp = PTR2UINT(cureg);
	child->sched.pc = PTR2UINT(sysrforkret);

	memmove(cureg, parent->dbgreg, sizeof(Ureg));

	/* Things from bottom of syscall which were never executed */
	child->psstate = 0;
	child->insyscall = 0;

	fpusysrforkchild(child, parent);
}

static void
posttrapnote(Ureg *ureg, uint cause, char *name)
{
	char buf[ERRMAX];

	spllo();
	snprint(buf, sizeof buf, "sys: trap: %s for address %#p",
		cause < nelem(excname)? excname[cause]: name, ureg->tval);
	postnote(up, 1, buf, NDebug);
}

int
userureg(Ureg* ureg)
{
	switch (ureg->curmode) {
	default:
		return (ureg->status & Spp) == 0;
	case Mppmach:
		return (ureg->status & Mpp) == Mppuser;
	}
}

static int
isfpinst(uintptr pc, Ureg *ureg)
{
	int n, csr;
	ulong inst;

	/* is it a compressed instruction? */
	inst = *(ushort *)pc;		/* instrs are little-endian */
	if (ISCOMPRESSED(inst)) {
		n = (inst >> 13) & MASK(3);
		if  ((inst & 1) == 0 && (n == 1 || n == 5))
			return 1;	/* fp load/store double */
	} else {
		/* is it a regular instruction (possibly not 4-byte aligned)? */
		inst = UNCOMPINST(pc);
		if (inst != ureg->tval)
			iprint("illinst: inst %#lux tval %#llux\n",
				inst, ureg->tval);
		/* >>2 to ignore "uncompressed" indicator bits */
		switch (BASEOP(inst) >> 2) {
		case 001:		/* load fp */
		case 011:		/* store fp */
		case 024:		/* fp op */
			return 1;
		case SYSTEM>>2:
			/* check for fp csr instrs */
			if (((inst >> 12) & MASK(2)) != 0) {
				csr = inst >> 20;
				if (csr >= FFLAGS && csr <= FCSR)
					return 1;
			}
			break;
		}
	}
	return 0;
}

/*
 * on riscv, we have to manually advance the PC past an ECALL instruction,
 * for example, so that we don't re-execute it.
 */
static void
advancepc(Ureg *ureg)
{
	/* examine short at PC, which is sufficient to decide if compressed */
	if ((ureg->pc & 1) == 0)
		ureg->pc += ISCOMPRESSED(*(ushort *)ureg->pc)? 2: 4;
}

void poll(Ureg *, Cause *);

static int
traplocalintr(Ureg *ureg, Cause *cp)
{
	int clockintr;
	uint cause;

	m->intr++;			/* okay here; only tmr and sw intrs */
	m->perf.intrts = perfticks();
	cause = cp->cause;
	if (cause < Local0intr)
		cause &= ~Msdiff;	/* map mach to super codes */
	switch (cause) {
	case Suptmrintr:
		clockoff();
		if (++m->clockintrdepth > 1 && m->clockintrsok) {
			/* nested clock interrupt; probably shutting down */
			m->clockintrsok = 0;
			// iprint("cpu%d: nested clock interrupt\n", m->machno);
		}
		timerintr(ureg, 0);
		--m->clockintrdepth;
		clockenable();
		clockintr = 1;
		break;
	case Supswintr:
		/*
		 * an ipi should normally pop out of wfi at splhi in idlehands,
		 * and not end up here.  getting here means that we sent an ipi
		 * to a cpu that stopped waiting before the ipi arrived, which
		 * is harmless.  sending the ipi zeroed ipiwait.  if we could
		 * interrupt idlehands, we would want to set mp->ipiwait=1 here
		 * so that it wouldn't be counted both by idlehands and below.
		 */
		gotipi = 1;
		clearipi();
		clockintr = 0;
		break;
	case Supextintr:
		/*
		 * NB: intr is not reached here, we short-circuited the cause
		 * to Globalintr in whatcause.
		 */
		// clockintr = intr(ureg, cp);
		panic("traplocalintr: handed an external interrupt");
		notreached();
	case 0:					/* probably NMI */
		if(m->machno == 0)
			panic("NMI @ %#p", ureg->pc);
		else {
			iprint("nmi: cpu%d: PC %#p\n", m->machno, ureg->pc);
			for(;;)
				idlehands();
		}
		notreached();
	default:
		panic("trap: unexpected local interrupt %d", cp->cause);
		notreached();
	}
	clrsipbit(1<<cp->cause);
	vecacct(vctl[cp->vno]);
	intrtime(m, cp->vno);
	if (!soc.plic)
		poll(ureg, nil);		/* frequent polling */
	return clockintr;
}

static void
trapmisaligned(Ureg *ureg, Cause *cp)
{
	if (cp->user)
		posttrapnote(ureg, cp->cause, "misaligned access");
	else
		panic("misaligned access to %#p at %#p", ureg->tval, ureg->pc);
}

#ifdef INST_DECODE
typedef struct Instdecoded Instdecoded;
struct Instdecoded {
	uchar	opcode;
	uchar	compressed;
	uchar	rs1;
	uchar	rs2;
	uchar	rd;
	uchar	funct3;		/* for R I S B types */
	uchar	funct7;
	/* immediate bits are swizzled and stored in random places */
};

/* decode uncompressed instruction at pc into idp */
static void
instdecode(Instdecoded *idp, Ureg *ureg, uintptr pc)
{
	ulong inst;

	memset(idp, 0, sizeof *idp);
	inst = *(ushort *)pc;		/* instrs are little-endian */
	if (ISCOMPRESSED(inst)) {
		idp->compressed = 1;
		return;
	}
	inst = UNCOMPINST(pc);
	if (inst != ureg->tval)
		iprint("illinst: inst %#lux tval %#llux\n", inst, ureg->tval);
	idp->opcode = BASEOP(inst);
	inst >>= 7;
	idp->rd = inst & MASK(5);
	inst >>= 5;
	idp->funct3 = inst & MASK(3);
	inst >>= 3;
	idp->rs1 = inst & MASK(5);
	inst >>= 5;
	idp->rs2 = inst & MASK(5);
	inst >>= 5;
	idp->funct7 = inst;
}
#endif

static void
badinst(Ureg *ureg, Cause *cp)
{
	if (cp->user)
		posttrapnote(ureg, cp->cause, "illegal instruction");
	else
		panic("illegal instruction at %#p: %#p", ureg->pc, ureg->tval);
}

/*
 * Illinst could be from legitimate user fp use while fpu is off.
 * It could also be due to the C910 erroneously trapping an unknown fence,
 * such as fence.tso.
 */
static void
trapillinst(Ureg *ureg, Cause *cp)
{
	int rd, skip, funct3;
	ulong inst;
	uintptr pc;

	/* if non-zero, ureg->tval will be the trapping instruction */
	pc = ureg->pc;
	if (pc & 1) {
		if (!cp->user)
			panic("trapillinst: odd pc %#p", pc);
		ureg->tval = pc;
		posttrapnote(ureg, cp->cause, "odd pc");
		return;
	}
	if (isfpinst(pc, ureg)) {
		if (!cp->user)
			panic("kernel fpu use at %#p: %#p", pc, ureg->tval);
		fptrap(ureg, 0);
		vecacct(vctl[cp->vno]);
		return;
	}

	skip = 0;
	inst = *(ushort *)pc;		/* instrs are little-endian */
	if (ISCOMPRESSED(inst)) {
		badinst(ureg, cp);
		return;
	}
	inst = UNCOMPINST(pc);
	if (inst != ureg->tval)
		iprint("illinst: inst %#lux tval %#llux\n", inst, ureg->tval);
	funct3 = (inst>>12) & MASK(3);
	switch (BASEOP(inst)) {
	case SYSTEM:
		if ((funct3 & MASK(2)) != 0) {
			/*
			 * bad CSR: zero dest and otherwise ignore.
			 * some CSRs are optional or obsolete.
			 */
			iprint("CSR instruction for bad CSR %ld at %#p\n",
				inst>>20, pc);
			rd = (inst>>7) & MASK(5);
			if (rd)
				ureg->regs[rd] = 0;
			m->probebad = 1;
			skip = 1;
		}
		break;
	case 0xf:
		if (funct3 == 0)
			/*
			 * some fence; shouldn't happen, but c910 is buggy.
			 * coherence has been called.
			 */
			skip = 1;
		break;
	}
	if (skip)
		ureg->pc += 4;
	else
		badinst(ureg, cp);
}

static void
trapaccess(Ureg *ureg, Cause *cp)
{
	if (cp->user)
		posttrapnote(ureg, cp->cause, "illegal access");
	else
		panic("trap: illegal access to %#p at %#p", ureg->tval,
			ureg->pc);
}

/* even if the memory exists, it must be mapped before calling this. */
/* may only work in machine mode if sbi intercepts access traps. */
vlong
probeulong(ulong *addr, int wr)
{
	ulong old;
	Mpl pl;

	pl = splhi();
	if (Probedebug) {
		iprint("probing %#p...", addr);
		delay(100);
	}
	m->probing = 1;		/* set probebad on any exception */
	m->probebad = 0;
	old = 0x01020304;
	USED(old);
	coherence();

	old = *addr;
	if (wr)
		*addr = old;	/* rewrite word, in hopes of doing no harm */
	coherence();		/* should fault by now if addr is bad */

	m->probing = 0;
	if (Probedebug) {
		iprint(m->probebad? "missing\n": "present\n");
		delay(100);
	}
	splx(pl);
	if (m->probebad) {
		m->probebad = 0;
		return -1;
	} else
		return old;
}

typedef struct Lastexcept Lastexcept;
struct Lastexcept {		/* should be per cpu */
	uintptr	pc;
	uintptr	addr;		/* failed address of load or store */
	short	consec;
};

static void
faultstuck(Ureg *ureg)
{
	static Lastexcept exc;

	if (ureg->pc == exc.pc && ureg->tval == exc.addr) {
		if (++exc.consec >= 10)
			panic("%d consecutive exceptions at pc %#p for addr %#p",
				exc.consec, exc.pc, exc.addr);
	} else {
		exc.pc = ureg->pc;
		exc.addr = ureg->tval;
		exc.consec = 0;
	}
}

static Exchandler exchandlers[] = {
[Breakpt]	debugbpt,
[Instpage]	faultriscv64,
[Loadpage]	faultriscv64,
[Storepage]	faultriscv64,
[Instaccess]	trapaccess,
[Loadaccess]	trapaccess,
[Storeaccess]	trapaccess,
[Illinst]	trapillinst,
[Instaddralign]	trapmisaligned,
[Loadaddralign]	trapmisaligned,
[Storeaddralign] trapmisaligned,
[Envcalluser]	trapsyscall,		/* backstop; short-cut in trap */
};

/* handle exceptions */
static int
trapriscv64(Ureg *ureg, Cause *cp)
{
	uint cause;
	Exchandler handler;

	if (cp->user)
		m->turnedfpoff = 0;
	else if (m->probing) {
		m->probebad = 1;
		m->probing = 0;
		coherence();
		if (0)
			iprint("probe trapped\n");
		/* have to advance PC on risc-v to skip faulting instruction. */
		advancepc(ureg);
		return 0;			/* not a clock interrupt */
	}

	cause = cp->cause;
	if (cause >= nelem(exchandlers))
		panic("trapriscv64: caused %d out of range", cause);
	handler = exchandlers[cause];
	if (handler) {
		(*handler)(ureg, cp);
		if (Trapdebug)
			faultstuck(ureg);
		return 0;		/* not a clock interrupt */
	}

	/* see trap() for Envcalluser short cut, mtrap.s for Envcallsup */
	if (cause == Envcallmch)
		panic("unexpected environment call from machine mode");

	/* could be a local intr */
	panic("unknown exception, cause %d from_user %d", cause, cp->user);
	notreached();
}

/* poll enabled devices; for systems without a working, standard plic */
void
poll(Ureg *ureg, Cause *cp)		/* desperate last resort */
{
	int vno;
	Vctl *v;

	if (cp != nil)
		clrsipbit(1<<cp->cause);

	for (v = pollvecs; v != nil; v = v->pollnxt)
		if (v->f) {
			m->perf.intrts = perfticks();
			vno = vctlidx(v->irq, Globalintr);
			if (vno < 0)
				panic("intr: intr id %d out of range", v->irq);
			(*v->f)(ureg, v->a);
			intrtime(m, vno);
			/* this is the best we can do without a plic */
			vecacct(v);
		}
}

void
pollkproc(void *)
{
	Ureg ureg;

	memset(&ureg, 0, sizeof ureg);
	for (;;) {
		splhi();
		poll(&ureg, nil);
		spllo();
	}
}

static void
callintrsvc(Ureg *ureg, Vctl *vec)
{
	void (*isr)(Ureg *, void *);
	Vctl *v;

	if (vec->irq)
		m->lastintr = vec->irq;
	for(v = vec; v != nil; v = v->next) {
		isr = v->f;
		if (isr == nil) {
			iprint("intr: no isr for vector %d\n", v->vno);
			continue;
		}
		(*isr)(ureg, v->a);
		splhi();		/* in case isr dropped PL */
		ainc(&v->count);
	}
}
		
/*
 * call for global interrupts (those coming from the plic).
 * these are never clock interrupts, so returns 0 (not a clockintr).
 */
int
intr(Ureg* ureg, Cause *cp)
{
	int id, ctxt, vno, trips;
	Vctl *vec;

	m->intr++;
	ctxt = m->plicctxt + Super;
	if (Intrdebug)
		iprint("intr: checking plic for ctxt %d\n", ctxt);
	trips = 100;
	while (soc.plic && (id = plicclaim(ctxt)) != 0) {
		m->perf.intrts = perfticks();
		/* id is actual cause, global irq.  map to a vector. */
		/* cp->vno = */ vno = vctlidx(id, Globalintr);
		if (vno < 0)
			panic("intr: intr id %d out of range", id);

		if (Intrdebug)
			iprint("intr: plic id %d vector %d\n", id, vno);
		vec = vctl[vno];
		if (vec == nil) {		/* maybe it's spurious? */
			plicdisable(vno);
			iprint("intr: no vector set up for intr id %d\n", id);
		} else
			callintrsvc(ureg, vec);
		pliccompl(id, ctxt);
		intrtime(m, vno);

		if (--trips <= 0) {
			plicoff();
			soc.plic = 0;		/* poll in future */
			iprint("intr: stuck in plicclaim loop, id %d, polling\n",
				id);
		}
	}
	if (!soc.plic)
		poll(ureg, cp);			/* mainly for tinyemu */
	/* having no work is not unusual */
	if (Intrdebug)
		iprint("intr: done\n");

	/* in case the cpus all raced into wfi, always wake */
	if(up)
		preempted();
	/*
	 * procs waiting for this interrupt could be on
	 * any cpu, so wake any idling cpus.
	 */
	idlewake();
	return 0;
}

int
intrnotrap(void)
{
	return intr(nil, nil);
}

#ifdef unused
int
intrclknotrap(Ureg *ureg)
{
	int vno;

	m->intr++;			/* okay here; only tmr and sw intrs */
	m->perf.intrts = perfticks();
	clockoff();

	if (++m->clockintrdepth > 1 && m->clockintrsok) {
		/* nested clock interrupt; probably shutting down */
		m->clockintrsok = 0;
		// iprint("cpu%d: nested clock interrupt\n", m->machno);
	}
	timerintr(ureg, 0);	/* uses ureg to distinguish user/kernel pc */
	--m->clockintrdepth;

	clockenable();
	clrsipbit(Stie|Mtie);
	vno = vctlidx(Suptmrintr, Localintr);
	vecacct(vctl[vno]);
	intrtime(m, vno);
	return 1;			/* is a clock intr */
}
#endif

static void
trapsyscall(Ureg *, Cause *)
{
	panic("no system calls in diagnostic");
}

static void
trapdbg(Ureg *ureg, Cause *cp, int entry)
{
	int type;

	type = cp->type;
	/*
	 * if we print uart interrupts, we'll recurse forever,
	 * and printing clock interrupts would flood the console.
	 */
	if (type != Exception)
		return;
	iprint("|%c%c%c ", ureg->curmode == Mppsuper? 'S': 'M', entry? '>': '<',
		type == Exception? 'E': 'I');
	if (cp->cause >= nelem(excname))
		iprint("cause %d", cp->cause);
	else {
		iprint("%s", excname[cp->cause]);
		if (cp->cause == Envcalluser) {
			iprint(" pid %d", up->pid);
			if (entry)
				iprint(" some system call");
			else
				iprint(" return %#llux", ureg->arg);
		}
	}
	iprint(" from %s pc %#p tval %#p up %#p\n",
		cp->user? "user": "kernel", ureg->pc, ureg->tval, up);
}

/* decode trap cause from *ureg into *cp & return type */
static int
whatcause(Cause *cp, Ureg *ureg)
{
	int type, cause;

	cp->user = userureg(ureg);
	if(cp->user){
		up->dbgreg = ureg;
		cycles(&up->kentry);
	}

	/* ureg->cause has MCAUSE or SCAUSE CSR, as appropriate */
	cause = ureg->cause & ~Rv64intr;
	if (!(ureg->cause & Rv64intr))
		type = Exception;
	else if ((cause & ~Msdiff) == Supextintr) {
		type = Globalintr;
		cause = 0;	/* actual causes will come from the plic */
	} else
		type = Localintr;	/* these are very frequent */
	cp->type = type;
	cp->cause = cause;

	cp->vno = vctlidx(cause, type);
	if (cp->vno < 0)
		panic("trap: cause %d or type %d out of range", cause, type);

	/* these counts are reset every second */
	if (Trapdebug && cp->vno &&
	    ++trapcnt[cp->vno] % (1024*1024) == 0)	/* tweak to taste */
		iprint("trap: vector %d trapping lots\n", cp->vno);
	return type;
}

static Traphandler traphandlers[Nfaulttypes] = {
[Unknownflt]	nil,
[Exception]	trapriscv64,			/* may enable fpu */
[Localintr]	traplocalintr,
[Globalintr]	intr,
};

/*
 *  All traps come here.  It is slower to have all traps call trap()
 *  rather than directly vectoring the handler.  However, this avoids a
 *  lot of code duplication and possible bugs.
 *  Trap is called with interrupts disabled.
 */
void
trap(Ureg* ureg)
{
	int clockintr;
	uint type;
	Cause why;
	Traphandler handler;

	if (ureg == nil)
		panic("trap with nil ureg");
	if (early)			/* still in low.c? */
		iprint("early trap %#p at pc %#p\n", ureg->cause, ureg->pc);

	/*
	 * if we trapped from supervisor mode into machine mode, revert to
	 * phys device addresses, assuming that we are about to reboot.
	 * this is only possible if we link with mtrap.$O (e.g., tinyemu).
	 */
	if (ureg->curmode == Mppmach && (ureg->status & Mpp) == Mppsuper)
		usephysdevaddrs();

	type = whatcause(&why, ureg);
	if (Trapdebug)
		trapdbg(ureg, &why, 1);

	/* short-cut for syscalls */
	if (ureg->cause == Envcalluser)	/* Rv64intr is off for exceptions */
		trapsyscall(ureg, nil);
		/* syscall did the whole job; we're done */
	else {
		if (type >= nelem(traphandlers))
			panic("trap: trap type %d too large", type);
		handler = traphandlers[type];
		if (handler == nil)
			panic("trap: trap type %d has no handler", type);
		clockintr = (*handler)(ureg, &why);
		splhi();
		fpsts2ureg(ureg); /* propagate Fsst changes back to user mode */

		/*
		 * delaysched set (because we held a lock or because our
		 * quantum ended)?
		 */
		if(up && up->delaysched && clockintr && m->clockintrsok) {
			sched();
			splhi();
		}
		if(why.user) {
			if(up->procctl || up->nnote)
				notify(ureg);
			/*
			 * kexit bills time to whatever process was running,
			 * so don't call it here.
			 */
		}
	}

	if (Trapdebug)
		trapdbg(ureg, &why, 0);
}

/*
 * Dump general registers.
 * try to fit it on a cga screen (80x25).
 */
static void
dumpgpr(Ureg* ureg)
{
	int i;

	if(up != nil)
		iprint("cpu%d: registers for %s %d\n",
			m->machno, up->text, up->pid);
	else
		iprint("cpu%d: registers for kernel\n", m->machno);

	for (i = 1; i <= 31; i++)
		iprint("r%d\t%#16.16p%c", i, ureg->regs[i],
			i%2 == 0 || i == 31? '\n': '\t');
	iprint("pc\t%#p\t", ureg->pc);
	iprint("type\t%#p\t", ureg->type);
	iprint("m\t%#16.16p\nup\t%#16.16p\n", m, up);
}

void
dumpregs(Ureg* ureg)
{
	if(getconf("*nodumpregs")){
		iprint("dumpregs disabled\n");
		return;
	}
	dumpgpr(ureg);

	/*
	 * Processor CSRs.
	 */
	iprint("satp\t%#16.16llux\n", (uvlong)getsatp());

//	archdumpregs();
}

/*
 * Fill in enough of Ureg to get a stack trace, and call a function.
 * Used by debugging interface rdb.
 */
void
callwithureg(void (*fn)(Ureg*))
{
	Ureg ureg;

	memset(&ureg, 0, sizeof ureg);
	ureg.pc = getcallerpc(&fn);
	ureg.sp = (uintptr)&fn;
	ureg.link = ureg.pc;
	fn(&ureg);
}

static void
dumpstackwithureg(Ureg* ureg)
{
	uintptr l, v, i, estack;
	int x;

	if(ureg != nil)
		dumpregs(ureg);
	if(getconf("*nodumpstack")){
		iprint("dumpstack disabled\n");
		return;
	}
	iprint("dumpstack\n");

	x = 0;
	x += iprint("ktrace /kernel/path %#p %#p %#p\n",
		ureg->pc, ureg->sp, ureg->link);
	i = 0;
	if(up != nil
//	&& (uintptr)&l >= (uintptr)up->kstack
	&& (uintptr)&l <= (uintptr)up->kstack+KSTACK)
		estack = (uintptr)up->kstack+KSTACK;
	else if((uintptr)&l >= m->stack && (uintptr)&l <= m->stack+MACHSTKSZ)
		estack = m->stack+MACHSTKSZ;
	else{
		if(up != nil)
			iprint("&up->kstack %#p &l %#p\n", up->kstack, &l);
		else
			iprint("&m %#p &l %#p\n", m, &l);
		return;
	}
	x += iprint("estackx %#p\n", estack);

	for(l = (uintptr)&l; l < estack; l += sizeof(uintptr)){
		v = *(uintptr*)l;
		if((KTZERO <= v && v < (uintptr)etext)
		|| ((uintptr)&l < v && v < estack) || estack-l < 256){
			x += iprint("%#16.16p=%#16.16p ", l, v);
			i++;
		}
		if(i == 2){
			i = 0;
			x += iprint("\n");
		}
	}
	if(i)
		iprint("\n");
}

void
dumpstack(void)
{
	callwithureg(dumpstackwithureg);
}

static void
debugbpt(Ureg* , Cause*)
{
	char buf[ERRMAX];

	if(up == 0)
		panic("kernel bpt");
	/*
	 * on riscv, pc points at the instruction that caused the trap,
	 * so there's no need to back up the pc.
	 */
	snprint(buf, sizeof buf, "sys: breakpoint");
	postnote(up, 1, buf, NDebug);
}

/*
 * the page fault couldn't be resolved, probably because the address is
 * unmapped.  Report the error in the appropriate way.
 */
static void
badpagefault(Ureg *ureg, uintptr addr, int read, int insyscall)
{
	char buf[ERRMAX];

	/*
	 * It is possible to get here with !user if, for example,
	 * a process was in a system call accessing a shared
	 * segment but was preempted by another process which shrunk
	 * or deallocated the shared segment; when the original
	 * process resumes it may fault while in kernel mode.
	 * No need to panic this case, post a note to the process
	 * and unwind the error stack. There must be an error stack
	 * (up->nerrlab != 0) if this is a system call, if not then
	 * the game's a bogey.
	 */
	if(!userureg(ureg) && (!insyscall || up->nerrlab == 0)){
		dumpregs(ureg);
		panic("fault: addr %#p pc %#p", addr, ureg->pc);
	}
	snprint(buf, sizeof buf, "sys: trap: fault %s addr=%#p pc=%#p",
		read? "read": "write", addr, ureg->pc);
	postnote(up, 1, buf, NDebug);
	if(insyscall)
		error(buf);
}

/*
 *  find out fault address and type of access.
 *  Call common fault handler.
 */
static void
faultriscv64(Ureg* ureg, Cause *cp)
{
	uintptr addr;
	int read, insyscall;

#ifdef TRAPDEBUG
iprint("*");
#endif
	/*
	 * There must be a user context.
	 * If not, the usual problem is causing a fault during
	 * initialisation before the system is fully up.
	 */
	addr = ureg->tval;
	if(up == nil)
		panic("fault %#lld with up == nil; pc %#p addr %#p",
			ureg->cause, ureg->pc, addr);
	if (addr == 0 && up->nlocks)		/* debugging for gs */
		panic("fault %#lld pc %#p addr %#p nlocks %d",
			ureg->cause, ureg->pc, addr, up->nlocks);

	vecacct(vctl[cp->vno]);
	insyscall = up->insyscall;
	up->insyscall = 1;
	if (soc.c910)
		/* dump cpu state (debugging) */
		iprint("page fault cause %lld\n", ureg->cause);
	read = ureg->cause != Storepage;  /* exception, so Rv64intr must be 0 */
	/* page fault on a kernel address is never okay. */
	if((intptr)addr < 0 || fault(addr, read) < 0)
		badpagefault(ureg, addr, read, insyscall);
	up->insyscall = insyscall;
}

/*
 *  return the userpc the last exception happened at
 */
uintptr
userpc(Ureg* ureg)
{
	if(ureg == nil && up != nil)
		ureg = up->dbgreg;
	return ureg? ureg->pc: 0;
}

/*
 * This routine must save the values of registers the user is not permitted
 * to write from devproc and then restore the saved values before returning.
 */
void
setregisters(Ureg* ureg, char* pureg, char* uva, int n)
{
	uintptr mie;

	mie = ureg->ie;
	memmove(pureg, uva, n);
	/* user shouldn't change any status bits on risc-v */
	ureg->ie = mie;
}

/*
 * Give enough context in the ureg to produce a kernel stack for
 * a sleeping process
 */
void
setkernur(Ureg* ureg, Proc* p)
{
	ureg->pc = p->sched.pc;
	/* skip return PC at sp to produce Ureg* */
	ureg->sp = SKIPSTKPC(p->sched.sp);
}

uintptr
dbgpc(Proc *p)
{
	Ureg *ureg;

	ureg = p->dbgreg;
	return ureg? ureg->pc: 0;
}

/*
 * machine-specific system call and note code
 */

static void
vrfyuregaddrs(Ureg *nur)
{
	/* pc may point to a compressed instruction, thus ushort */
	if(!okaddr(nur->pc, sizeof(ushort), 0) ||
	   !okaddr(nur->sp, sizeof(uintptr), 0)){
		qunlock(&up->debug);
		pprint("suicide: trap in noted pc=%#p sp=%#p\n",
			nur->pc, nur->sp);
		pexit("Suicide", 0);
	}
}

/*
 *   Return user to state before notify() or kill the process.
 *   There is an NFrame on the user's stack, below the original Ureg.
 */
void
noted(Ureg* cur, uintptr arg0)
{
	NFrame *nf;
	Note note;
	Ureg *nur;

	qlock(&up->debug);
	if(arg0 != NRSTR && !up->notified){
		qunlock(&up->debug);
		pprint("suicide: call to noted when not notified\n");
		pexit("Suicide", 0);
	}
	up->notified = 0;
	fpunoted();

	nf = up->ureg;

	/* sanity clause */
	if(!okaddr(PTR2UINT(nf), sizeof(NFrame), 0)){
		qunlock(&up->debug);
		pprint("suicide: bad ureg %#p in noted\n", nf);
		pexit("Suicide", 0);
	}

	nur = &nf->ureg; /* Ureg on users's stack, possibly modified by user */
	/* don't let the user change any of system status */
	nur->status = cur->status;
//	nur->status = cur->status & ~(Spp|Sie) | Spie;
	memmove(cur, nur, sizeof(Ureg));

	switch((int)arg0){
	case NCONT:
	case NRSTR:
		vrfyuregaddrs(cur);
		up->ureg = nf->old;		/* restore original Ureg */
		/* if we interrupted a system call, advance PC */
		if ((cur->pc & 1) == 0 && UNCOMPINST(cur->pc) == ECALLINST)
			advancepc(cur);
		qunlock(&up->debug);
		/* upon return, user process will resume at up->pc */
		break;
	case NSAVE:
		vrfyuregaddrs(cur);
		qunlock(&up->debug);

		splhi();
		nf->arg1 = nf->msg;
		nf->arg0 = &nf->ureg;
		cur->arg = PTR2UINT(nf->arg0);
		nf->pc = 0;
		cur->sp = PTR2UINT(nf);
		break;
	default:
		memmove(&note, &up->lastnote, sizeof(Note));
		qunlock(&up->debug);
		pprint("suicide: bad arg %#p in noted: %s\n", arg0, note.msg);
		pexit(note.msg, 0);
		break;
	case NDFLT:
		memmove(&note, &up->lastnote, sizeof(Note));
		qunlock(&up->debug);
		if(note.flag == NDebug)
			pprint("suicide: %s\n", note.msg);
		pexit(note.msg, note.flag != NDebug);
		break;
	}
}

/*
 *  Call user, if necessary, with note.
 *  Pass user the Ureg struct and the note on his stack.
 */
int
notify(Ureg* ureg)
{
	int l;
	Mpl s;
	Note note;
	uintptr sp;
	NFrame *nf;

	if(up->procctl)
		procctl(up);
	if(up->nnote == 0)
		return 0;

	fpunotify(ureg);

	s = spllo();
	qlock(&up->debug);

	up->notepending = 0;
	memmove(&note, &up->note[0], sizeof(Note));
	if(strncmp(note.msg, "sys:", 4) == 0){
		l = strlen(note.msg);
		if(l > ERRMAX-sizeof(" pc=0x0123456789abcdef"))
			l = ERRMAX-sizeof(" pc=0x0123456789abcdef");
		seprint(note.msg+l, note.msg + ERRMAX, " pc=%#p", ureg->pc);
	}

	if(note.flag != NUser && (up->notified || up->notify == nil)){
		qunlock(&up->debug);
		if(note.flag == NDebug)
			pprint("suicide: %s\n", note.msg);
		pexit(note.msg, note.flag != NDebug);
	}

	if(up->notified){
		qunlock(&up->debug);
		splhi();
		return 0;
	}

	if(up->notify == nil){
		qunlock(&up->debug);
		pexit(note.msg, note.flag != NDebug);
	}
	if(!okaddr(PTR2UINT(up->notify), sizeof(ureg->pc), 0)){
		qunlock(&up->debug);
		pprint("suicide: bad function address %#p in notify\n",
			up->notify);
		pexit("Suicide", 0);
	}

	/* allocate frame (containing a new Ureg) on user-mode stack */
	sp = ureg->sp - sizeof(NFrame);
	if(!okaddr(sp, sizeof(NFrame), 1)){
		qunlock(&up->debug);
		pprint("suicide: bad stack address %#p in notify\n", sp);
		pexit("Suicide", 0);
	}

	/* populate that new frame */
	nf = (NFrame *)sp;
	memmove(&nf->ureg, ureg, sizeof(Ureg));
	nf->old = up->ureg;
	up->ureg = nf;
	memmove(nf->msg, note.msg, ERRMAX);
	nf->arg1 = nf->msg;
	nf->arg0 = &nf->ureg;
	ureg->arg = PTR2UINT(nf->arg0);
	nf->pc = 0;

	/* upon return, the note handler will run with NFrame on its stack */
	ureg->sp = sp;
	ureg->pc = PTR2UINT(up->notify);
	up->notified = 1;
	up->nnote--;
	memmove(&up->lastnote, &note, sizeof(Note));
	memmove(&up->note[0], &up->note[1], up->nnote*sizeof(Note));

	qunlock(&up->debug);
	splx(s);

	return 1;
}
