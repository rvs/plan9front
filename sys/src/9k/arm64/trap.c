/*
 * traps, faults, interrupts, system call entry
 */
#include	"u.h"
#include	"../port/lib.h"
#include	"mem.h"
#include	"dat.h"
#include	"fns.h"
#include	"../port/error.h"

#include	"/sys/src/libc/9syscall/sys.h"
#include	<tos.h>
#include	"ureg.h"
#include	"arm64.h"
#include	"io.h"

enum {
	Debug = 0,

	Ngintr = 1024,			// TODO: dummy
	Nlintr = 1024,			// TODO: dummy
	Nexc = 1024,			// TODO: dummy
	Ntimevec = 20,		/* number of time buckets for each intr */
	Ncauses = Ngintr + Nlintr + Nexc,
};
enum Faultypes {
	Unknownflt, Exception, Localintr, Globalintr, Nfaulttypes,
};

/* syscall stack frame (on arm at least); used for notes */
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
	uchar	type;		/* exception or interrupt flavour */
	uchar	user;
	short	vno;
} Cause;
typedef int (*Traphandler)(Ureg *, Cause *);
typedef void (*Exchandler)(Ureg *, Cause *);

static void debugbpt(Ureg*, Cause*);
static void dumpgpr(Ureg* ureg);
static void faultarm64(Ureg*, Cause *);

static Lock vctllock;
static Vctl *vctl[Ncauses];
static ulong trapcnt[Ncauses];

ulong intrtimes[Ncauses][Ntimevec];

int	intr(Ureg* ureg, Cause *cp);

static void trapsyscall(Ureg *ureg, Cause *);

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

static uint
causeof(uint vno)
{
	if (vno < Ngintr)
		return vno;
	else if (vno < Ngintr + Nlintr)
		return vno - Ngintr;
	else if (vno < Ngintr + Nlintr + Nexc)
		return vno - (Ngintr + Nlintr);
	else {
		print("causeof %d: unknown vno\n", vno);
		return -1;
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
	clockoff(clintaddr());
	coherence();
	clearipi();
}

void
trapvecs(void)
{
#ifdef TODO
	putsscratch((uintptr)m);	/* high virtual */
	putstvectodo((void *)strap);	/* " */
	putsie(getsie() | Superie);	/* individual enables; still splhi */
#endif
}

/*
 * originally, we enabled the interrupt in all S contexts, and
 * let the fastest cpu service it.  routing them all to cpu0 seems
 * to be about the same speed and avoids quadratic sending of IPIs.
 */
void
gicenable(uint irq)
{
}

void
gicnopend(uint irq)			/* unused */
{
}

void
gicdisable(uint irq)
{
}

void
gicoff(void)
{
}

/* returns interrupt id (irq); 0 is none */
uint
gicclaim(uint ctxt)
{
	return 0;
}

void
giccompl(uint irq, uint ctxt)		/* tell hw interrupt is serviced */
{
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
		print("intrenable: irq %d > Ngintr (%d)\n", irq, Ngintr);
		return nil;
	}

	v = newvec(f, a, tbdf, name);
	v->isintr = 1;
	v->irq = irq;

	ilock(&vctllock);
	vno = vctlidx(irq, Globalintr);
	if(vno == -1){
		iunlock(&vctllock);
		print("intrenable: couldn't enable irq %d, tbdf %#ux for %s\n",
			irq, tbdf, v->name);
		free(v);
		return nil;
	}
	if((uint)vno >= nelem(vctl)) {
		iunlock(&vctllock);
		panic("intrenable: vno %d out of range", vno);
	}
	if(vctl[vno]){
		iunlock(&vctllock);
		panic("intrenable: vno %d for %s already allocated by %s",
			vno, v->name, vctl[vno]->name);
	}
	gicenable(irq);
	/* we assert that vectors are unshared, though irqs may be */
	v->vno = vno;
	v->type = Globalintr;
	vctl[vno] = v;
	iunlock(&vctllock);
	if (Debug)
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
	Pcidev *pcidev;

	pcidev = ic->pcidev;
	ic->vector = intrenable(ic->irq, f, ctlr,
		(pcidev? pcidev->tbdf: BUSUNKNOWN), name);
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
	gicdisable(v->vno);
	vctl[v->vno] = nil;
	iunlock(&vctllock);
	free(v);
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
		panic("trapenable: vno %d too big", vno);
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
	if (Debug)
		memset(trapcnt, 0, sizeof trapcnt);
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

/*
 * IPI: a wakeup from another cpu.
 * will break out of interruptible wfi if resumed.
 * if wfi is not interruptible, the ipi will resume it and we won't get here.
 * thus these ipis are probably due to races: we thought the target wanted
 * an ipi, but it had already changed that signal.
 */
void
trapipi(Ureg *, void *v)
{
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
#ifdef TODO
	vecacct(vctl[vctlidx(Supswintr, Localintr)]);
#endif
}

void
trapinit(void)
{
#ifdef TODO
	int vno;

	/* allocate these vectors for irqalloc counters at least */
	trapenable(vctlidx(Suptmrintr, Localintr), trapclock, nil,
		"clint clock (super)");
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
#endif

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
	tos->kcycles += t - up->kentry;
	tos->pcycles = up->pcycles;
	tos->pid = up->pid;
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
#ifdef TODO
	snprint(buf, sizeof buf, "sys: trap: %s for address %#p",
		cause < nelem(excname)? excname[cause]: name, ureg->tval);
#endif
	postnote(up, 1, buf, NDebug);
}

int
userureg(Ureg* ureg)
{
#ifdef TODO
	switch (ureg->curmode) {
	default:
		return (ureg->status & Spp) == 0;
	case Mppmach:
		return (ureg->status & Mpp) == Mppuser;
	}
#endif
	return 0;
}

static int
isfpinst(uintptr pc)
{
	int fp, n, csr;
	ulong inst;

	fp = 0;

	/* is it a compressed instruction? */
	inst = *(ulong *)pc;		/* instrs are little-endian */
	/* TODO: flesh out */
	return fp;
}

/*
 * on riscv, we had to manually advance the PC past an ECALL instruction,
 * for example, so that we don't re-execute it.
 */
static void
advancepc(Ureg *ureg)
{
	/* examine short at PC, which is sufficient to decide if compressed */
	ureg->pc += sizeof(ulong);
}

void
trapclockintr(Ureg *ureg, Clint *clnt)
{
	clockoff(clnt);
	if (++m->clockintrdepth > 1 && m->clockintrsok) {
		/* nested clock interrupt; probably shutting down */
		m->clockintrsok = 0;
		// iprint("cpu%d: nested clock interrupt\n", m->machno);
	}
	timerintr(ureg, 0);			/* nil Ureg* is risky */
	--m->clockintrdepth;
#ifdef TODO
	vecacct(vctl[vctlidx(Suptmrintr, Localintr)]);
#endif
	clockenable();
}

static int
traplocalintr(Ureg *ureg, Cause *cp)
{
	int clockintr;

	m->perf.intrts = perfticks();
#ifdef TODO
	switch (cp->cause & ~Msdiff) {		/* map mach to super codes */
	case Suptmrintr:
		m->intr++;
		trapclockintr(ureg, clintaddr());
		clockintr = 1;
		break;
	case Supswintr:
		trapipi(nil, vctl[cp->vno]);
		m->ipiwait = 1;	/* zeroed by sender, don't double count */
		clockintr = 0;
		break;
	case Supextintr:
		/* NB: intr not reached here, short-circuited in whatcause */
		clockintr = intr(ureg, cp);
		break;
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
		panic("trap: unknown local interrupt cp->cause %d", cp->cause);
		notreached();
	}
	clrsipbit(1<<cp->cause);
#endif
	intrtime(m, cp->vno);
	return clockintr;
}

static void
trapmisaligned(Ureg *ureg, Cause *cp)
{
	if (cp->user)
		posttrapnote(ureg, cp->cause, "misaligned access");
#ifdef TODO
	else
		panic("misaligned access to %#p at %#p", ureg->tval, ureg->pc);
#endif
}

/*
 * Illinst could be from legitimate user fp use while fpu is off.
 */
static void
trapillinst(Ureg *ureg, Cause *cp)
{
	int rd, funct3;
	ulong inst;
	uintptr pc;

	/* if non-zero, ureg->tval will be the trapping instruction */
	pc = ureg->pc;
	if (isfpinst(pc)) {
#ifdef TODO
		if (!cp->user)
			panic("kernel fpu use at %#p: %#p", pc, ureg->tval);
#endif
		fptrap(ureg, 0);
		vecacct(vctl[cp->vno]);
		return;
	}

	/* ignore bad CSR number.  some of them are optional. */
#ifdef TODO
	inst = UNCOMPINST(pc);
	funct3 = (inst>>12) & MASK(3);
	if ((inst & MASK(7)) == SYSTEM && (funct3 & 3) != 0) {
		/* a CSR instruction: zero dest and otherwise ignore */
		iprint("CSR instruction for bad CSR %ld at %#p\n",
			inst>>20, pc);
		rd = (inst>>7) & MASK(5);
		if (rd)
			ureg->regs[rd] = 0;
		ureg->pc += 4;
		return;
	}

	if (!cp->user)
		panic("illegal instruction at %#p: %#p", pc, ureg->tval);
#endif
	posttrapnote(ureg, cp->cause, "illegal instruction");
}

static void
trapaccess(Ureg *ureg, Cause *cp)
{
	if (cp->user)
		posttrapnote(ureg, cp->cause, "illegal access");
#ifdef TODO
	else
		panic("trap: illegal access to %#p at %#p", ureg->tval,
			ureg->pc);
#endif
}

/* even if the memory exists, it must be mapped before calling this. */
vlong
probeulong(ulong *addr, int wr)
{
	ulong old;
	Mpl pl;

	if (m == nil)
		return -1;	/* too early */
	pl = splhi();
	if (Debug) {
		iprint("probing %#p...", addr);
		delay(100);
	}
	m->probing = 1;
	m->probebad = 0;
	coherence();

	old = *addr;
	if (wr)
		*addr = old;	/* rewrite word, in hopes of doing no harm */
	coherence();		/* should fault by now if addr is bad */

	m->probing = 0;
	if (Debug) {
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

static Exchandler exchandlers[] = {
#ifdef TODO
[Breakpt]	debugbpt,
[Instpage]	faultarm64,
[Loadpage]	faultarm64,
[Storepage]	faultarm64,
[Instaccess]	trapaccess,
[Loadaccess]	trapaccess,
[Storeaccess]	trapaccess,
[Illinst]	trapillinst,
[Instaddralign]	trapmisaligned,
[Loadaddralign]	trapmisaligned,
[Storeaddralign] trapmisaligned,
[Envcalluser]	trapsyscall,		/* backstop; short-cut in trap */
#endif
		nil,
};

/* handle exceptions */
static int
traparm64(Ureg *ureg, Cause *cp)
{
	int cause;
	Exchandler handler;

	if (cp->user)
		m->turnedfpoff = 0;
	else if (m->probing) {
		m->probebad = 1;
		m->probing = 0;
		coherence();
iprint("probe trapped\n");
		/* have to advance PC on risc-v to skip faulting instruction. */
		advancepc(ureg);
		return 0;			/* not a clock interrupt */
	}

	cause = cp->cause;
	if ((uint)cause < nelem(exchandlers)) {
		handler = exchandlers[cause];
		if (handler) {
			(*handler)(ureg, cp);
			return 0;		/* not a clock interrupt */
		}
	}

	/* could be a local intr */
	panic("unknown exception, cause %d from_user %d", cause, cp->user);
	notreached();
}

/*
 * call for global interrupts (those coming from the gic).
 * these are never clock interrupts, so returns 0 (not a clockintr).
 */
int
intr(Ureg* ureg, Cause *cp)
{
	int id, ctxt, vno;
	void (*isr)(Ureg *, void *);
	Vctl *v, *vec;

	m->intr++;
	ctxt = m->gicctxt + Super;
	while ((id = gicclaim(ctxt)) != 0) {
		/* id is actual cause */
		m->perf.intrts = perfticks();
		/* cp->vno = */ vno = vctlidx(id, Globalintr);
		if (vno < 0)
			panic("intr: intr id %d out of range", id);
		vec = vctl[vno];
		if (vec == nil) {		/* maybe it's spurious? */
			gicdisable(vno);
			iprint("intr: no vector set up for intr id %d\n", id);
		} else {
			if (vec->irq)
				m->lastintr = vec->irq;
			for(v = vec; v != nil; v = v->next) {
				isr = v->f;
				if (isr == nil) {
					iprint("intr: no isr for vector %d\n",
						vno);
					continue;
				}
				(*isr)(ureg, v->a);
				splhi();	/* in case isr dropped PL */
				ainc(&v->count);
			}
		}
		giccompl(id, ctxt);
		intrtime(m, vno);
	}

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

static void
trapsyscall(Ureg *ureg, Cause *)
{
	uint scallnr;
	uintptr pc;

	m->turnedfpoff = 0;

	/* syscall may change ureg->pc, so save a copy. */
	pc = ureg->pc;
	scallnr = ureg->arg;
	/*
	 * Last argument is location of return value in frame.
	 * successful exec does not return here, but at the entry
	 * point of the new program.
	 */
	if ((getsp() % sizeof(vlong)) != 0)
		print("trapsyscall: odd sp %#p\n", getsp());
	syscall(scallnr, ureg, (Ar0 *)&ureg->ret);

	/* a changed pc could be the result of receiving a note */
	if (pc == ureg->pc)
		advancepc(ureg);
	else if (Debug && scallnr != EXEC)
		iprint("syscall %s changed return ureg->pc %#p (old pc %#p)\n",
			systab[scallnr].n, ureg->pc, pc);
}

static void
trapdbg(Ureg *ureg, Cause *cp, int entry)
{
	int type;

	type = cp->type;
	if (type != Exception)
		return;
#ifdef TODO
	iprint("|%c%c%c ", ureg->curmode == Mppsuper? 'S': 'M', entry? '>': '<',
		type == Exception? 'E': 'I');
	/* if we print uart interrupts, we'll recurse forever. */
	if (type == Exception && cp->cause < nelem(excname)) {
		iprint("%s", excname[cp->cause]);
		if (cp->cause == Envcalluser) {
			iprint(" pid %d", up->pid);
			if (entry)
				iprint(" %s", systab[ureg->arg].n);
			else
				iprint(" return %#llux", ureg->arg);
		}
	} else
		iprint("cause %d", cp->cause);
	iprint(" from %s pc %#p tval %#p up %#p\n",
		cp->user? "user": "kernel", ureg->pc, ureg->tval, up);
#endif
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
#ifdef TODO
	cause = ureg->cause & ~Rv64intr;
	if (!(ureg->cause & Rv64intr))
		type = Exception;
	else if ((cause & ~Msdiff) == Supextintr) {
		type = Globalintr;
		cause = 0;  /* actual causes will come from the gic, if any */
	} else
		type = Localintr;	/* these are very frequent */
#endif
	cp->type = type;
	cp->cause = cause;

	cp->vno = vctlidx(cause, type);
	if (cp->vno < 0)
		panic("trap: cause %d or type %d out of range", cause, type);

	/* these counts are reset every second */
	if (Debug && ++trapcnt[cp->vno] % (1024*1024) == 0) /* tweak to taste */
		iprint("trap: vno %d trapping lots\n", cp->vno);
	return type;
}

static Traphandler traphandlers[Nfaulttypes] = {
[Unknownflt]	nil,
[Exception]	traparm64,			/* may enable fpu */
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
	Cause why;

	/*
	 * if we trapped from supervisor mode into machine mode, revert to
	 * phys device addresses, assuming that we are about to reboot.
	 * this is only possible if we link with mtrap.$O (e.g., tinyemu).
	 */
#ifdef TODO
	if (ureg->curmode == Mppmach && (ureg->status & Mpp) == Mppsuper)
		m->virtdevaddrs = 0;

	whatcause(&why, ureg);
	if (Debug)
		trapdbg(ureg, &why, 1);

	/* short-cut for syscalls */
	if (ureg->cause == Envcalluser)	/* Rv64intr is off for exceptions */
		trapsyscall(ureg, nil);
		/* syscall did the whole job; we're done */
	else {
		clockintr = (*traphandlers[why.type])(ureg, &why);
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
#endif

	if (Debug)
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
	 * on arm, pc points at the instruction that caused the trap,
	 * so there's no need to back up the pc.
	 */
	snprint(buf, sizeof buf, "sys: breakpoint");
	postnote(up, 1, buf, NDebug);
}

static void
doublefault(Ureg *ureg, void*)
{
	panic("double fault; pc %#p", ureg->pc);
}

/* called if SP or SB cause a fault in strap.s */
void
badstack(void)
{
	panic("cpu%d: bad sp or sb in super trap handler", m->machno);
}

static void
faultarm64(Ureg* ureg, Cause *cp)
{
	uintptr addr;
	int read, user, insyscall;

#ifdef TODO
	addr = ureg->tval;
	user = userureg(ureg);
#endif
//	if(!user && mmukmapsync(addr))
//		return;

	vecacct(vctl[cp->vno]);
	/*
	 * There must be a user context.
	 * If not, the usual problem is causing a fault during
	 * initialisation before the system is fully up.
	 */
#ifdef TODO
	if(up == nil)
		panic("fault %#llux with up == nil; pc %#p addr %#p",
			ureg->cause, ureg->pc, addr);

	read = 1;
	switch (ureg->cause) {		/* exception, so Rv64intr must be 0 */
	case Storeaccess:
	case Storepage:
	case Storeaddralign:
		read = 0;
		break;
	}
#endif
	insyscall = up->insyscall;
	up->insyscall = 1;
	if(fault(addr, read) < 0){
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
		if(!user && (!insyscall || up->nerrlab == 0)){
			dumpregs(ureg);
			panic("fault: addr %#p pc %#p", addr, ureg->pc);
		}
		snprint(buf, sizeof buf, "sys: trap: fault %s addr=%#p",
			read? "read": "write", addr);
		postnote(up, 1, buf, NDebug);
		if(insyscall)
			error(buf);
	}
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
#ifdef TODO
	uintptr mie;

	mie = ureg->ie;
	memmove(pureg, uva, n);
	/* user shouldn't change any status bits on risc-v */
	ureg->ie = mie;
#endif
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
	if(!okaddr(nur->pc, sizeof(ulong), 0) ||
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
#ifdef TODO
	nur->status = cur->status;
//	nur->status = cur->status & ~(Spp|Sie) | Spie;
#endif
	memmove(cur, nur, sizeof(Ureg));

	switch((int)arg0){
	case NCONT:
	case NRSTR:
		vrfyuregaddrs(cur);
		up->ureg = nf->old;		/* restore original Ureg */
		/* if we interrupted a system call, advance PC */
#ifdef TODO
		if (UNCOMPINST(cur->pc) == ECALLINST)
			advancepc(cur);
#endif
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
	Mreg s;
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
