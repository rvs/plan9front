/*
 * #P/cputype, shutdown and timing.
 */
#include "u.h"
#include "../port/lib.h"
#include "mem.h"
#include "dat.h"
#include "fns.h"
#include "../port/error.h"
#include "arm64.h"

#include <ctype.h>

enum {
	Qdir = 0,
	Qbase,

	Qmax = 16,

	Debug = 0,
};

typedef long Rdwrfn(Chan*, void*, long, vlong);

static Rdwrfn *readfn[Qmax];
static Rdwrfn *writefn[Qmax];

static Dirtab archdir[Qmax] = {
	".",		{ Qdir, 0, QTDIR },	0,	0555,
};
Lock archwlock;	/* the lock is only for changing archdir */
int narchdir = Qbase;

extern Dev archdevtab;

/*
 * Add a file to the #P listing.  Once added, you can't delete it.
 * You can't add a file with the same name as one already there,
 * and you get a pointer to the Dirtab entry so you can do things
 * like change the Qid version.  Changing the Qid path is disallowed.
 */
Dirtab*
addarchfile(char *name, int perm, Rdwrfn *rdfn, Rdwrfn *wrfn)
{
	int i;
	Dirtab d;
	Dirtab *dp;

	memset(&d, 0, sizeof d);
	strcpy(d.name, name);
	d.perm = perm;

	lock(&archwlock);
	if(narchdir >= Qmax){
		unlock(&archwlock);
		return nil;
	}

	for(i=0; i<narchdir; i++)
		if(strcmp(archdir[i].name, name) == 0){
			unlock(&archwlock);
			return nil;
		}

	d.qid.path = narchdir;
	archdir[narchdir] = d;
	readfn[narchdir] = rdfn;
	writefn[narchdir] = wrfn;
	dp = &archdir[narchdir++];
	unlock(&archwlock);
	return dp;
}

static Chan*
archattach(char* spec)
{
	return devattach(archdevtab.dc, spec);
}

Walkqid*
archwalk(Chan* c, Chan *nc, char** name, int nname)
{
	return devwalk(c, nc, name, nname, archdir, narchdir, devgen);
}

static long
archstat(Chan* c, uchar* dp, long n)
{
	return devstat(c, dp, n, archdir, narchdir, devgen);
}

static Chan*
archopen(Chan* c, int omode)
{
	return devopen(c, omode, archdir, narchdir, devgen);
}

static void
archclose(Chan*)
{
}

enum
{
	Linelen= 31,
};

static long
archread(Chan *c, void *a, long n, vlong offset)
{
	Rdwrfn *fn;

	switch((ulong)c->qid.path){

	case Qdir:
		return devdirread(c, a, n, archdir, narchdir, devgen);
	default:
		if(c->qid.path < narchdir && (fn = readfn[c->qid.path]))
			return fn(c, a, n, offset);
		error(Eperm);
		notreached();
	}
}

static long
archwrite(Chan *c, void *a, long n, vlong offset)
{
	Rdwrfn *fn;

	switch((ulong)c->qid.path){
	default:
		if(c->qid.path < narchdir && (fn = writefn[c->qid.path]))
			return fn(c, a, n, offset);
		error(Eperm);
		notreached();
	}
}

Dev archdevtab = {
	'P',
	"arch",

	devreset,
	devinit,
	devshutdown,
	archattach,
	archwalk,
	archstat,
	archopen,
	devcreate,
	archclose,
	archread,
	devbread,
	archwrite,
	devbwrite,
	devremove,
	devwstat,
};

void
nop(void)
{
}

static long
cputyperead(Chan*, void *a, long n, vlong off)
{
	char str[32];

	snprint(str, sizeof(str), "%s %ud\n", cputype, m->cpumhz);
	return readstr(off, a, n, str);
}

void
archinit(void)
{
	addarchfile("cputype", 0444, cputyperead, nil);
}

void
archreset(void)
{
	cacheflush();
	spllo();
	delay(1);
	if (rvreset)
		rvreset();
	iprint("last resort: into wfi.\n");
	for (;;)
		halt();
}

static void
haltsys(void)
{
	int i;

	iprint("mpshutdown: %d active cpus\n", sys->nonline);
	delay(5);
	devtabshutdown();
	splhi();

	/* paranoia: wait for other cpus */
	for (i = 100; i > 0 && sys->nonline > 1; i--) {
		iprint(" %d", sys->nonline);
		delay(200);
	}

	/* we can now use the uniprocessor reset */
	archreset();
	notreached();
}

/*
 * secondary cpus should all idle (here or in shutdown, called from reboot).
 * for a true shutdown, reset and boot, cpu 0 should then reset itself or the
 * system as a whole.  if plan 9 is rebooting (loading a new kernel and
 * starting it), cpu 0 needs to keep running to do that work, so we return
 * in that case only.
 *
 * we assume shutdown has previously been called on this cpu.
 */
void
mpshutdown(void)
{
	void (*tramp)(Reboot *);

	if(m->machno == 0) {
		static QLock mpshutdownlock;

		qlock(&mpshutdownlock);
		/* we are the first proc on cpu0 to begin shutdown */
		if(!active.rebooting) {
			haltsys();
			notreached();
		}
	} else {
		/*
		 * we're secondary, we should go into sys->reboottramp to enter
		 * wfi.  if idling, sys->Reboot won't be set and that's okay.
		 */
		cacheflush();
		mmulowidmap();			/* disables user mapping too */
		m->virtdevaddrs = 0;
		/* await populated sys->Reboot */
		/* secstall is cleared by schedcpus, settrampargs from reboot */
//		while (sys->secstall == RBFLAGSET)
//			coherence();
		tramp = (void (*)(Reboot *))PADDR(sys->reboottramp);
		(*tramp)(&sys->Reboot);
		notreached();
	}
}

/*
 * there are two main timers: the system-wide clint clock (mtime)
 * and the per-core cpu clock (rdtsc).  The clint clock is typically
 * something like 1 or 10 MHz and the cpu clock 600 - 1200 MHz.
 */

/*
 *  return value and frequency of timer (via hz)
 */
uvlong
fastticks(uvlong* hz)
{
	uvlong clticks;

	if(hz != nil)
		*hz = timebase;
	coherence();
//	clticks = RDCLTIME(clintaddr());
	if (sys->epoch == 0)
		return clticks;
	if (clticks >= sys->epoch)
		clticks -= sys->epoch;		/* clticks since boot */
	else
		iprint("fastticks: cpu%d: clticks %,lld < sys->epoch %,lld\n",
			m->machno, clticks, sys->epoch);
	return clticks;
}

ulong
µs(void)
{
	return fastticks2us(fastticks(nil));
}

void
setclinttmr(Clint *clnt, uvlong clticks)
{
	/* seems not to work under OpenSBI, though undocumented */
//	WRCLTIMECMP(clnt, clticks);
	coherence();	/* Stip might not be extinguished immediately */
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
		idlepause = 1;
		if (Debug)
			print("clocks are wonky or emulated; using PAUSE to idle.\n");
		return 1;
	}
	return 0;
}

static void
timeop(Clint *clint, char *name, void (*op)(Clint *))
{
	int i;
	uvlong tsc, mtime, tsc2, mtime2, diffsumtsc, diffsumclint;

//	RDTIMES(clint, tsc, mtime);
	for (i = 0; i < 1000; i++)
		op(clint);
//	RDTIMES(clint, tsc2, mtime2);

	diffsumtsc = tsc2 - tsc;
	diffsumclint = mtime2 - mtime;

	if (Debug)
		print("op %s:\t%d took %,lld tsc cycles and %,lld clint ticks\n",
			name, i, diffsumtsc, diffsumclint);
	areclockswonky(diffsumtsc, diffsumclint);
}

static void
nullop(Clint *)
{
}

static void
rdtimeop(Clint *clint)
{
//	vlong junk;

//	junk = RDCLTIME(clint);
//	USED(junk);
}

static void
wrtimeop(Clint *clint)
{
//	WRCLTIME(clint, RDCLTIME(clint));
}

static void
wrtimecmpop(Clint *clint)
{
//	WRCLTIMECMP(clint, VMASK(63));
}

vlong
vlabs(vlong vl)
{
	return vl >= 0? vl: -vl;
}

/*
 * compute timing parameters, turn off WFI use if clocks are inconsistent.
 */
void
calibrate(void)
{
	uvlong tsc, mtime, tsc2, mtime2, tscperclint;
	Clint *clint;

	if (sys->clintsperhz != 0)
		return;
	clint = clintaddr();
//	RDTIMES(clint, tsc, mtime);
	delay(100);
//	RDTIMES(clint, tsc2, mtime2);

	tscperclint = 1;
	if (mtime2 > mtime) {
		tscperclint = (tsc2 - tsc) / (mtime2 - mtime);
		if ((vlong)tscperclint < 1)
			tscperclint = 1;
	}
	tscperclintglob = tscperclint;
	mtime2 -= mtime;
	tsc2 -= tsc;
	if (Debug)
		print("%,llud clint ticks is %,llud rdtsc cycs, or %lld cycs/clint\n",
			mtime2, tsc2, tscperclint);
	areclockswonky(tsc2, mtime2);

	print("timebase given as %,llud Hz\n", timebase);

	/* compute constants for use by timerset & idle code */
	sys->clintsperhz = timebase / HZ;	/* clint ticks per HZ */
	sys->clintsperµs = timebase / 1000000;
	/*
	/* min. interval until intr; was /(100*HZ) but made too many intrs.
	 * Does minclints need to be less than timebase/HZ?  It allows
	 * shorter and more precise sleep intervals, e.g., for clock0link
	 * polling.  To keep the interrupt load and interactive response
	 * manageable, it needs to be somewhat > 0.
	 */
	sys->minclints = timebase / (5*HZ);

	/* time various clock-related operations.  primarily debugging. */
	timeop(clint, "null", nullop);
	timeop(clint, "read time", rdtimeop);
	timeop(clint, "set time", wrtimeop);
	timeop(clint, "set timecmp", wrtimecmpop);
}

void
clockoff(Clint *clint)
{
//	putsie(getsie() & ~Stie);
	/* ~0ull makes sense, but looks negative on some machines */
	setclinttmr(clint, VMASK(63));
//	clrstip();
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
	Clint *clint;

	pl = splhi();
	if (sys->clintsperhz == 0)
		panic("timerset: sys->clintsperhz not yet set");
	if (next == 0)
		fticks = sys->clintsperhz;
	else {
		fticks = next - fastticks(nil);
		/* enforce sane bounds */
		if (fticks < (vlong)sys->minclints)
			/* don't interrupt immediately */
			fticks = sys->minclints;
		else if (fticks > (vlong)sys->clintsperhz)
			fticks = sys->clintsperhz;
	}

	clint = clintaddr();
	clockoff(clint);		/* don't trigger early */
//	setclinttmr(clint, RDCLTIME(clint) + fticks);
	clockenable();
	splx(pl);
}
