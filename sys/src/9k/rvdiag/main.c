/*
 * initialise a risc-v RV64G system, start all cpus, and
 * start scheduling processes on them, notably /boot/boot as process 1.
 * also contains graceful shutdown and reboot code.
 */
#include "u.h"
#include "../port/lib.h"
#include "mem.h"
#include "dat.h"
#include "fns.h"

#include "io.h"
#include "ureg.h"
#include "riscv.h"

#include "init.h"
#include "reboot.h"
#include <a.out.h>
#include <ctype.h>

// #include "../port/dbgprint.h"

enum {
	Debug = 0,		/* flag: prompt for debug letters */
};

#ifdef SIFIVEUART
#define UARTGETC() sifivegetc(&sifiveuart[0])

int	sifivegetc(Uart *);

Uart	sifiveuart[];
#else
#define UARTGETC() i8250getc(&i8250uart[0])

int	i8250getc(Uart *);

Uart	i8250uart[];
#endif

extern char defnvram[];		/* from kernel config */

int	hartcnt = 0;		/* machno allocator; init to avoid bss */

Sys* sys = nil;			/* initializer keeps sys out of bss */
uintptr sizeofSys = sizeof(Sys);
uintptr sizeofSyspercpu = sizeof(Syspercpu);

char cant[] = "can't happen: notreached reached\n";
char dbgflg[256] = { 0, };
char cputype[] = "riscv64";
Watchdog *watchdog = nil;

/*
 * Option arguments from the command line (obsolete).
 * oargv[0] is the boot file (/boot/boot).
 * mboptinit() was called from multiboot() on amd64 to set it all up.
 */
static int oargc;
static char* oargv[20];
static char oargb[128];
static int oargblen;

static uintptr usp;		/* user stack of init proc */

/* dreg from parsing multiboot options on pcs */
void
mboptinit(char* s)
{
	oargblen = strecpy(oargb, oargb+sizeof(oargb), s) - oargb;
	oargc = tokenize(oargb, oargv, nelem(oargv)-1);
	oargv[oargc] = nil;
}

void
fakecpuhz(void)
{
	/*
	 * Need something for initial delays
	 * until a timebase is worked out.
	 */
	m->cpuhz = cpuhz;
	m->cpumhz = cpuhz/1000000;
	m->perf.period = 1;
}

static vlong
sethz(void)
{
	vlong hz;

	hz = archhz();
	if(hz != 0){
		m->cpuhz = hz;
		m->cpumhz = hz/1000000;
	}
	return hz;
}

static void
setepoch(void)
{
	if (m->machno == 0 && sys->epoch == 0) {
		sys->ticks = m->ticks = 0;
		/* clint->mtime is only zeroed upon reset on some systems */
		if (nosbi)
			wrcltime(1);		/* attempt to reset */
		coherence();
		sys->epoch = rdcltime();
	}
}

static void
mptimesync(void)
{
	if (m->machno == 0) {
		sys->timesync = rdcltime();
		coherence();
	}
}

/*
 * start a cpu other than 0.  entered splhi with initial id map page table
 * and with PC in KZERO.
 */
void
squidboy(uint machno)
{
	lock(&active);
	cpuactive(machno);
	unlock(&active);

	/*
	 * starting here, we are using kernel addresses, and they should
	 * be upper addresses, but won't be until mmuinitap.
	 */
	archmmu();			/* work out page sizes available */
	assert(m->npgsz >= 1);

	/* mmuinitap switches to private copy of cpu0's page table */
	mmuinitap();	/* returns executing in high addresses with vmap */

	DBG("Hello Squidboy %d\n", machno);

	/*
	 * Beware the Curse of The Non-Interruptable Were-Temporary.
	 */
	if(sethz() == 0) {
		print("squidboy: cpu%d: 0Hz\n", machno);
		notreached();
	}
	cpuidprint();

	fpuinit();

	/*
	 * Handshake with cpu to let it know the startup succeeded.
	 */
	m->splpc = 0;			/* unused */
	coherence();

	/*
	 * Handshake with main to proceed with initialisation.
	 */
	DBG("mach %d waiting for epoch\n", machno);
	/* sys->timesync is set on cpu0 in mptimesync from multiprocinit (1) */
	while(sys->timesync == 0)
		pause();
	mptimesync();

	/*
	 * Cannot allow interrupts while waiting for online.
	 * A clock interrupt here might call the scheduler,
	 * and that would be a mistake.
	 */
	DBG("mach%d: waiting for online flag\n", machno);
	while(!m->online)  /* set on cpu0 in onlinewaiting from schedcpus (2) */
		pause();
	DBG("mach%d: online color %d\n", machno, m->color);

	pollsechartids();

	DBG("mach %d is go %#p %#p\n", machno, m, m->ptroot->va);
	timersinit();	/* uses m->cpuhz; set up HZ timers on this cpu */

	clocksanity();			/* enables clock & allows clock intrs */
	DBG("[cpu%d scheding after %lld cycles]\n", machno,
		rdtsc() - m->boottsc);

	delay(machno*100); /* stagger startup & let cpu0 get to schedinit 1st */
	setsie(Superie);
	setsts();

	schedinit();			/* no return */
	panic("cpu%d: schedinit returned", machno);
}

/*
 * for cpu 0 only.  sets m->machno and sys->machptr[], and
 * needs 'up' to be set to nil.  Mach already zeroed by low.c.
 */
void
machsysinit(void)
{
	m->machno = 0;
	sys->machptr[m->machno] = &sys->mach;
	m->stack = PTR2UINT(sys->machstk);
	sys->nonline = 0;
	cpuactive(0);
	m->online = 1;
	sys->copymode = 0;			/* COW */
}

static int
onlinewaiting(int machno)
{
	Mach *mp;

	mp = sys->machptr[machno];
	if(mp == nil || !mp->waiting || mp->online)
		return 0;
	if (mp->hartid == 0 && soc.hobbled)	/* boot hart? */
		return 0;

	/* cpu is waiting but not yet on-line, so bring it up */
	mp->color = corecolor(machno);
	if(mp->color < 0)
		mp->color = 0;
	mp->online = 1;
	mp->waiting = 0;
	coherence();

	DBG("%d on with mach %#p...", machno, mp);
	return 1;
}

/*
 * Release the hounds.  After boot, secondary cpus will be
 * looping waiting for cpu0 to signal them.
 */
static void
schedcpus(int ncpus)
{
	int machno, running, tries;

	running = 1;			/* cpu0 */
	USED(running, ncpus);
	if (MACHMAX <= 1 || running == ncpus)
		return;

	/* if rebooting, let other cpus jump to entry from reboottramp */
	sys->secstall = 0;  /* let them start then poll m->online */
	coherence();
	delay(50);
	sys->secstall = 0;
	coherence();
	/*
	 * even on a fairly slow machine, a cpu can start in under .6 s.
	 * the range on 600MHz icicle with debugging prints:
	 * 236244054-323670622 cycles = .39-.54 s.
	 * be tolerant of delays due to debug printing or staggered startups.
	 */
	for (tries = 0; running < ncpus && tries < 150; tries++) {
		delay(100);	/* let them indicate presence; be patient */
		for(machno = 1; machno < MACHMAX; machno++) /* skip me (cpu0) */
			running += onlinewaiting(machno);
	}
	iprint("%d cpus running, %d missing\n", running, ncpus - running);
}

/*
 * allocate memory for and awaken any other cpus, if needed.
 * copy the page table root to a private copy for each cpu.
 * hartcnt global is incremented in start.s by each hart.
 */
static void
multiprocinit(void)
{
	int ncpus;

	/*
	 * during a reboot, secondary cpus will stall until we're ready.
	 * at cold boot, we may have to start them.
	 */
	if (havesbihsm && sys->rebooting != RBFLAGSET)
		hsmstartall();
	sys->rebooting = 0;

	/*
	 * when we get here, hartcnt should be correct and secondary
	 * cpus should be running, if only spinning while waiting.
	 */
	ncpus = hartcnt;
	cpusalloc(ncpus);
	mptimesync();		/* signal secondaries via sys->timesync (1) */
	schedcpus(ncpus);	/* signal secondaries via m->online (2) */
	/* the third synchronization step is in main, after userinit */

	probeallhartids();
}

static void
debuginit(void)
{
	int c;

	if (!Debug)
		return;
	iprint("debug letters: ");
	while ((c = UARTGETC()) != '\n' && c != '\r') {
		dbgflg[c] = 1;
		iprint("%c", c);		/* echo */
	}
}

/*
 * early initialization after zeroing BSS.  most of this set up is
 * for system-wide resources, thus only done once.
 */
static void
cpu0init(void)
{
//	if (sys->rebooting == RBFLAGSET)
//		iprint("system is rebooting.\n");
	plicoff();
	physmeminit();
	/* we know physical memory size and end (sys->pmend) here */

	mboptinit("/boot/ipconfig ether");	/* dreg from pc multiboot */

	/* device discovery hasn't happened yet, so be careful. */
	i8250console("0");
	fmtinit();			/* install P, L, R, N, Q verbs */
	debuginit();
	/*
	 * if we linked with prf.$O, logging will only start once
	 * mallocinit is called from meminit.
	 */
	kmesginit();
	/* cpuactive(0) is now called by machsysinit */
	active.exiting = 0;

	/* if epoch is set after timersinit, timers will be confused */
	setepoch();		/* get notions of time right here */
	sethz();		/* also enables l2 cache */
	calibrate();

	/*
	 * Mmuinit before meminit because it makes mappings and flushes
	 * the TLB.  It will get page table pages from Sys as needed.
	 */
	setkernmem();
	mmuinit();	/* uses kernmem; sets sys->vm*; vmaps soc devices */
	meminit();		/* map KZERO to ram, call mallocinit */
	archinit();		/* populate #P with cputype */
	trapinit();
	if (socinit)
		socinit();	/* turn on bloody clock signals */

	/*
	 * Printinit should cause the first malloc call to happen (printinit->
	 * qopen->malloc).  If the system dies here, it's probably due to malloc
	 * not being initialised correctly, or the data segment is misaligned
	 * (it's amazing how far you can get with things like that completely
	 * broken).
	 */
	printinit();	/* actually, establish cooked console input queue */

	pcireset();		/* turn off bus masters & intrs */
}

static void
chooseidler(void)
{
	print("WFI,");
	if (!soc.idlewake)
		print(" not");
	print(" sending IPIs over %lld ns.\n", sys->nsthresh);
}

static void
prearlypages(void)
{
	if (sys->freepage)
		iprint("%lud/%d early pages allocated (probably by vmap)\n",
			(ulong)(((char *)sys->freepage - (char *)sys->pts)/PGSZ),
			EARLYPAGES);
}

/*
 * copy reboot trampoline function to its expected location.
 * a reboot could be requested at any time, and all cores will
 * jump to the trampoline.  some of them may still be executing there,
 * so only overwrite if necessary.
 */
static void
copyreboottramp(void)
{
	if (memcmp(sys->reboottramp, rebootcode, 250) != 0)
		memmove(sys->reboottramp, rebootcode, sizeof(rebootcode));
	cacheflush();
}

enum {
	Memstride = 256*MB,
};

/*
 * this is not entirely safe; if it crashes, don't call it.
 * nevertheless, it can be a help when documentation is unclear.
 *
 * probing in super mode could work if sbi or hw doesn't intercept access
 * traps, or forwards them to super.
 */
static void
probemem(void)
{
	uintptr addr, start, end;

	if (TODO && !bootmachmode)	/* only probe in machine mode */
		return;
	print("memory map to %N: ", ADDRSPCSZ);
	for (start = end = addr = membanks[0].addr; addr < ADDRSPCSZ;
	    addr += Memstride) {
		if (probeulong((ulong *)addr, Read) < 0)
			break;
		end = addr = ROUNDDN(addr + sizeof(long), Memstride) - sizeof(long);
	}
	print("%#p-%#p size %N\n", start, end, end + sizeof(long) - start);
}

static int
ismissing(void *addr, char *name)
{
	int ret;

	ret = probeulong(addr, Read) < 0;
	if (ret)
		print(" no");
	print(" %s", name);
	return ret;
}

static void
probeclint(void)
{
	print("clint direct access:");
	if (ismissing(m->clint->msip, "msip,"))
		soc.ipiclint = 0;
	ismissing(&m->clint->mtime, "mtime,");
	ismissing(m->clint->mtimecmp, "mtimecmp");
	print(".\n");
}

/* are unaligned accesses visible to us? */
static void
misalignedtrap(void)
{
	vlong dummy, rd;

	dummy = 0x04030201;
	rd = (ulong)~dummy; USED(rd);
	rd = probeulong((ulong *)((char *)&dummy + 1), Read);
	if (rd >= 0)
		print("no ");
	print("super trap on unaligned access for %#p, read %#llux\n",
		(char *)&dummy + 1, rd);
}

extern char *kerndatestr;

static void
prcpucfg(void)
{
	print("\n");
	if (kerndatestr)
		print("built %s\n", kerndatestr);
	sanity();
	cpuidprint();
	if (bootmachmode)		/* see what stuck after delegation */
		print("mideleg %#p medeleg %#p ", mideleg, medeleg);
	print("senvcfg %#p\n", csrrd(SENVCFG));	/* 0 if csr missing */

	print("using %d-bit virtual addresses, can exploit %N bytes, "
		"and idling with ", VMBITS, ADDRSPCSZ);
	chooseidler();
}

/*
 * entered in supervisor mode with paging on, both low identity map
 * and KZERO->0 map in effect, and PC in KZERO space.
 * using initial page table until pageidmap then mmuinit (from cpu0init)
 * runs on cpu0, or mmuinitap runs on other cpus, which copies page table
 * root from cpu0.
 *
 * fakecpuhz has already been called in low.c.
 */
void
main(int cpu)
{
	uintptr caller;

	cpuinit(cpu);
	if (cpu != 0) {
		wfi();
		notreached();
	}
	/*
	 * this calls kmesginit but if we linked with prf.$O, logging will
	 * only start once mallocinit is called from cpu0init.
	 */
	cpu0init();
	/* we know physical memory size and end (sys->pmend) here */
	/* now logging console output to kmesg */

	caller = getcallerpc(&cpu);
	if (isuseraddr(caller))
		print("called from low memory: %#p\n", caller);
	prcpucfg();

	probeclint();
	timersinit();	/* uses m->cpuhz; set up HZ timers on this cpu */
	clocksanity();		/* enables the clock */
	enablezerotrapcnts();
	misalignedtrap();

	fpuinit();
	psinit();
	initimage();

	probemem();		/* useful when debugging on new hardware */
	links();

	print("diagnostic done.\n");
	wfi();
}

/*
 * process 1 runs this (see userinit), so it's now okay for devtabinit to
 * spawn kprocs.
 */
void
init0(void)
{
	char buf[2*KNAMELEN];

	up->nerrlab = 0;

	spllo();

	/*
	 * These are o.k. because rootinit is null.
	 * Then early kproc's will have a root and dot.
	 */
	up->slash = namec("#/", Atodir, 0, 0);
	pathclose(up->slash->path);
	up->slash->path = newpath("/");
	up->dot = cclone(up->slash);

	devtabinit();

	if(!waserror()){
		snprint(buf, sizeof(buf), "%s %s", cputype, conffile);
		ksetenv("terminal", buf, 0);
		ksetenv("cputype", cputype, 0);
		if(cpuserver)
			ksetenv("service", "cpu", 0);
		else
			ksetenv("service", "terminal", 0);
		ksetenv("nvram", defnvram, 0);
		ksetenv("nobootprompt", "tcp", 0);
//		confsetenv();  /* no config to convert; see ../k10/bootconf.c */
		poperror();
	}
	kproc("alarm", alarmkproc, 0);
	if (soc.poll)
		kproc("poll", pollkproc, 0);
	/*
	 * start user phase executing initcode[] from init.h, compiled
	 * from init9.c (main) and ../port/initcode.c (startboot),
	 * which in turn execs /boot/boot.
	 *
	 * usp is a result of bootargs.
	 */
	setsie(Superie);
	setsts();
	if (getsts() & Uie)
		print("user interrupts enabled in S; can't be disabled\n");
	clockenable();
	touser(usp);
}

uintptr
bootargs(uintptr base)
{
	int i;
	uintptr ssize;
	char **av, *p;

	/*
	 * Push the boot args onto the stack.
	 * Make sure the validaddr check in syscall won't fail
	 * because there are fewer than the maximum number of
	 * args by subtracting sizeof(up->arg).
	 */
	i = oargblen+1;
	p = (void *)STACKALIGN(base + PGSZ - sizeof(up->arg) - i);
	memmove(p, oargb, i);

	/*
	 * Now push argc and the argv pointers.
	 * This isn't strictly correct as the code jumped to by
	 * touser in init9.[cs] calls startboot (port/initcode.c) which
	 * expects arguments
	 * 	startboot(char* argv0, char* argv[])
	 * not the usual (int argc, char* argv[]), but argv0 is
	 * unused so it doesn't matter (at the moment...).
	 * Added +1 to ensure nil isn't stepped on, another for vlong padding.
	 */
	av = (char**)(p - (oargc+2+1+1)*sizeof(char*));
	ssize = base + PGSZ - PTR2UINT(av);
	*av++ = (char*)oargc;
	for(i = 0; i < oargc; i++)
		*av++ = (oargv[i] - oargb) + (p - base) + (USTKTOP - PGSZ);
	*av = nil;
	return STACKALIGN(USTKTOP - ssize);
}

void
userinit(void)
{
	Proc *p;
	Segment *s;
	KMap *k;
	Page *pg;

	p = newproc();
	p->pgrp = newpgrp();
	p->egrp = smalloc(sizeof(Egrp));
	p->egrp->ref = 1;
	p->fgrp = dupfgrp(nil);
	p->rgrp = newrgrp();
	p->procmode = 0640;

	kstrdup(&eve, "");
	kstrdup(&p->text, "*init*");
	kstrdup(&p->user, eve);

	/*
	 * Kernel Stack
	 *
	 * N.B. make sure there's enough space for syscall to check
	 *	for valid args and
	 *	space for gotolabel's return PC
	 */
	p->sched.pc = PTR2UINT(init0);	/* proc 1 starts here in kernel phase */
	p->sched.sp = PTR2UINT(p->kstack+KSTACK-sizeof(up->arg));
	p->sched.sp = STACKALIGN(p->sched.sp);

	/*
	 * User Stack
	 *
	 * Technically, newpage can't be called here because it
	 * should only be called when in a user context as it may
	 * try to sleep if there are no pages available, but that
	 * shouldn't be the case here.
	 */
	s = newseg(SG_STACK, USTKTOP-USTKSIZE, USTKTOP);
	p->seg[SSEG] = s;
	pg = newpage(Zeropage, s, USTKTOP-(1LL<<s->lg2pgsize), 0);
	segpage(s, pg);
	k = kmap(pg);
	usp = bootargs(VA(k));
	/* usp will be init0's stack pointer via touser */
	kunmap(k);

	/*
	 * Text
	 */
	s = newseg(SG_TEXT, UTZERO, UTZERO+PGSZ);
	s->flushme++;
	p->seg[TSEG] = s;
	pg = newpage(Zeropage, s, UTZERO, 0);
	memset(pg->cachectl, PG_TXTFLUSH, sizeof(pg->cachectl));
	segpage(s, pg);
	k = kmap(s->map[0]->pages[0]);
	memmove((void *)VA(k), initcode, sizeof initcode);
	kunmap(k);

	wbinvd();
	/* not using address-space ids currently */
	putsatp(pagingmode | (m->ptroot->pa / PGSZ));

	ready(p);
}

void
drainuart(void)
{
	int i;

	if (islo())
		for (i = 300; i > 0 && consactive(); i--)
			delay(10);
	else
		delay(10);
}

/* shutdown this cpu */
static void
shutdown(int ispanic)
{
	int ms, once;

	/* simplify life by shutting off any watchdog */
	if (watchdogon && watchdog) {
		watchdog->disable();
		watchdogon = 0;
	}

	lock(&active);
	if(ispanic)
		active.ispanic = ispanic;
	else if(m->machno == 0 && !iscpuactive(0))
		active.ispanic = 0;		/* reboot */
	once = iscpuactive(m->machno);
	/*
	 * setting exiting will make hzclock() on each processor call exit(0),
	 * which calls shutdown(0) and mpshutdown(), which idles non-bootstrap
	 * cpus and returns on bootstrap processors (to permit a reboot).
	 * clearing our bit in active.machsmap avoids calling exit(0) from
	 * hzclock() on this processor.
	 */
	cpuinactive(m->machno);
	active.exiting = 1;
	unlock(&active);

	if(once)
		iprint("cpu%d: %s...", m->machno, m->machno? "idling": "exiting");
	spllo();
	for(ms = 5*1000; ms > 0; ms -= TK2MS(2)){
		delay(TK2MS(2));
		if(sys->nonline <= 1 && consactive() == 0)
			break;
	}

	cacheflush();
	m->clockintrsok = 0;
	if(active.ispanic && m->machno == 0){
		if(cpuserver)
			delay(2000);
		else
			for(;;)
				idlehands();
	} else
		delay(1000);
}

/* will be called from hzclock if active.exiting is true */
void
exit(int ispanic)
{
	iprint("cpu%d: exit...", m->machno);
	shutdown(ispanic);
	mpshutdown();
	/* only cpu0 gets here, and only on reboot */
	archreset();
}

static int
okkernel(int magic)
{
	return magic == Y_MAGIC || magic == B_MAGIC;
}

int (*isokkernel)(int) = okkernel;

/*
 * if we have to reschedule, up must be set (i.e., we must be in a
 * process context).
 */
void
runoncpu(int cpu)
{
	if (m->machno == cpu)
		return;			/* done! */
	if (up == nil)
		panic("runoncpu: nil up");
	if (up->nlocks)
		print("runoncpu: holding locks, so sched won't work\n");
	procwired(up, cpu);
	sched();
	if (m->machno != cpu)
		iprint("cpu%d: can't switch proc to cpu%d\n", m->machno, cpu);
}

static void
shutothercpus(void)
{
	intrcpu0();
	/*
	 * the other cpus could be holding locks that will never get
	 * released (e.g., in the print path) if we put them into
	 * reset now, so ask them to shutdown gracefully.
	 * once active.rebooting is set, any or all
	 * of the other cpus may be idling but not servicing interrupts.
	 */
	sys->secstall = RBFLAGSET; /* stall sec cores 'til sys->Reboot ready */
	lock(&active);
	active.rebooting = 1;		/* request other cpus shutdown */
	unlock(&active);
	shutdown(Shutreboot);

	/* any intrs to other cpus will not be delivered hereafter */
}

static void
settrampargs(void *phyentry, void *code, long size)
{
	/* set up args for trampoline */
	sys->tramp = (void (*)(Reboot *))PADDR(sys->reboottramp);
	/*
	 * jl by default doesn't produce an extended header, so entry is
	 * only 32 bits.
	 */
	sys->phyentry = (uintptr)ensurelow(phyentry);
	sys->phycode = PADDR(code);
	sys->rebootsize = size;
	coherence();
	sys->secstall = 0;  /* let cpus in mpshutdown proceed to trampoline */
	coherence();
}

/*
 * shutdown this kernel, jump to trampoline code in Sys, which copies
 * the next kernel (size @ code) into the addresses it was linked for,
 * and jumps to the new kernel's entry address.
 *
 * put other harts into wfi in trampoline in sys, jump into
 *	id map, jump to trampoline code which copies new kernel into place,
 *	start all harts running it.
 */
void
reboot(void *phyentry, void *code, long size)
{
	if (m->machno != 0 && up)
		runoncpu(0);

	/* other cpus may be idling; make them jump to trampoline */
	sys->rebooting = RBFLAGSET;		/* for new kernel */
	if (sys->nonline > 1)
		shutothercpus();		/* calls shutdown */

	if (prstackmax)
		prstackmax();

	/* there's no config and nowhere to store it */
	drainuart();

	/*
	 * interrupts (including uart) may be routed to any or all cpus, so
	 * shutdown devices, other cpus, and interrupts (rely upon iprint
	 * hereafter).
	 */
	devtabshutdown();
	drainuart();		/* before stopping cpus & interrupts */

	/*
	 * should be the only processor scheduling now.
	 * any intrs to other cpus will not be delivered hereafter.
	 */
	memset(active.machsmap, 0, sizeof active.machsmap);
	splhi();
	clockoff();
	plicoff();
	pcireset();			/* disable pci bus masters & intrs */
	putsie(0);
	clrsipbit(~0);

	/* we've been asked to just `halt'? */
	if (phyentry == 0 && code == 0 && size == 0) {
		spllo();
		archreset();	/* we can now use the uniprocessor reset */
		notreached();
	}

	mmulowidmap();				/* disables user mapping too */
	settrampargs(phyentry, code, size);	/* clears sys->secstall */

	delay(100);			/* let secondaries settle */
	while (sys->nonline > 1) {	/* paranoia */
		iprint("%d cpus...", sys->nonline);
		delay(50);
		coherence();
	}

	/* possibly won't be seen */
	iprint("\nstarting new kernel via tramp @ %#p (%#p <- %#p size %,ld)\n",
		sys->tramp, sys->phyentry, sys->phycode,
		(ulong)sys->rebootsize);
	/*
	 * if we were entered in machine mode, we can get back to it from
	 * supervisor mode with an ECALL.  otherwise, we can't get back to it
	 * at all.  the reboot trampoline will adjust the entry point suitably.
	 */
	m->bootmachmode = bootmachmode;		/* for rebootcode.s */
	if (bootmachmode)
		/*
		 * to machine mode to trigger reboot with sys->Reboot args.
		 * in machine mode, it's unsafe to use sys.
		 */
		ecall();			/* see totramp in mtrap.s */
	else
		/* jump into the trampoline in the id map - never to return */
		(*sys->tramp)(&sys->Reboot);
	notreached();
}
