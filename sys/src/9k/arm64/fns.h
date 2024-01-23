#include "../port/portfns.h"

void	aamloop(int);
Dirtab*	addarchfile(char*, int, long(*)(Chan*,void*,long,vlong), long(*)(Chan*,void*,long,vlong));
void	archfmtinstall(void);
vlong	archhz(void);
void	archinit(void);
int	archmmu(void);
void	archreset(void);
uintptr	asmalloc(uintptr, uintptr, int, uintptr);
int	asmfree(uintptr, uintptr, int);
void	asminit(void);
void	asmmapinit(uintptr, uintptr, int);
void	asmmodinit(uintptr, uintptr, char*);
void	cacheflush(void);
void	calibrate(void);
void	cgaconsputs(char*, int);
void	cgainit(void);
void	clearipi(void);
#define clintaddr()	(Clint *)0
void	clockenable(void);
void	clockintr(Ureg* ureg, void *);
void	clockoff(Clint *);
int	clocksanity(void);
void	clrsipbit(ulong);
void	clrstip(void);
void	coherence(void);
void	confsetenv(void);
#define CONSREGS()	(!m->virtdevaddrs && phyuart != 0? phyuart: uart0regs)
#define	corecolor(sizep) 0
void	cpuidinit(void);
#define	cycles(t) (*(t) = rdtsc())
int	dbgprint(char*, ...);
#define decref(r)	adec(&(r)->ref)
#define delay(ms) millidelay(ms)
void	dumpstk(void *stk);
void	ecall(...);
uintptr	ensurehigh(uintptr addr);
uintptr	ensurelow(uintptr addr);
int	etherfmt(Fmt* fmt);
void	etherintr(Ureg *);
void	evenaddr(uintptr);
void*	evmap(uintptr pa, uintptr size);
void	fpconstset(void);
void	fpoff(void);
void	fpon(void);
int	fpudevprocio(Proc*, void*, long, uintptr, int);
void	_fpuinit(void);
void	fpuinit(void);
void	fptrap(Ureg *, void *);
void	fpunoted(void);
void	fpunotify(Ureg*);
void	fpuprocrestore(Proc*);
void	fpuprocsave(Proc*);
void	fpurestore(uchar *);
void	fpusave(uchar *);
void	fpusysprocsetup(Proc*);
void	fpusysrforkchild(Proc*, Proc*);
void	fpusysrfork(Ureg*);
//char*	getconf(char*);
#define getconf(n) nil
ulong	getfcsr(void);
int	getmhartid(void);
uintptr	getmie(void);
uintptr	getmip(void);
uintptr	getmsts(void);
uintptr	getsatp(void);
uintptr	getsb(void);
uintptr	getsie(void);
uintptr	getsip(void);
uintptr	getsp(void);
uintptr	getsts(void);
void	gicoff(void);
void	gohigh(uintptr);
void	golow(uintptr);
void	halt(void);
void*	i8250alloc(uintptr, int, int);
Uart*	i8250console(char*);
void	idlehands(void);
void	idthandlers(void);
#define inb(p)		junk = p
#define incref(r)	ainc(&(r)->ref)
#define inl inb
#define ins inb
int	intrdisable(void*);
void*	intrenable(int, void (*)(Ureg*, void*), void*, int, char*);
void	invlpg(uintptr);
Ioconf*	ioconf(char *, int);
int	ipitohart(int);
int	iprint(char*, ...);
//int	isaconfig(char*, int, ISAConf*);
#define	isaconfig(s, i, isap)	nil
int	ismemory(uintptr *va, uintptr addr);
int	isnotempty(uintptr *va);
void	jumphigh(void);
void	jumplow(void);
int	junk;
void	kbdenable(void);
void	kbdinit(void);
void	kexit(Ureg*);
#define	kmapinval()
void	kmesginit(void);
void	l2cacheflush(void *addr, uintptr len);
void	links(void);
void	main(int);
void	mboptinit(char*);
#define	memcolor(addr, sizep) 0
void	meminit(void);
void	mfence(void);
void	mmudump(uintptr);
void	mmuflushtlb(Page *);
void	mmuidentitymap(void);
void	mmuinitap(void);
void	mmuinit(void);
void	mmulowidmap(void);
uintptr	mmuphysaddr(uintptr);
int	mmuwalk(uintptr, int, PTE**, uintptr (*)(uintptr));
int	mmuwalknewpte(uintptr va, uintmem mem, int lvl);
void	mmuwrpte(PTE *ptep, PTE pte);
void	monitor(void* address, ulong extensions, ulong hints);
void	mpshutdown(void);
void	mret(void);
void	mtrap(void);
void	mwait(ulong extensions, ulong hints);
void	nop(void);
int	notify(Ureg*);
uchar	nvramread(int);
void	nvramwrite(int, uchar);
#define outb(p, v)	do { junk = 0; USED(junk, p, v); } while (0)
#define outl	outb
#define outs	outb
void	pageidmap(void);
int	pagingmode2bits(uvlong pagingmode);
int	pagingmode2levels(uvlong pagingmode);
void	pause(void);
ulong	pcibarsize(Pcidev*, int);
int	pcicfgr16(Pcidev*, int);
int	pcicfgr32(Pcidev*, int);
int	pcicfgr8(Pcidev*, int);
void	pcicfgw16(Pcidev*, int, int);
void	pcicfgw32(Pcidev*, int, int);
void	pcicfgw8(Pcidev*, int, int);
void	pciclrbme(Pcidev*);
int	pciclrcfgbit(Pcidev *p, int reg, ulong bit, char *offmsg);
void	pciclrioe(Pcidev*);
void	pciclrmwi(Pcidev*);
int	pcigetmsi(Pcidev *p, Msi *msi);
int	pcigetmsixcap(Pcidev *p);
int	pcigetpciecap(Pcidev *p);
int	pcigetpms(Pcidev*);
void	pcihinv(Pcidev*);
void	pciintrs(Pcidev*);
uchar	pciipin(Pcidev*, uchar);
Pcidev*	pcimatch(Pcidev*, int, int);
Pcidev*	pcimatchtbdf(int);
void	pcimsioff(Vctl*, Pcidev*);
void	pcinointrs(Pcidev*);
void	pcireset(void);
int	pciscan(int, Pcidev**);
void	pcisetbme(Pcidev*);
int	pcisetcfgbit(Pcidev *p, int reg, ulong bit, char *onmsg);
void	pcisetioe(Pcidev*);
int	pcisetmsi(Pcidev *p, Msi *msi);
void	pcisetmwi(Pcidev*);
int	pcisetpms(Pcidev*, int);
#define	perfticks() ((ulong)rdtsc()) /* performance measurement ticks. must be low overhead. doesn't have to count over 1 s. */
vlong	probeulong(ulong *addr, int wr);
void	prsnolock(char *s);
void	putmcounteren(uintptr);
void	putmie(uintptr);
void	putmip(uintptr);
void	putmscratch(uintptr);
void	putmsts(uintptr);
void	putmtvec(uintptr);
void	putsatp(uintptr);
void	putsb(uintptr);
void	putsscratch(uintptr);
void	putsie(uintptr);
void	putsip(uintptr);
void	putsp(uintptr);
void	putsts(uintptr);
#define	putstvectodo(x)	0
uvlong	rdtsc(void);
void	runoncpu(int cpu);
uvlong	sampletimer(int tmrport, int *cntp);
int	screenprint(char*, ...);			/* debugging */
void	setclinttmr(Clint *clnt, uvlong clints);
void	setfcsr(ulong);
void	setkernmem(void);
void	setsb(void);
void*	sigsearch(char* signature);
void	strap(void);
void*	sysexecregs(uintptr, ulong, ulong);
uintptr	sysexecstack(uintptr, int);
void	sysprocsetup(Proc*);
void	sysrforkret(void);
uintptr	topram(void);
void	touser(uintptr);
void	trapclockintr(Ureg *, Clint *);
void	trapenable(int, void (*)(Ureg*, void*), void*, char*);
void	trapinit(void);
void	trapsclear(void);
void	trapvecs(void);
void	_uartputs(char *s, int n);
void	uartextintr(Ureg *reg);
void	uartsetregs(int i, uintptr regs);
int	userureg(Ureg*);
PTE	va2gbit(uintptr va);
void*	vmap(uintmem, uintptr);
void	vmbotch(ulong, char *);
void	vunmap(void*, uintptr);
int	waitfor(int *vp, int val);
void	wbinvd(void);
void	writeconf(void);
void	wrtsc(uvlong);
void	zerotrapcnts(void);

extern int islo(void);
extern void spldone(void);
extern Mpl splhi(void);
extern Mpl spllo(void);
extern void splx(Mpl);

/* riscv atomics */
ulong	amoorw(ulong *addr, ulong bits);
ulong	amoswapw(ulong *addr, ulong nv);

/* libc atomics */
int	_tas(int*);
int	cas(uint*, int, int);

#define CASW		cas
#define TAS		_tas

#define	waserror()	(up->nerrlab++, setlabel(&up->errlab[up->nerrlab-1]))

#define PTR2UINT(p)	((uintptr)(p))
#define UINT2PTR(i)	((void*)(i))

void*	KADDR(uintptr);
uintptr	PADDR(void*);

/*
 * archrv.c
 */
extern void millidelay(int);

/*
 * cpustart.c
 */
extern void cpusalloc(int);
