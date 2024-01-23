#include "../port/portfns.h"

typedef long (*Archfnp)(Chan*, void*, long, vlong);

void	aamloop(int);
Dirtab*	addarchfile(char*, int, Archfnp, Archfnp);
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
void	badminst(void);
void	badsinst(void);
void	cacheflush(void);
void	calibrate(void);
void	cgaconsputs(char*, int);
void	cgainit(void);
void	clearipi(void);
void	clockenable(void);
void	clockintr(Ureg* ureg, void *);
void	clockoff(void);
int	clocksanity(void);
void	clrreserv(void);
uintptr	clrsie(uintptr);
void	clrsipbit(ulong);
void	clrstie(void);
int	clzzbb(Clzuint);
void	coherence(void);
void	confsetenv(void);
#define CONSREGS()	(m && m->consuart? m->consuart: PAUart0) /* for prf */
#define	corecolor(sizep) 0
int	cpu2context(uint cpu);
void	cpuidinit(void);
void	cpuidprint(void);
void	cpuinit(int cpu);
#define	csrclr(csrno, new)	docsr(Csrrc, (csrno), ARG, (new))
#define	csrrd(csrno)		docsr(Csrrs, (csrno), 0, 0) /* rs 0: no write */
#define	csrset(csrno, new)	docsr(Csrrs, (csrno), ARG, (new))
#define	csrswap(csrno, new)	docsr(Csrrw, (csrno), ARG, (new))
#define	cycles(t) (*(t) = rdtsc())
int	dbgprint(char*, ...);
#define decref(r)	adec(&(r)->ref)
#define delay(ms) millidelay(ms)
uintptr	docsr(uchar op, ushort csrno, uchar rs, uintptr new);
void	drainuart(void);
void	dumpstk(void *stk);
PTE	*earlypagealloc(void);
void	ecall(...);
void	enablezerotrapcnts(void);
#define	ensurehigh(addr) (void *)((uintptr)(addr) | KZERO)
#define	ensurelow(addr)  (void *)((uintptr)(addr) & ~KZERO)
int	etherfmt(Fmt* fmt);
void	etherintr(Ureg *);
void	evenaddr(uintptr);
void*	evmap(uintptr pa, uintptr size);
void	fakecpuhz(void);
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
#define getconf(n) nil
ulong	getfcsr(void);
uintptr	getmie(void);
uintptr	getmip(void);
uintptr	getmsts(void);
void*	getmtvec(void);
uintptr getpc(void);
uintptr	getsatp(void);
uintptr getsb(void);
uintptr	getsie(void);
uintptr	getsip(void);
uintptr	getsp(void);
uintptr	getsts(void);
void*	getstvec(void);
void	gohigh(void);
void	golow(void);
void	halt(void);
void	hsmstartall(void);
void*	i8250alloc(uintptr, int, int);
#ifdef SIFIVEUART
#define i8250console sifiveconsole
Uart*	sifiveconsole(char*);
#else
Uart*	i8250console(char*);
#endif
void	idlehands(void);
void	idthandlers(void);
#define incref(r)	ainc(&(r)->ref)
void	intrall(void);
int	intrclknotrap(Ureg *);
void	intrcpu0(void);
int	intrdisable(void*);
void*	intrenable(int, void (*)(Ureg*, void*), void*, int, char*);
void	intrenableall(void);
int	intrnotrap(void);
void	invlpg(uintptr);
Ioconf*	ioconf(char *, int);
int	ipitohart(int);
int	iprint(char*, ...);
#define	isaconfig(s, i, isaconfp)	nil
#define	iskern(addr) (((uintptr)(addr) & KZERO) != 0)
int	ismemory(uintptr *va, uintptr addr);
int	isnotempty(uintptr *va);
#define	isphys(addr) (((uintptr)(addr) & KZERO) == 0)
void	jumphigh(void);
void	jumplow(void);
void	kbdenable(void);
void	kbdinit(void);
void	kexit(Ureg*);
#define	kmapinval()
void	kmesginit(void);
void	l2cacheflush(void *addr, uintptr len);
void	links(void);
uint	mach2context(Mach *);
void	machsysinit(void);
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
int	mmuwalknewpte(uintptr va, uintmem mem, int lvl, PTE attrs);
#define mmuwrpte(ptep, pte) (wbinvd(), *(ptep) = (pte))
void	monitor(void* address, ulong extensions, ulong hints);
void	mpshutdown(void);
void	mret(void);
void	mtrap(void);
void	mwait(ulong extensions, ulong hints);
Mach*	newcpupages(int machno);
void	nointrs(Intrstate *is);
void	nop(void);
int	notify(Ureg*);
uchar	nvramread(int);
void	nvramwrite(int, uchar);
void	pageidmap(void);
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
#define	perfticks() ((ulong)rdcltime())/* cheap perf. measurement ticks. need count to 1s. */
void	physmeminit(void);
void	plicdisable(uint irq);
int	plicenable(uint irq);
void	plicinit(void);
void	plicoff(void);
void	pollkproc(void *);
int	pollsechartids(void);
void	probeallhartids(void);
uintptr	probeinstr(void *insts, uintptr new, int *failedp);
vlong	probeulong(ulong *addr, int wr);
void	putmie(uintptr);
void	putmip(uintptr);
void	putmsts(uintptr);
void*	putmtvec(void *);
vlong	putsatp(uintptr);
vlong	_putsatp(uintptr);
void	putsie(uintptr);
void	putsip(uintptr);
void	putsscratch(uintptr);
void	putsts(uintptr);
void*	putstvec(void *);
uvlong	rdcltime(void);
uvlong	rdcltimecmp(Mach *mp);
uvlong	rdtime(void);
uvlong	rdtsc(void);
vlong	readmprv(ulong *);
void	recmtrapalign(void);
void	rectrapalign(void);
void	restintrs(Intrstate *is);
void	runoncpu(int cpu);
vlong	sbicall(uvlong, uvlong, uvlong, Sbiret *, uvlong *);
vlong	sbiclearipi(void);
vlong	sbiecall(uvlong, uvlong, uvlong, Sbiret *, uvlong *);
vlong	sbigetimplid(void);
vlong	sbigetimplvers(void);
vlong	sbigetmarchid(void);
vlong	sbigetmvendorid(void);
vlong	sbihartstart(uvlong hartid, uvlong phys_start, uvlong private);
vlong	sbihartstatus(uvlong hartid);
vlong	sbihartsuspend(void);
vlong	sbiprobeext(uvlong);
void	sbireset(ulong type, ulong reason);
vlong	sbisendipi(uvlong map[]);
void	sbisettimer(uvlong);
void	sbishutdown(void);
int	screenprint(char*, ...);			/* debugging */
void	setsb(void);
void	setclinttmr(uvlong clints);
void	setfcsr(ulong);
void	setkernmem(void);
uintptr	setsie(uintptr);
void	setsp(uintptr);
void	setsts(void);
void	sfence_vma(uintptr);
void*	sigsearch(char* signature);
void	(*socinit)(void);
void	strap(void);
void*	swapmtvec(void *);
void*	swapstvec(void *);
void	sync_is(void);
void*	sysexecregs(uintptr, ulong, ulong);
uintptr	sysexecstack(uintptr, int);
void	sysprocsetup(Proc*);
void	sysrforkret(void);
uintptr	topram(void);
void	touser(uintptr);
void	trapenable(int, void (*)(Ureg*, void*), void*, char*);
void	trapinit(void);
void	trapsclear(void);
void	trapvecs(void);
void	_uartputs(char *s, int n);
void	uartextintr(Ureg *reg);
void	uartsetregs(int i, uintptr regs);
void	usephysdevaddrs(void);
int	userureg(Ureg*);
void	usevirtdevaddrs(void);
PTE	va2gbit(uintptr va);
void*	vmap(uintmem, uintptr);
void	vmbotch(ulong, char *);
void	vunmap(void*, uintptr);
int	waitfor(int *vp, int val);
void	wbinvd(void);
void	wfi(void);
void	wrcltime(uvlong);
void	wrcltimecmp(uvlong);
void	writeconf(void);
void	writelmprv(ulong *, ulong);
void	wrtsc(uvlong);
void	zerotrapcnts(void);

extern int islo(void);
extern void spldone(void);
extern Mpl splhi(void);
extern Mpl spllo(void);
extern void splx(Mpl);

/* riscv atomics */
ulong	amoorw(ulong *addr, ulong bits);	/* set bits */
ulong	amonandw(ulong *addr, ulong bits);	/* clear bits */
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
 * mmu.c
 */
extern void cpusalloc(int);
