/*
 * kernel profiling device
 */
#include	"u.h"
#include	"../port/lib.h"
#include	"mem.h"
#include	"dat.h"
#include	"fns.h"
#include	"../port/error.h"

#define LRES	3		/* log of PC resolution */
#define SZ	4		/* sizeof of count cell; well known as 4 */

struct
{
	uintptr	minpc;
	uintptr	maxpc;
	int	nbuf;
	int	time;
	ulong	*buf;
}kprof;

enum{
	Kprofdirqid,
	Kprofdataqid,
	Kprofctlqid,
};
Dirtab kproftab[]={
	".",		{Kprofdirqid, 0, QTDIR},0,	DMDIR|0550,
	"kpdata",	{Kprofdataqid},		0,	0600,
	"kpctl",	{Kprofctlqid},		0,	0600,
};

Dev kprofdevtab;

static void
_kproftimer(uintptr pc)
{
	if(kprof.time == 0 || kprof.buf == nil)
		return;
	/*
	 *  if the pc is coming out of spllo or splx,
	 *  use the pc saved when we went splhi.
	 */
	if(pc >= (uintptr)spllo && pc <= (uintptr)spldone && m->splpc != 0)
		pc = m->splpc;

	/* only 1 cpu at a time updates profile due to atomics */
	aadd((int *)&kprof.buf[0], TK2MS(1));
	if(kprof.minpc<=pc && pc<kprof.maxpc){
		pc -= kprof.minpc;
		pc >>= LRES;
		aadd((int *)&kprof.buf[pc], TK2MS(1));
	}else
		aadd((int *)&kprof.buf[1], TK2MS(1));
}

static void
kprofinit(void)
{
	if(SZ != sizeof kprof.buf[0])
		panic("kprof size");
	kproftimer = _kproftimer;
}

static Chan*
kprofattach(char *spec)
{
	ulong n;

	/* allocate when first used */
	if(kprof.buf == nil) {
		kprof.minpc = KTZERO;
		kprof.maxpc = (uintptr)etext;
		/* one more in case difference isn't a multiple of 2^LRES */
		kprof.nbuf = ((kprof.maxpc-kprof.minpc) >> LRES) + 1;
		n = kprof.nbuf*SZ;
		kprof.buf = malloc(n);
		if(kprof.buf == nil)
			error(Enomem);
		kproftab[1].length = n;
	}
	return devattach(kprofdevtab.dc, spec);
}

static Walkqid*
kprofwalk(Chan *c, Chan *nc, char **name, int nname)
{
	return devwalk(c, nc, name, nname, kproftab, nelem(kproftab), devgen);
}

static long
kprofstat(Chan *c, uchar *db, long n)
{
	return devstat(c, db, n, kproftab, nelem(kproftab), devgen);
}

static Chan*
kprofopen(Chan *c, int omode)
{
	if(c->qid.type & QTDIR && omode != OREAD)
		error(Eperm);
	c->mode = openmode(omode);
	c->flag |= COPEN;
	c->offset = 0;
	return c;
}

static void
kprofclose(Chan*)
{
}

static long
kprofread(Chan *c, void *va, long n, vlong off)
{
	uchar *a, *ea;
	ulong end, offset = off;
	ulong *bp;

	switch((int)c->qid.path){
	case Kprofdirqid:
		return devdirread(c, va, n, kproftab, nelem(kproftab), devgen);

	case Kprofdataqid:
		end = kprof.nbuf*SZ;
		if(offset & (SZ-1))
			error(Ebadarg);
		if(offset >= end){
			n = 0;
			break;
		}
		if(offset+n > end)
			n = end-offset;
		n &= ~(SZ-1);
		a = va;
		ea = a + n;
		bp = kprof.buf + offset/SZ;
		while(a < ea)
			a = beputl(a, *bp++);
		break;

	default:
		n = 0;
		break;
	}
	return n;
}

static long
kprofwrite(Chan *c, void *a, long n, vlong)
{
	switch((int)(c->qid.path)){
	case Kprofctlqid:
		if(strncmp(a, "startclr", 8) == 0){
			kprof.time = 0;	/* prevent updates while zeroing */
			coherence();
			if (kprof.buf != nil)
				memset((char *)kprof.buf, 0, kprof.nbuf*SZ);
			kprof.time = 1;
		}else if(strncmp(a, "start", 5) == 0)
			kprof.time = 1;
		else if(strncmp(a, "stop", 4) == 0)
			kprof.time = 0;
		break;
	default:
		error(Ebadusefd);
	}
	return n;
}

Dev kprofdevtab = {
	'K',
	"kprof",

	devreset,
	kprofinit,
	devshutdown,
	kprofattach,
	kprofwalk,
	kprofstat,
	kprofopen,
	devcreate,
	kprofclose,
	kprofread,
	devbread,
	kprofwrite,
	devbwrite,
	devremove,
	devwstat,
};
