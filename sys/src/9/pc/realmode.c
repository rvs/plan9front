#include	"u.h"
#include	"../port/lib.h"
#include	"mem.h"
#include	"dat.h"
#include	"fns.h"
#include	"io.h"
#include	"ureg.h"
#include	"../port/error.h"

/*
 * Back the processor into real mode to run a BIOS call,
 * then return.  This must be used carefully, since it
 * completely disables hardware interrupts (i.e., the i8259 and lapic)
 * while running.  It is *not* using VM86 mode.
 * Maybe that's really the right answer, but real mode
 * is fine for now.  We don't expect to use this very much --
 * just for VGA and APM.
 */

#define LORMBUF (RMBUF-KZERO)

enum { CF = 1, };		/* carry flag: indicates error in bios call */

extern void realmode0(void);	/* in lreal.s */

static Ureg rmu;
static Lock rmlock;

void
realmode(Ureg *ureg)
{
	int s;
	ulong cr3;

	if(getconf("*norealmode"))
		return;

	lock(&rmlock);
	*(Ureg*)RMUADDR = *ureg;

	/*
	 * copy l.s so that it can be run from 16-bit mode.
	 * the RMCODE page is writable because it's between KZERO and KTZERO.
	 */
	memmove((void*)RMCODE, (void*)KTZERO, BY2PG);

	s = splhi();
	m->pdb[PDX(0)] = m->pdb[PDX(KZERO)];	/* identity map low */
	cr3 = getcr3();
	putcr3(PADDR(m->pdb));
	arch->introff();
	realmode0();
	splhi();			/* who knows what the bios did */
	/*
	 * Called from memory.c before initialization of mmu.
	 * Don't turn interrupts on before the kernel is ready!
	 */
	if(m->tss)
		arch->intron();
	m->pdb[PDX(0)] = 0;	/* remove low mapping */
	putcr3(cr3);
	splx(s);

	*ureg = *(Ureg*)RMUADDR;
	unlock(&rmlock);
}

static long
rtrapread(Chan*, void *a, long n, vlong off)
{
	if(off < 0)
		error("badarg");
	if(n+off > sizeof rmu)
		n = sizeof rmu - off;
	if(n <= 0)
		return 0;
	memmove(a, (char*)&rmu+off, n);
	return n;
}

static long
rtrapwrite(Chan*, void *a, long n, vlong off)
{
	if(off || n != sizeof rmu)
		error("write a Ureg");
	memmove(&rmu, a, sizeof rmu);
	/*
	 * Sanity check
	 */
	if(rmu.trap == 0x10){	/* VBE */
		rmu.es = (LORMBUF>>4)&0xF000;
		rmu.di = LORMBUF&0xFFFF;
	}else
		error("invalid trap arguments");
	realmode(&rmu);
	return n;
}

static long
rmemrw(int isr, void *a, long n, vlong off)
{
	if(off < 0 || n < 0)
		error("bad offset/count");
	if(isr){
		if(off >= MB)
			return 0;
		if(off+n >= MB)
			n = MB - off;
		memmove(a, KADDR((ulong)off), n);
	}else{
		/* realmode buf page ok, allow vga framebuf's access */
		if(off >= MB || off+n > MB ||
		    (off < LORMBUF || off+n > LORMBUF+BY2PG) &&
		    (off < 0xA0000 || off+n > 0xB0000+0x10000))
			error("bad offset/count in write");
		memmove(KADDR((ulong)off), a, n);
	}
	return n;
}

static long
rmemread(Chan*, void *a, long n, vlong off)
{
	return rmemrw(1, a, n, off);
}

static long
rmemwrite(Chan*, void *a, long n, vlong off)
{
	return rmemrw(0, a, n, off);
}

static void
realmodediag(void)
{
	Ureg *ureg;
	Ureg uregs;

	ureg = &uregs;
	memset(ureg, 0, sizeof *ureg);
	ureg->trap = 0x15;
	/* intel removed a20 gate in 2008 */
	ureg->ax = 2401;	/* harmless: enable A20 line */
	iprint("to real mode...");
	realmode(ureg);
	if (ureg->flags & CF)
		iprint("a20-on bios call (nop) failed\n");
	else
		iprint("and back.\n");
}

void
realmodelink(void)
{
	addarchfile("realmode", 0660, rtrapread, rtrapwrite);
	addarchfile("realmodemem", 0660, rmemread, rmemwrite);
	realmodediag();
}

