/*
 * jh7110-specific stuff (u74 for e.g., visionfive 2): soc init, reset
 */
#include "u.h"
#include "../port/lib.h"
#include "mem.h"
#include "dat.h"
#include "fns.h"
#include "../port/error.h"
#include "riscv.h"

typedef struct Wdog Wdog;
struct Wdog {
	ulong	load;		/* value to load next */
	ulong	value;		/* counts down to 0 */
	ulong	ctl;
	ulong	intclr;
	ulong	rawis;		/* raw intr sts */
	ulong	enais;		/* enabled intr sts */

	uchar	_pad1[0xc00 - 24];
	ulong	lock;		/* write 0x1acce551 to unlock regs */

	uchar	_pad2[0xf00 - 0xc04];
	ulong	intrtstctl;
	ulong	intrtstop;
};

enum {				/* ctl bits */
	Resen	= 1<<1,		/* enable reset output */
	Inten	= 1<<0,		/* enable intr output */
};

/*
 * 0x10230000	stg crg, 64KB
 * 0x10240000	stg syscon, 64KB
 * 0x13020000	system crg, 64KB
 * 0x13030000	system syscon, 64KB
 * 0x17000000	aon (always-on) crg, 64KB
 * 0x17010000	aon (always-on) syscon, 64KB
 */
enum {
	Stgcrg	= 0x10230000,
	Syscrg	= 0x13020000,
	Aoncrg	= 0x17000000,
	Aonsys	= 0x17010000,

	Jh7110clkena = 1ul<<31,
};

void
crgclkenable(ulong *rp)
{
	if ((*rp & Jh7110clkena) == 0)
		*rp |= Jh7110clkena;
}

void
crgclkenarange(ulong *rp, ulong *lastrp)
{
	while (rp <= lastrp)
		crgclkenable(rp++);
}

/*
 * ensure that all the bloody fiddly clock signals are on.
 * assumes mmu is on.  ensure clock enable bit is set in
 * many crg control registers.
 */
static void
jh7110init(void)
{
	ulong *rp, *lastrp;

	lastrp = KADDR(Stgcrg + 0x70);
	for (rp = KADDR(Stgcrg); rp <= lastrp; rp++)
		if (rp != KADDR(Stgcrg + 0x1c))
			crgclkenable(rp);
	coherence();
	*(ulong *)KADDR(Stgcrg + 0x74) = 0;	/* everybody out of reset */
	coherence();
	delay(100);			/* let effects of clocks ripple */

	crgclkenarange(KADDR(Syscrg + 0x24), KADDR(Syscrg + 0x2f4));
	coherence();
	lastrp = KADDR(Syscrg + 0x304);
	for (rp = KADDR(Syscrg + 0x2f8); rp <= lastrp; rp++)
		*rp = 0;		/* everybody out of reset */
	coherence();
	delay(100);			/* let effects of clocks ripple */

	crgclkenarange(KADDR(Aoncrg + 0x8), KADDR(Aoncrg + 0x34));
	coherence();
	*(ulong *)KADDR(Aoncrg + 0x38) = 0;	/* everybody out of reset */
	coherence();
	delay(500);			/* let effects of clocks ripple */
}

static void
jh7110reset(void)
{
	if(soc.wdog0) {
		Wdog *wd = (Wdog *)soc.wdog0;

		wd->lock = 0x1acce551;	/* unlock regs */
		coherence();
		wd->load = 0;		/* timer count */
		wd->ctl = Resen;	/* reset enable */
		coherence();
		wd->intclr = 0;		/* reload value */
		coherence();
		splhi();
		for (;;)
			halt();
	}
}

void (*socinit)(void) = jh7110init;
void (*rvreset)(void) = jh7110reset;
