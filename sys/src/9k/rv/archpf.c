/*
 * icicle-specific (u54) stuff: soc init, reset
 */
#include "u.h"
#include "../port/lib.h"
#include "mem.h"
#include "dat.h"
#include "fns.h"
#include "../port/error.h"
#include "riscv.h"

enum {
	/* ctl bits */
	Devrst	= 1<<5,

	Forceresetkey = 0xdead,
	Refreshkey = 0xdeadc0de,
};

typedef struct Wdog Wdog;
struct Wdog {			/* icicle */
	ulong	refresh;
	ulong	ctl;
	ulong	sts;
	/*
	 * time, msvp & trigger are locked after writing any of above or time.
	 * set trigger, msvp, time and ctl, in that order.
	 */
	ulong	time;
	ulong	msvp;	/* must be ≤ time */
			/* MVRP: max. value up to which refresh is permitted */
	ulong	trigger;
	ulong	force;
};

static void
pfinit(void)
{
}

static void
pfreset(void)
{
	if(soc.wdog0) {
		Wdog *wdog = (Wdog *)soc.wdog0;

		wdog->trigger = 1000;
		wdog->msvp = 0;
		coherence();
		wdog->time = 1000;
		wdog->ctl = Devrst;
		// wdog->refresh = Refreshkey;
		coherence();
		splhi();
		for (;;) {
			wdog->force = Forceresetkey;
			pause();
		}
	}
}

void (*socinit)(void) = pfinit;
void (*rvreset)(void) = pfreset;
