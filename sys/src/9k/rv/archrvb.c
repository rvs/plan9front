/*
 * rvb-specific (xuantie c910) stuff: soc init, reset
 */
#include "u.h"
#include "../port/lib.h"
#include "mem.h"
#include "dat.h"
#include "fns.h"
#include "../port/error.h"
#include "riscv.h"

static void
rvbreset(void)
{
	if(soc.wdog0) {
		ulong *wd = (ulong *)soc.wdog0;

		wd[0] = 0x5ada7200;
		wd[4] = 0;
		coherence();
		splhi();
		for (;;)
			halt();
	}
}

void (*rvreset)(void) = rvbreset;
