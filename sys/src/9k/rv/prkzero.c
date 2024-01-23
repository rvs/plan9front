/* print base addresses of kernel (virtual) and rebootcode (physical) */
#include <u.h>
#include "../port/lib.h"
#include "mem.h"
#include "dat.h"

#ifndef BANK0SIZE
#define BANK0SIZE GB
#endif

void	exits(char *);

void
main(void)
{
	print("KZERO=%#p\n", KZERO);
	/* see lowsys computation in low.c */
	print("RBKTZERO=%#p\n",
		((Sys *)(PHYSMEM + BANK0SIZE - MB))->reboottramp);
	exits(0);
}
