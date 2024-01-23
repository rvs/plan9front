/*
 * log2 and pow2 routines.
 */
#include "u.h"
#include "../port/lib.h"
#include "mem.h"
#include "dat.h"
#include "fns.h"

/*
 * these routines should be cheap enough that there will
 * be no hesitation to use them.
 *
 * once 5c in-lines vlong ops, just use the vlong versions.
 */

int
countbits(ulong u)
{
	int n;

	for (n = 0; u != 0; n++)
		/* clear highest set bit */
		u &= ~(1u << (BI2BY*sizeof(Clzuint) - 1 - clz(u)));
	return n;
}

int
ispow2(uvlong uvl)
{
	return ISPOW2(uvl);
}

static int
isulpow2(ulong ul)				/* temporary speed hack */
{
	return ISPOW2(ul);
}

/*
 * return exponent of smallest power of 2 ≥ n
 */
int
log2(ulong n)
{
	int i;

	i = BI2BY * sizeof(Clzuint) - 1 - clz(n);
	if (n == 0 || !ISPOW2(n))
		i++;
	return i;
}
