/* count leading zero bits */
#include <u.h>
#include <libc.h>

#define VMASK(w) ((1ull<<(w)) - 1)

enum {
	Clzbits = 8 * sizeof(uvlong),
};

int
_clz(uvlong n)
{
	/* some 64-bit compilers generate better code for vlongs */
	uintptr cnt, hibits;
	uvlong mask;

	if (n == 0)
		return Clzbits;
	cnt = 0;
	mask = VMASK(Clzbits/2) << (Clzbits/2);
	/* this will take at most log2(Clzbits) iterations */
	for (hibits = Clzbits/2; hibits > 0; ) {
		if ((n & mask) == 0) {
			/* highest bits are zero; count and toss them */
			cnt += hibits;
			n <<= hibits;
		}
		/* halve mask width for next iteration */
		hibits /= 2;
		mask <<= hibits;
	}
	return cnt;
}
