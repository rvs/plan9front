/*
 * verify that various things work as expected:
 * data segment alignment, varargs, long dereferences.
 */

#include "u.h"
#include "../port/lib.h"
#include "mem.h"
#include "dat.h"
#include "fns.h"

enum {
	Nosanity = 0,		/* flag: don't do sanity checks */
};

/* verify varargs machinery */
static void
varargck(char *fmt, ...)
{
	uint i, j;
	uchar c, c2;
	uvlong vl, vl2;
	va_list list;

	va_start(list, fmt);
	i = 0;
	if (fmt[0] == '\0')
		i = va_arg(list, uint);
	j = va_arg(list, uint);
	c  = va_arg(list, uchar);
	vl = va_arg(list, uvlong);
	c2  = va_arg(list, uchar);
	vl2 = va_arg(list, uvlong);
	va_end(list);

	if (fmt[0] == '\0')
		if (i != (1<<30))
			iprint("varargck %s int i vararg botch\n", fmt);
	if (j != (2<<28))
		iprint("varargck %s int j vararg botch\n", fmt);
	if (c != 0222)
		iprint("varargck %s char c vararg botch\n", fmt);
	if (vl != (3ll<<40 | 5<<20))
		iprint("varargck %s vlong vl vararg botch\n", fmt);
	if (c2 != 0111)
		iprint("varargck %s char c2 vararg botch\n", fmt);
	if (vl2 != (7ll<<40 | 12<<20))
		iprint("varargck %s vlong vl2 vararg botch\n", fmt);
}

static long
negfunc(void)
{
	long l = -1;

	return l;
}

static void
negint(void)
{
	long l = -1;
	long *lp;

	lp = &l;
	if (*lp >= 0)
		print("*lp >= 0!\n");
	if (negfunc() >= 0)
		print("negfunc() >= 0!\n");
}

static void
signext(void)
{
	mpdigit ui, ui2, uiz;
	mpdigit *uip;
	uvlong uvl, vl;
	static mpdigit sui;

	uiz = 0;
	sui = ~uiz;
	uip = &sui;

	ui = sui;
	if (ui != sui)
		print("uint != uint\n");
//	print("ui (1) %#ux\n", ui);
	uvl = sui;
	if (uvl != sui)
		print("uvl != uint\n");
	if ((vlong)uvl < 0)
		print("uvl < 0 (1)\n");
//	print("uvl (1) %#llux\n", uvl);

	ui = *uip;
	if (ui != sui)
		print("uint != *uintp\n");
	uvl = *uip;
	if (uvl != sui)
		print("uvl != *uintp\n");
	if ((vlong)uvl < 0)
		print("uvl < 0 (2)\n");

	vl = *uip;
	if (vl != sui)
		print("vl != *uintp\n");
//	print("vl (1) %#llux\n", vl);
	if ((vlong)vl < 0)
		print("vl < 0\n");

	if ((ui>>1) != (vl>>1))
		print("ui>>1 != vl>>1\n");
	if((ui>>1) > ui)
		print("ui>>1 > ui\n");
	ui2 = ui<<8;
	if(ui2 >= ui)
		print("ui2 %#ux >= ui %#ux\n", ui2, ui);
}

static void
clzchk(void)
{
	int i;
	Clzuint v;

	if (clz(0) != Clzbits)
		print("clz(%d) = %d\n", 0, clz(0));
	for (i = 0; i < Clzbits; i++) {
		v = 1ull << i;
		if (clz(v) != Clzbits-1-i)
			print("clz(%#lluo) = %d (vs %d)\n",
				v, clz(v), (int)Clzbits-1-i);
	}
	if (clz(~0ull) != 0)
		print("clz(~0ull) = %d\n", clz(~0ull));
}

static vlong zvl;		/* in bss */

void
sanity(void)
{
	static Lock testlock;
	static ulong align = 123456;

	if (Nosanity)
		return;
	if (align != 123456)
		panic("mis-aligned data segment; expected %#x saw %#lux",
			123456, align);
	if (zvl != 0)
		panic("mis-initialized bss; expected 0 saw %#llux", zvl);

	assert(sys != 0);
	if (PPN(KTZERO) != PPN((uintptr)_main))
		panic("KTZERO %#p != _main %#p", KTZERO, _main);
	if ((uintptr)sys < KZERO)
		panic("sys %#p < KZERO %#p", sys, (uvlong)KZERO);

	lock(&testlock);
	if (canlock(&testlock))
		panic("locks broken: acquired one twice");
	unlock(&testlock);

	varargck("", 1<<30, 2<<28, 0222, 3ll<<40 | 5<<20, 0111,
		(7ll<<40 | 12<<20));
	varargck("2", 2<<28, 0222, 3ll<<40 | 5<<20, 0111,
		(7ll<<40 | 12<<20));
	varargck("3", 2<<28, 0222, 3ll<<40 | 5<<20, 0111,
		(7ll<<40 | 12<<20), 0);
	negint();
	signext();
	clzchk();
	print("basic sanity established.\n");
}
