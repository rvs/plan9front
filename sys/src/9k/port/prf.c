/*
 * dumb i8250 or sifive uart printing for last-resort debugging.
 * not strictly portable, but the i8250 is what vendors usually provide.
 */
#include	"u.h"
#include	"../port/lib.h"
#include	"mem.h"
#include	"dat.h"
#include	"fns.h"

enum {
	/* 8250 registers */
	Thr	= 0,
	Lsr	= 5,		/* line status */

	/* Thr bits */
	Thre	= 1<<5,		/* transmit hold register empty */

	/* sifive registers */
	Txdata	= 0,

	Notready= 1ul<<31,
};

int polledprint = 1;		/* flag: advertise availability of prf, etc. */
static Lock prlock = { 0 };

/*
 * take care in case &prflock is high when called from low.c with mmu off.
 */

void *
myspace(uintptr addr)
{
	return (void *)(addr & ~KZERO | getpc() & KZERO);
}

/*
 * this ilock hangs on vf2 only.  ilock used to need m set (by newmach).
 */
static void
prflock(void)
{
//	if (m)
	ilock(myspace((uintptr)&prlock));
}

static void
prfunlock(void)
{
//	if (m)
	iunlock(myspace((uintptr)&prlock));
}

static void
prmaylock(uchar c, int dolock)
{
	ulong tries;
	ulong *uartp;
	static uchar lastc = 0;

	if (c == '\n' && lastc != '\r')
		prmaylock('\r', dolock);
	uartp = (ulong *)CONSREGS();
	if (uartp == nil) {		/* true on pc, no mem-mapped regs */
		uartputc(c);
		return;
	}
	if (dolock)
		prflock();
	coherence();
#ifdef SIFIVEUART
	for (tries = 100000000; tries > 0 && uartp[Txdata] & Notready; tries++)
		pause();
	uartp[Txdata] = c;
#else
	for (tries = 100000000; tries > 0 && (uartp[Lsr] & Thre) == 0; tries++)
		pause();
	uartp[Thr] = c;
#endif
	coherence();
	lastc = c;
	if (dolock)
		prfunlock();
	microdelay(10);
}

void
pr(uchar c)
{
	prmaylock(c, 1);
}

void
prnolock(uchar c)
{
	prmaylock(c, 0);
}

void
prn(char *s, int n)
{

	if (s == nil)
		return;
	prflock();
	while (*s != '\0' && n-- > 0)
		prnolock(*s++);
	prfunlock();
}

void
prsnolock(char *s)
{
	if (s == nil)
		return;
	while (*s != '\0')
		prnolock(*s++);
}

void
prs(char *s)
{
	prflock();
	prsnolock(s);
	prfunlock();
}

static char *
prnumb(char *buf, int size, va_list *argp, int ells, int base, int hassign)
{
	int sign;
	uvlong d;
	char *s;
	static char hexdigs[] = "0123456789abcdef";

	if (ells <= 1)
		d = va_arg(*argp, ulong);
	else
		d = va_arg(*argp, uvlong);
	if(d == 0)
		return "0";
	s = buf+size;
	*--s = 0;

	if(hassign && (vlong)d < 0){
		d = -(vlong)d;
		sign = -1;
	}else
		sign = 1;
	for(; d > 0; d /= base)
		*--s = hexdigs[d%base];
	if(sign < 0)
		*--s = '-';
	return s;
}

/*
 * simple print for debugging
 */
int
prf(char *fmt, ...)
{
	int ells;
	uchar c;
	char *p, *s;
	va_list arg;
	static char buf[64];		/* minimise stack use */

	va_start(arg, fmt);
	prflock();
	for(p = fmt; *p; p++){
		if(*p != '%') {
			prnolock(*p);
			continue;
		}

		/* count 'l', ignore rest up to verb */
		ells = 0;
		for (; (c = p[1]) == 'l' || c == 'u' || c == '#' || c == '.' ||
		    c == ',' || c >= '0' && c <= '9'; p++)
			if (c == 'l')
				++ells;
		if (*++p == '\0')
			break;

		/* implement verbs */
		s = "0";		/* default output */
		switch(*p){
		case 'p':
		case 'P':
			ells = sizeof(uintptr) / sizeof(ulong);
			/* fall through */
		case 'x':
		case 'X':
			prsnolock("0x");
			s = prnumb(buf, sizeof buf, &arg, ells, 16, 0);
			break;
		case 'd':
			s = prnumb(buf, sizeof buf, &arg, ells, 10, 1);
			break;
		case 'c':
			s = buf;
			s[0] = va_arg(arg, int);
			s[1] = '\0';
			break;
		case 's':
			s = va_arg(arg, char*);
			break;
		}
		if (s)
			prsnolock(s);
	}
	va_end(arg);
	prfunlock();
	return 0;
}
