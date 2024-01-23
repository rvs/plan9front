/*
 * 8250 UART and almost-compatibles driver.
 */
#include "u.h"
#include "../port/lib.h"
#include "mem.h"
#include "dat.h"
#include "fns.h"
#include "io.h"

enum {
	Change_speed	= 0,		/* flag: set baud rate */

	Uart0		= PAUart0,
};

enum {					/* registers */
	Rbr		= 0,		/* Receiver Buffer (RO) */
	Thr		= 0,		/* Transmitter Holding (WO) */
	Ier		= 1,		/* Interrupt Enable */
	Iir		= 2,		/* Interrupt Identification (RO) */
	Fcr		= 2,		/* FIFO Control (WO) */
	Lcr		= 3,		/* Line Control */
	Mcr		= 4,		/* Modem Control */
	Lsr		= 5,		/* Line Status */
	Msr		= 6,		/* Modem Status */
	Scr		= 7,		/* Scratch Pad, missing in 8250 */
	Usr		= 0x1f,		/* status; synopsys dw 8250 only */
	Dll		= 0,		/* Divisor Latch LSB */
	Dlm		= 1,		/* Divisor Latch MSB */
};

enum {					/* Ier */
	Erda		= 0x01,		/* Enable Received Data Available */
	Ethre		= 0x02,		/* Enable Thr Empty */
	Erls		= 0x04,		/* Enable Receiver Line Status */
	Ems		= 0x08,		/* Enable Modem Status */
};

enum {					/* Iir */
	Ims		= 0x00,		/* Ms interrupt */
	Ip		= 0x01,		/* Interrupt Pending (not) */
	Ithre		= 0x02,		/* Thr Empty */
	Irda		= 0x04,		/* Received Data Available */
	Irls		= 0x06,		/* Receiver Line Status */
	Ictoi		= 0x0C,		/* Character Time-out Indication */
	IirMASK		= 0x3F,
	Ifena		= 0xC0,		/* FIFOs enabled */
	Ibusy		= 0x07,		/* wrote lcr while busy; dw only */
};

enum {					/* Fcr */
	FIFOena		= 0x01,		/* FIFO enable */
	FIFOrclr	= 0x02,		/* clear Rx FIFO */
	FIFOtclr	= 0x04,		/* clear Tx FIFO */
	FIFO1		= 0x00,		/* Rx FIFO trigger level 1 byte */
	FIFO4		= 0x40,		/*	4 bytes */
	FIFO8		= 0x80,		/*	8 bytes */
	FIFO14		= 0xC0,		/*	14 bytes */
};

enum {					/* Lcr */
	Wls5		= 0x00,		/* Word Length Select 5 bits/byte */
	Wls6		= 0x01,		/*	6 bits/byte */
	Wls7		= 0x02,		/*	7 bits/byte */
	Wls8		= 0x03,		/*	8 bits/byte */
	WlsMASK		= 0x03,
	Stb		= 0x04,		/* 2 stop bits */
	Pen		= 0x08,		/* Parity Enable */
	Eps		= 0x10,		/* Even Parity Select */
	Stp		= 0x20,		/* Stick Parity, aka Spar */
	Brk		= 0x40,		/* Break */
	Dlab		= 0x80,		/* Divisor Latch Access Bit */
};

enum {					/* Mcr */
	Dtr		= 0x01,		/* Data Terminal Ready */
	Rts		= 0x02,		/* Ready To Send */
	Out1		= 0x04,		/* no longer in use */
	Ie		= 0x08,		/* IRQ Enable */
	Dm		= 0x10,		/* Diagnostic Mode loopback */
};

enum {					/* Lsr */
	Dr		= 0x01,		/* Data Ready */
	Oe		= 0x02,		/* Overrun Error */
	Pe		= 0x04,		/* Parity Error */
	Fe		= 0x08,		/* Framing Error */
	Bi		= 0x10,		/* Break Interrupt */
	Thre		= 0x20,		/* Thr Empty */
	Temt		= 0x40,		/* Transmitter Empty */
	FIFOerr		= 0x80,		/* error in receiver FIFO */
};

enum {					/* Msr */
	Dcts		= 0x01,		/* Delta Cts */
	Ddsr		= 0x02,		/* Delta Dsr */
	Teri		= 0x04,		/* Trailing Edge of Ri */
	Ddcd		= 0x08,		/* Delta Dcd */
	Cts		= 0x10,		/* Clear To Send */
	Dsr		= 0x20,		/* Data Set Ready */
	Ri		= 0x40,		/* Ring Indicator */
	Dcd		= 0x80,		/* Data Set Ready */
};

enum {					/* Usr */
	Busy		= 1<<0,	/* serial xfr in progress; don't write Lcr */
};

typedef struct Ctlr {
	Intrcommon;
	int	defirq;
	ulong	*io;		/* mapped va */
	ulong	*phyio;		/* unmapped pa */
	ulong	tbdf;
	int	iena;
	int	poll;
	int	unit;

	uchar	sticky[8];

	Lock;
	int	hasfifo;
	int	checkfifo;
	int	fena;
} Ctlr;

extern PhysUart i8250physuart;

static Ctlr i8250ctlr[] = {
{	.io	= (ulong *)Uart0,
	.phyio	= (ulong *)Uart0,
	.tbdf	= -1,
	.unit	= 0,
	.poll	= 0, },
#ifdef UART1				/* 2nd uart */
{	.io	= (ulong *)Uart1,
	.phyio	= (ulong *)Uart1,
	.tbdf	= -1,
	.unit	= 1,
	.poll	= 0, },
#endif
};

/* static */
Uart i8250uart[] = {
{	.regs	= &i8250ctlr[0],
	.name	= "eia0",
	.phys	= &i8250physuart,
	.special= 0,
#ifdef UART1				/* 2nd uart */
	.next	= &i8250uart[1], },
{	.regs	= &i8250ctlr[1],
	.name	= "eia1",
	.phys	= &i8250physuart,
	.special= 0,
#endif
	.next	= nil, },
};

Uart *consuart = &i8250uart[0];

#define regbase(c)	((ulong *)(m->virtdevaddrs? (c)->io: (c)->phyio))
#define csr8r(c, r)	(coherence(), regbase(c)[r])
#define csr8w(c, r, v)	(regbase(c)[r] = (c)->sticky[r] | (v), coherence())
#define csr8o(c, r, v)	(regbase(c)[r] = (v), coherence())

/* wait for uart to be not busy before writing Lcr to avoid Ibusy interrupt */
static void
lcrwait(Ctlr *ctlr)
{
	if (soc.dwuart)
		while (csr8r(ctlr, Usr) & Busy)
			;
}

void
uartsetregs(int i, uintptr regs)
{
	Ctlr *ctlr;
	Uart *uart;

	if((uint)i >= nuart) {
		print("uartconsole: no uart%d\n", i);
		return;
	}
	uartregs[i] = regs;
	uart = &i8250uart[i];
	ctlr = uart->regs;
	if (ctlr == nil)
		panic("uartsetregs: uart%d: nil uart->regs", i);
	ctlr->io = (ulong *)regs;
	if (consuart == nil) {		/* set consuart on first use */
		consuart = uart;
		uart->console = 1;
	}
}

static long
i8250status(Uart* uart, void* buf, long n, long offset)
{
	char *p;
	Ctlr *ctlr;
	uchar ier, lcr, mcr, msr;

	ctlr = uart->regs;
	p = malloc(READSTR);
	mcr = ctlr->sticky[Mcr];
	msr = csr8r(ctlr, Msr);
	ier = ctlr->sticky[Ier];
	lcr = ctlr->sticky[Lcr];
	snprint(p, READSTR,
		"b%d c%d d%d e%d l%d m%d p%c r%d s%d i%d\n"
		"dev(%d) type(%d) framing(%d) overruns(%d) "
		"berr(%d) serr(%d)%s%s%s%s\n",

		uart->baud,
		uart->hup_dcd,
		(msr & Dsr) != 0,
		uart->hup_dsr,
		(lcr & WlsMASK) + 5,
		(ier & Ems) != 0,
		(lcr & Pen) ? ((lcr & Eps) ? 'e': 'o'): 'n',
		(mcr & Rts) != 0,
		(lcr & Stb) ? 2: 1,
		ctlr->fena,

		uart->dev,
		uart->type,
		uart->ferr,
		uart->oerr,
		uart->berr,
		uart->serr,
		(msr & Cts) ? " cts": "",
		(msr & Dsr) ? " dsr": "",
		(msr & Dcd) ? " dcd": "",
		(msr & Ri) ? " ring": ""
	);
	n = readstr(offset, buf, n, p);
	free(p);

	return n;
}

/* wait for the output fifo to drain, but don't wait forever */
static void
drainfifo(Ctlr *ctlr)
{
	int loops;

	for(loops = 1000; --loops > 0 && !(csr8r(ctlr, Lsr) & Temt); )
		microdelay(10);
}

static void
i8250fifo(Uart* uart, int level)
{
	Ctlr *ctlr;

	ctlr = uart->regs;
	if(ctlr->hasfifo == 0)
		return;

	/*
	 * Changing the FIFOena bit in Fcr flushes data
	 * from both receive and transmit FIFOs; there's
	 * no easy way to guarantee not losing data on
	 * the receive side, but it's possible to wait until
	 * the transmitter is really empty.
	 */
	drainfifo(ctlr);
	ilock(ctlr);
	drainfifo(ctlr);

	/*
	 * Set the trigger level, default is the max.
	 * value.
	 * Some UARTs require FIFOena to be set before
	 * other bits can take effect, so set it twice.
	 */
	ctlr->fena = level;
	switch(level){
	case 0:
		break;
	case 1:
		level = FIFO1|FIFOena;
		break;
	case 4:
		level = FIFO4|FIFOena;
		break;
	case 8:
		level = FIFO8|FIFOena;
		break;
	default:
		level = FIFO14|FIFOena;
		break;
	}
	csr8w(ctlr, Fcr, level);
	coherence();
	csr8w(ctlr, Fcr, level);
	iunlock(ctlr);
}

static void
i8250dtr(Uart* uart, int on)
{
	Ctlr *ctlr;

	/*
	 * Toggle DTR.
	 */
	ctlr = uart->regs;
	if(on)
		ctlr->sticky[Mcr] |= Dtr;
	else
		ctlr->sticky[Mcr] &= ~Dtr;
	csr8w(ctlr, Mcr, 0);
}

static void
i8250rts(Uart* uart, int on)
{
	Ctlr *ctlr;

	/*
	 * Toggle RTS.
	 */
	assert(uart);
	ctlr = uart->regs;
	assert(ctlr);
	if(on)
		ctlr->sticky[Mcr] |= Rts;
	else
		ctlr->sticky[Mcr] &= ~Rts;
	csr8w(ctlr, Mcr, 0);
}

static void
i8250modemctl(Uart* uart, int on)
{
	Ctlr *ctlr;

	ctlr = uart->regs;
	ilock(&uart->tlock);
	if(on){
		ctlr->sticky[Ier] |= Ems;
		csr8w(ctlr, Ier, ctlr->sticky[Ier]);
		uart->modem = 1;
		uart->cts = csr8r(ctlr, Msr) & Cts;
	}
	else{
		ctlr->sticky[Ier] &= ~Ems;
		csr8w(ctlr, Ier, ctlr->sticky[Ier]);
		uart->modem = 0;
		uart->cts = 1;
	}
	iunlock(&uart->tlock);

	/* modem needs fifo */
	(*uart->phys->fifo)(uart, on);
}

static int
i8250parity(Uart* uart, int parity)
{
	int lcr;
	Ctlr *ctlr;

	ctlr = uart->regs;
	lcr = ctlr->sticky[Lcr] & ~(Eps|Pen);

	switch(parity){
	case 'e':
		lcr |= Eps|Pen;
		break;
	case 'o':
		lcr |= Pen;
		break;
	case 'n':
		break;
	default:
		return -1;
	}
	ctlr->sticky[Lcr] = lcr;
	lcrwait(ctlr);
	csr8w(ctlr, Lcr, 0);

	uart->parity = parity;

	return 0;
}

static int
i8250stop(Uart* uart, int stop)
{
	int lcr;
	Ctlr *ctlr;

	ctlr = uart->regs;
	lcr = ctlr->sticky[Lcr] & ~Stb;

	switch(stop){
	case 1:
		break;
	case 2:
		lcr |= Stb;
		break;
	default:
		return -1;
	}
	ctlr->sticky[Lcr] = lcr;
	lcrwait(ctlr);
	csr8w(ctlr, Lcr, 0);

	uart->stop = stop;

	return 0;
}

static int
i8250bits(Uart* uart, int bits)
{
	int lcr;
	Ctlr *ctlr;

	ctlr = uart->regs;
	lcr = ctlr->sticky[Lcr] & ~WlsMASK;

	switch(bits){
	case 5:
		lcr |= Wls5;
		break;
	case 6:
		lcr |= Wls6;
		break;
	case 7:
		lcr |= Wls7;
		break;
	case 8:
		lcr |= Wls8;
		break;
	default:
		return -1;
	}
	ctlr->sticky[Lcr] = lcr;
	lcrwait(ctlr);
	csr8w(ctlr, Lcr, 0);

	uart->bits = bits;

	return 0;
}

static int
i8250baud(Uart* uart, int baud)
{
	ulong bgc;
	Ctlr *ctlr;

	/*
	 * Set the Baud rate by calculating and setting the Baud rate
	 * Generator Constant. This will work with fairly non-standard
	 * Baud rates.
	 */
	if(baud <= 0)
		return -1;
	if (Change_speed) {
		bgc = (uartfreq+8*baud-1)/(16*baud);

		ctlr = uart->regs;
		lcrwait(ctlr);
		csr8w(ctlr, Lcr, Dlab);
		csr8o(ctlr, Dlm, bgc>>8);
		csr8o(ctlr, Dll, bgc);
		csr8w(ctlr, Lcr, 0);

		uart->baud = baud;
	}
	USED(uart);
	return 0;
}

static void
i8250break(Uart* uart, int ms)
{
	Ctlr *ctlr;

	/*
	 * Send a break.
	 */
	if(ms <= 0)
		ms = 200;

	ctlr = uart->regs;
	csr8w(ctlr, Lcr, Brk);
	if (up == nil)
		delay(ms);
	else
		tsleep(&up->sleep, return0, 0, ms);
	csr8w(ctlr, Lcr, 0);
}

static void
i8250kick(Uart* uart)
{
	int i;
	Ctlr *ctlr;

	if(uart->cts == 0 || uart->blocked || uart->oq == nil)
		return;

	ctlr = uart->regs;

	/*
	 *  128 here is an arbitrary limit to make sure
	 *  we don't stay in this loop too long.  If the
	 *  chip's output queue is longer than 128, too
	 *  bad -- presotto
	 */
	for(i = 0; i < 128; i++){
		if(!(csr8r(ctlr, Lsr) & Thre))
			break;
		if(uart->op >= uart->oe && uartstageoutput(uart) == 0)
			break;
		csr8o(ctlr, Thr, *uart->op++);
		coherence();
	}
	if(i < 128 && (csr8r(ctlr, Lsr) & Temt))
		ctlr->sticky[Ier] &= ~Ethre;
	else
		ctlr->sticky[Ier] |= Ethre;
	csr8w(ctlr, Ier, 0);
}

static Intrsvcret
i8250interrupt(Ureg*, void* arg)
{
	Ctlr *ctlr;
	Uart *uart;
	int iir, lsr, old, r, nrx;
	static Lock intrlock;

	uart = arg;
	if (uart == nil)
		return;		// panic("i8250interrupt: nil uart");
	ctlr = uart->regs;
	if (ctlr == nil)
		return;		// panic("i8250interrupt: nil ctlr");
	ilock(&intrlock);
	for(iir = csr8r(ctlr, Iir); !(iir & Ip); iir = csr8r(ctlr, Iir)){
		switch(iir & IirMASK){
		case Ims:		/* Ms interrupt */
			r = csr8r(ctlr, Msr);
			if(r & Dcts){
				ilock(&uart->tlock);
				old = uart->cts;
				uart->cts = r & Cts;
				if(old == 0 && uart->cts)
					uart->ctsbackoff = 2;
				iunlock(&uart->tlock);
			}
		 	if(r & Ddsr){
				old = r & Dsr;
				if(uart->hup_dsr && uart->dsr && !old)
					uart->dohup = 1;
				uart->dsr = old;
			}
		 	if(r & Ddcd){
				old = r & Dcd;
				if(uart->hup_dcd && uart->dcd && !old)
					uart->dohup = 1;
				uart->dcd = old;
			}
			break;
		case Ithre:		/* Thr Empty */
			uartkick(uart);
			break;
		case Irda:		/* Received Data Available */
		case Irls:		/* Receiver Line Status */
		case Ictoi:		/* Character Time-out Indication */
			/*
			 * Consume any received data.
			 * If the received byte came in with a break,
			 * parity or framing error, throw it away;
			 * overrun is an indication that something has
			 * already been tossed.
			 */
			nrx = 0;
			lsr = csr8r(ctlr, Lsr);
			/* tinyemu doesn't set Dr and has only a single char */
			while(soc.tinyemuuart || lsr & Dr) {
				if(lsr & (FIFOerr|Oe))
					uart->oerr++;
				if(lsr & Pe)
					uart->perr++;
				if(lsr & Fe)
					uart->ferr++;
				r = csr8r(ctlr, Rbr);
				if(!(lsr & (Bi|Fe|Pe)) && r)
					uartrecv(uart, r);
				nrx++;
				if (soc.tinyemuuart)
					break;
				lsr = csr8r(ctlr, Lsr);
			}
			/*
			 * Fix for synopsys 8250 bug: if Dr not set, read Rbr
			 * to clear interrupt and discard bogus char.
			 */
			if (nrx == 0) {
				r = csr8r(ctlr, Rbr);
				USED(r);
			}
			break;
		case Ibusy:
			r = csr8r(ctlr, Usr);	/* dismiss */
			USED(r);
			break;

		default:
			iprint("weird uart interrupt %#2.2ux\n", iir);
			break;
		}
	}
	iunlock(&intrlock);
}

static void
i8250disable(Uart* uart)
{
	Ctlr *ctlr;

	/*
	 * Turn off DTR and RTS, disable interrupts and fifos.
	 */
	(*uart->phys->dtr)(uart, 0);
	(*uart->phys->rts)(uart, 0);
	(*uart->phys->fifo)(uart, 0);

	/* if uart is our console, this will stop (i)print output */
	ctlr = uart->regs;
	ctlr->sticky[Ier] = 0;
	csr8w(ctlr, Ier, ctlr->sticky[Ier]);

	if(ctlr->iena != 0){
		if(intrdisable(ctlr->vector) == 0)
			ctlr->iena = 0;
	}
}

static void
i8250enable(Uart* uart, int ie)
{
	Ctlr *ctlr;

	ctlr = uart->regs;
	if (ctlr->io == 0)
		return;
	/*
	 * Check if there is a FIFO.
	 * Changing the FIFOena bit in Fcr flushes data
	 * from both receive and transmit FIFOs; there's
	 * no easy way to guarantee not losing data on
	 * the receive side, but it's possible to wait until
	 * the transmitter is really empty.
	 * Also, reading the Iir outwith i8250interrupt()
	 * can be dangerous, but this should only happen
	 * once, before interrupts are enabled.
	 */
	if(!ctlr->checkfifo)
		drainfifo(ctlr);
	ilock(ctlr);
	if(!ctlr->checkfifo){
		/*
		 * Wait until the transmitter is really empty.
		 */
		drainfifo(ctlr);
		csr8w(ctlr, Fcr, FIFOena);
		if(csr8r(ctlr, Iir) & Ifena)
			ctlr->hasfifo = 1;
		csr8w(ctlr, Fcr, 0);
		ctlr->checkfifo = 1;
	}
	iunlock(ctlr);

	/*
	 * Enable interrupts and turn on DTR and RTS.
	 * Be careful if this is called to set up a polled serial line
	 * early on not to try to enable interrupts as interrupt-
	 * -enabling mechanisms might not be set up yet.
	 */
	if(ie){
		if(ctlr->iena == 0 && !ctlr->poll){
			ctlr->irq = ioconf("uart", ctlr->unit)->irq;
			ctlr->tbdf = BUSUNKNOWN;
			enableintr(ctlr, i8250interrupt, uart, uart->name);
			ctlr->iena = 1;
		}
		ctlr->sticky[Ier] = Ethre|Erda;
		ctlr->sticky[Mcr] |= Ie;
	}
	else{
		ctlr->sticky[Ier] = 0;
		ctlr->sticky[Mcr] = 0;
	}
	csr8w(ctlr, Ier, ctlr->sticky[Ier]);
	csr8w(ctlr, Mcr, ctlr->sticky[Mcr]);

	(*uart->phys->dtr)(uart, 1);
	(*uart->phys->rts)(uart, 1);

	/*
	 * During startup, the i8259 interrupt controller is reset.
	 * This may result in a lost interrupt from the i8250 uart.
	 * The i8250 thinks the interrupt is still outstanding and does not
	 * generate any further interrupts. The workaround is to call the
	 * interrupt handler to clear any pending interrupt events.
	 * Note: this must be done after setting Ier.
	 */
	if(ie)
		i8250interrupt(nil, uart);
	return Intrunconverted;
}

void*
i8250alloc(uintptr io, int irq, int tbdf)
{
	Ctlr *ctlr;

	if((ctlr = malloc(sizeof(Ctlr))) != nil){
		ctlr->io = (void *)io;
		ctlr->phyio = (void *)io;
		ctlr->irq = irq;
		ctlr->tbdf = tbdf;
	}

	return ctlr;
}

static Uart*
i8250pnp(void)
{
	int i;
	Ctlr *ctlr;
	Uart *head, *uart;

	head = i8250uart;
	for(i = 0; i < nelem(i8250uart) && i < nuart; i++){
		/*
		 * Does it exist?
		 * Should be able to write/read the Scratch Pad
		 * (except on COM1, where it seems to confuse the 8250)
		 * and reserve the I/O space.
		 */
		uart = &i8250uart[i];
		ctlr = uart->regs;
		if (ctlr == nil) {
			print("i8250pnp: no Ctlr for uart %d\n", i);
			continue;
		}
		if (ctlr->io == 0) {
			print("i8250pnp: nil Ctlr->io for uart %d\n", i);
			continue;
		}
		if(uart == head)
			head = uart;
		else
			(uart-1)->next = uart;
	}

	return head;
}

/* static */
int
i8250getc(Uart* uart)
{
	Ctlr *ctlr;

	ctlr = uart->regs;
	while(!(csr8r(ctlr, Lsr) & Dr))
		delay(1);
	return csr8r(ctlr, Rbr);
}

static void
i8250drain(Ctlr *ctlr)
{
	int i;

	for(i = 1000; !(csr8r(ctlr, Lsr) & Thre) && i > 0; i--)
		delay(1);
}

static void
i8250putc(Uart* uart, int c)
{
	Ctlr *ctlr;

	ctlr = uart->regs;
	i8250drain(ctlr);
	csr8o(ctlr, Thr, c);
	coherence();
	i8250drain(ctlr);
}

void
_uartputs(char* s, int n)	/* debugging */
{
	char *e;
	int lastc;

	lastc = 0;
	for(e = s+n; s < e; s++){
		if(*s == '\n' && lastc != '\r')
			i8250putc(&i8250uart[0], '\r');
		i8250putc(&i8250uart[0], *s);
		lastc = *s;
	}
}

static void
i8250poll(Uart* uart)
{
	Ctlr *ctlr;

	/*
	 * If PhysUart has a non-nil .poll member, this
	 * routine will be called from the uartclock timer.
	 * If the Ctlr .poll member is non-zero, when the
	 * Uart is enabled interrupts will not be enabled
	 * and the result is polled input and output.
	 * Not very useful here, but ports to new hardware
	 * or simulators can use this to get serial I/O
	 * without setting up the interrupt mechanism.
	 */
	ctlr = uart->regs;
	if(!ctlr->iena && ctlr->poll)
		i8250interrupt(nil, uart);
}

PhysUart i8250physuart = {
	.name		= "i8250",
	.pnp		= i8250pnp,
	.enable		= i8250enable,
	.disable	= i8250disable,
	.kick		= i8250kick,
	.dobreak	= i8250break,
	.baud		= i8250baud,
	.bits		= i8250bits,
	.stop		= i8250stop,
	.parity		= i8250parity,
	.modemctl	= i8250modemctl,
	.rts		= i8250rts,
	.dtr		= i8250dtr,
	.status		= i8250status,
	.fifo		= i8250fifo,
	.getc		= i8250getc,
	.putc		= i8250putc,
	.poll		= i8250poll,
};

Uart*
i8250console(char* cfg)
{
	int i;
	Uart *uart;
	Ctlr *ctlr;
	char *cmd, *p;

	if (nuart == 0) {
		print("i8250console: no uarts\n");
		return nil;
	}

	/*
	 * Before i8250pnp() is run, we can only set the console
	 * to 0 or 1 because those are the only uart structs which
	 * will be the same before and after that.
	 */
	if((p = getconf("console")) == nil && (p = cfg) == nil)
		return nil;
	i = strtoul(p, &cmd, 0);
	if(p == cmd)
		return nil;
	if((uart = uartconsole(i, cmd)) != nil){	/* already enabled? */
		consuart = uart;
		return uart;
	}

	/* set it up */
	if (i >= nuart) {
		print("i8250console: uart %d out of range\n", i);
		return nil;
	}
	uart = &i8250uart[i];
	ctlr = uart->regs;
	if (ctlr->io == 0) {
		print("i8250console: ctlr->io nil\n");
		return nil;
	}

	/*
	 * Does it exist?
	 * Should be able to write/read the Scratch Pad (except COM1)
	 * but it seems to confuse the 8250.
	 */
	if (0 && i > 0) {
		csr8o(ctlr, Scr, 0x55);
		if(csr8r(ctlr, Scr) != 0x55)
			return nil;
	}

	(*uart->phys->enable)(uart, 0);
	if (Change_speed)
		uartctl(uart, "b115200"); /* non-standard but u-boot default */
	uartctl(uart, "l8 pn s1 i1");
	if(*cmd != '\0')
		uartctl(uart, cmd);
	consuart = uart;
	uart->console = 1;
	return uart;
}
