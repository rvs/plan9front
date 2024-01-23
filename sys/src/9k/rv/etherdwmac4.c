/*
 * Synopsys Gb Ethernet Mac (GMAC) v4.0 and later.
 * v4 introduced incompatible changes with v3 and earlier.
 * VisionFive 2's is v5.20 (dwmac-5.20) with bus width 8.
 *
 * Only able to use the bottom 4GB.
 */
#include "u.h"
#include "../port/lib.h"
#include "mem.h"
#include "dat.h"
#include "fns.h"
#include "io.h"
#include "../port/error.h"
#include "../port/netif.h"
#include "etherif.h"
#include "riscv.h"

typedef struct Ctlr Ctlr;
typedef struct Rd Rd;

enum {
	Crclen = 4,
	/*
	 * the STi7105 manual gives the maximum frame size as 1518 bytes
	 * for non-VLAN frames, and requires the descriptor's frame size
	 * to be a multiple of 4 or 8 or 16 to match the bus width.
	 */
	Rbsz	= ROUNDUP(ETHERMAXTU+Crclen, 16),
	Descalign= 32,		/* maybe should be CACHELINESZ */
	Desperate = 0,		/* flag: anything to make packets flow */

	/* tunable parameters */
	/*
	 * systems can need more receive buffers
	 * due to bursts of (non-9P) traffic.
	 */
	Ntd	= 64,
	Nrd	= 64,
	Nrb	= 2*Nrd,	/* private receive buffers, > Nrd */
};

enum {
	Aonsys = 0x17010000,	/* aon (always-on) syscon, 64KB */
};

enum {				/* interesting dev registers, by ulong* index */
	Cfg	= 0,
	Extconf	= 4/4,		/* extended config */
	Pktfilt	= 8/4,		/* rcv filters */
	Hashbot	= 0x10/4,
	Hashtop	= 0x14/4,	/* hash is upper 6 bits of crc register(?) */
	Version	= 0x20/4,
	Txq0flow= 0x70/4,
	Rxflow	= 0x90/4,
	Rxq0ctl0= 0xa0/4,
	Intsts	= 0xb0/4,
	Intenab	= 0xb4/4,	/* uninteresting enable bits */
	Hwfeat0	= 0x11c/4,
	Addrhi0	= 0x300/4,
	Addrlo0	= 0x304/4,

	/* mac mgmt counters */
	Mmcctl	= 0x0700/4,
	Mmcrxirq= 0x0704/4,
	Mmctxirq= 0x0708/4,
	Mmcrxirqmask = 0x070c/4,
	Mmctxirqmask = 0x0710/4,

	/* dma registers */
	Dmamode	= 0x1000/4,
	Busmode	= 0x1004/4,
	Dmasts	= 0x1008/4,	/* aka DMA_IS */
	Opmode	= 0x1018/4,

	Dmactl	= 0x1100/4,
	Dmatxctl= 0x1104/4,
	Dmarxctl= 0x1108/4,
	Xmitbase= 0x1114/4,
	Rcvbase	= 0x111c/4,
	Xmittail= 0x1120/4,
	Rcvtail	= 0x1128/4,
	Xmitlen = 0x112c/4,
	Rcvlen	= 0x1130/4,
	Dmainten= 0x1134/4,	/* aka DMA_CH0_IE */
	Curtd	= 0x1144/4,
	Currd	= 0x114c/4,
	Curtbuf	= 0x1154/4,
	Currbuf	= 0x115c/4,
	Dma0sts	= 0x1160/4,	/* aka DMA_CH0_STA */
};

enum {				/* Cfg bits */
	Acs	= 1<<20,	/* strip crc of incoming packets ≤ 1500 bytes */
	Jd	= 1<<17,	/* transmit " */
	Je	= 1<<16,	/* jumbo enable (up to 9018 bytes) */
	Dm	= 1<<13,	/* full duplex */
	Te	= 1<<1,		/* xmit on */
	Ipc	= 1<<27,	/* checksum offload */
	Re	= 1<<0,		/* rcvr on */
};

enum {				/* Pktfilt bits */
	Ra	= 1ul<<31,	/* disable address filters */
	Hpf	= 1<<10,	/* hash or perfect filter */
	Saf	= 1<<9,		/* src addr filter on */
	Saif	= 1<<8,		/* " " inverse filter on */
	Dbf	= 1<<5,		/* disable bcasts */
	Pm	= 1<<4,		/* accept all multicast */
	Daif	= 1<<3,		/* dest addr inverse filter on */
	Hmc	= 1<<2,		/* filter multicast by hash */
	Huc	= 1<<1,		/* " unicast " " */
	Pr	= 1<<0,		/* promiscuous */
};

enum {				/* Busmode bits */
	Dsl	= MASK(5)<<2,	/* desc gap in words; zero is contiguous */
	Dslshft	= 2,
	Swr	= 1<<0,		/* software reset */
};

enum {				/* Dma[rt]xctl bits */
	Ctlrstart= 1<<0,
};

enum {				/* Dmasts bits */
	Dmaisdma	= 1<<0,
	Dmaismtl	= 1<<16,
	Dmaismac	= 1<<17,
};

enum {				/* Dmainten & Dma0sts bits */
	Nie	= 1<<15,	/* normal intr summary enable */
	Aie	= 1<<14,	/* abnormal intr summary enable */
	Rie	= 1<<6,		/* rcv intr */
	Tie	= 1<<0,		/* xmit intr */
};

typedef struct {
	uint	reg;
	char	*name;
} Stat;

static Stat stattab[] = {
	0,	nil,
};

/* this is a register layout, which can't tolerate bogus padding */
#define DESCPAD	(Descalign/sizeof(long) - 4)	/* longs to next cacheline */
struct Rd {			/* Receive Descriptor */
	ulong	addr;
	ulong	zero;
	ulong	ctlcnts;	/* tx lengths */
	ulong	status;

	ulong	pad[DESCPAD];	/* avoid sharing cache lines with other descs */
};

enum {				/* Td-> and Rd->status bits */
	Own	= 1ul<<31,	/* owned by hw */

	Rxinte	= 1<<30,
	Txfs	= 1<<29,	/* first segment */
	Txls	= 1<<28,	/* last segment */
	Rxbuf2v	= 1<<25,
	Rxbuf1v	= 1<<24,

	Errsum	= 0,		/* could be something else */
};

enum {				/* Td->ctlcnts bits */
	Txioc	= 1ul<<31,	/* interrupt on tx completion */
};

#define Td Rd			/* Transmit Descriptor */

/* Td->addr is a byte address; there are no other bits in it */

struct Ctlr {
	Ethident;		/* see etherif.h */

	Lock	reglock;
	uint	im;		/* interrupt mask (enables) */
//	uint	lim;		/* ie as lproc wakeup */
	uint	rim;		/* ie as rproc wakeup */
	uint	tim;		/* ie as tproc wakeup */

	Rendez	rrendez;
	Rd*	rdba;		/* receive descriptor base address */
	uint	rdh;		/* " " head */
	uint	rdt;		/* " " tail */
	Block**	rb;		/* " buffers */
	uint	rintr;
	uint	rsleep;
	Watermark wmrd;
	Watermark wmrb;
	int	rdfree;		/* " descriptors awaiting packets */

	Rendez	trendez;
	Td*	tdba;		/* transmit descriptor base address */
	uint	tdh;		/* " " head */
	uint	tdt;		/* " " tail */
	Block**	tb;		/* " buffers */
	uint	tintr;
	uint	tsleep;
	Watermark wmtd;
	QLock	tlock;
	ulong	txtick;		/* tick at start of last tx start */

//	Rendez	lrendez;

	uchar	flag;		/* Factive or not */
	uchar	procsrunning;	/* flag: kprocs started for this Ctlr? */
	uchar	attached;
	QLock	alock;		/* attach lock */

	uchar	ra[Eaddrlen];	/* receive address */

	QLock	slock;
	ulong	stats[nelem(stattab)];

	uint	ixcs;		/* valid hw checksum count */
	uint	ipcs;		/* good hw ip checksums */
	uint	tcpcs;		/* good hw tcp/udp checksums */
};

enum {				/* flag bits */
	Factive	= 1<<0,
};

static	Ctlr	*ctlrtab[8];
static	int	nctlr;

/* these are shared by all the controllers of this type */
static	Lock	rblock;
static	Block	*rbpool;
/* # of rcv Blocks awaiting processing; can be briefly < 0 */
static	int	nrbfull;
static	int	nrbavail;

static void	replenish(Ctlr *ctlr);

/* wait up to a second for bit in ctlr->regs[reg] to become zero */
static int
awaitregbitzero(Ctlr *ctlr, int reg, ulong bit)
{
	return awaitbitpat(&ctlr->regs[reg], bit, 0);
}

static void
readstats(Ctlr *ctlr)
{
	uint i, reg;

	qlock(&ctlr->slock);
	for(i = 0; i < nelem(ctlr->stats); i++) {
		reg = stattab[i].reg;
		ctlr->stats[i] += ctlr->regs[reg / BY2WD];
	}
	qunlock(&ctlr->slock);
}

static long
ifstat(Ether *edev, void *a, long n, ulong offset)
{
	uint i;
	char *s, *p, *e;
	Ctlr *ctlr;

	ctlr = edev->ctlr;
	p = s = malloc(READSTR);
	if(p == nil)
		error(Enomem);
	e = p + READSTR;

	readstats(ctlr);
	for(i = 0; i < nelem(stattab); i++)
		if(stattab[i].name != nil && ctlr->stats[i] > 0)
			p = seprint(p, e, "%s\t%uld\n", stattab[i].name,
				ctlr->stats[i]);
	p = seprint(p, e, "%æ\n", ctlr);
	p = seprint(p, e, "cfg %#ux\n", ctlr->regs[Cfg]);
	p = seprint(p, e, "rintr %d rsleep %d\n", ctlr->rintr, ctlr->rsleep);
	p = seprint(p, e, "tintr %d tsleep %d\n", ctlr->tintr, ctlr->tsleep);
	p = seprintmark(p, e, &ctlr->wmrb);
	p = seprintmark(p, e, &ctlr->wmrd);
	seprintmark(p, e, &ctlr->wmtd);
	n = readstr(offset, a, n, s);
	free(s);

	return n;
}

static void
ienablelcked(Ctlr *ctlr, int ie)
{
	ctlr->im |= ie;
	ctlr->regs[Dmainten] = ctlr->im | Nie | Aie;
	// ctlr->regs[Intenab] |= ie;		/* paranoid */
	// ctlr->regs[Dmasts] = ~0;
	// ctlr->regs[Dma0sts] |= ctlr->im;
}

static void
ienable(Ctlr *ctlr, int ie)
{
	ilock(&ctlr->reglock);
	ienablelcked(ctlr, ie);
	iunlock(&ctlr->reglock);
}

/* return a Block from our pool */
static Block*
rballoc(void)
{
	Block *bp;

	ilock(&rblock);
	if((bp = rbpool) != nil){
		rbpool = bp->next;
		bp->next = nil;
		adec(&nrbavail);
		if (nrbavail < 0)
			print("rballoc: nrbavail negative\n");
	}
	iunlock(&rblock);
	return bp;
}

/* called from freeb for receive Blocks, returns them to our pool */
void
rbfree(Block *bp)
{
	bp->wp = bp->rp = (uchar *)ROUNDDN((uintptr)bp->lim - Rbsz, BLOCKALIGN);
	assert(bp->rp >= bp->base);
	assert(((uintptr)bp->rp & (BLOCKALIGN-1)) == 0);
	bp->flag &= ~(Bipck | Budpck | Btcpck | Bpktck);

	ilock(&rblock);
	bp->next = rbpool;
	rbpool = bp;
	adec(&nrbfull);
	ainc(&nrbavail);
	iunlock(&rblock);
}

/* reclaim sent buffers & descriptors */
static void
cleanup(Ctlr *ctlr)
{
	uint i, n, tdh;
	Block *bp;
	Td *td;

	tdh = ctlr->tdh;
	i = 0;
	n = NEXT(tdh, Ntd);
	while (td = &ctlr->tdba[n], n != ctlr->tdt && i++ < Ntd-2) {
		if (td->status & Own)
			break;
		tdh = n;
		bp = ctlr->tb[tdh];
		ctlr->tb[tdh] = nil;
		if (bp)
			freeb(bp);
		/*
		 * setting Own here would allow this (non-existent) buffer
		 * to be xmitted.
		 */
		n = NEXT(tdh, Ntd);
	}
	ctlr->tdh = tdh;
	notemark(&ctlr->wmrb, nrbfull);
}

void
transmit(Ether *edev)
{
	uint nqd, tdt, tdh, len;
	Block *bp;
	Ctlr *ctlr;
	Td *td, *tdtail;

	ctlr = edev->ctlr;
	if(!canqlock(&ctlr->tlock)){
		ienable(ctlr, Tie);
		return;
	}
	if (!ctlr->attached) {
		iprint("%æ: transmit called but ctlr not yet attached\n", ctlr);
		qunlock(&ctlr->tlock);
		return;
	}
	cleanup(ctlr);				/* free transmitted buffers */
	tdh = ctlr->tdh;
	tdt = ctlr->tdt;
	assert(ctlr->tdba != nil);
	tdtail = nil;
	for(nqd = 0; NEXT(tdt, Ntd) != tdh; nqd++){	/* ring not full? */
		if((bp = qget(edev->oq)) == nil)
			break;

		/* set up this xmit descriptor for this Block's buffer */
		assert((uintptr)PCIWADDR(bp->rp) < 4ull*GB);
		len = BLEN(bp);
		td = &ctlr->tdba[tdt];
		td->addr = PCIWADDR(bp->rp);
		td->zero = 0;
		td->ctlcnts = Txioc | len;
		coherence();
		td->status = Txls | Txfs | Own | len; /* Own allows xmit */

		if (ctlr->tb[tdt])
			panic("%æ: xmit q full", ctlr);
		ctlr->tb[tdt] = bp;
		tdtail = td + 1;
		if (tdtail >= ctlr->tdba + Ntd * sizeof *td)
			tdtail = ctlr->tdba;

		tdt = NEXT(tdt, Ntd);
	}
	/* note size of queue of tds awaiting transmission */
	notemark(&ctlr->wmtd, (uint)(tdt + Ntd - tdh) % Ntd);
	if (NEXT(tdt, Ntd) == tdh)
		iprint("%æ: out ring full\n", ctlr);
	USED(tdtail);
	ilock(&ctlr->reglock);
	if(nqd) {
		// ctlr->regs[Xmittail] = PADDR(tdtail);	/* old */
		ctlr->regs[Xmittail] = PADDR(&ctlr->tdba[tdt]);
		coherence();
		ctlr->tdt = tdt;
		ctlr->txtick = sys->ticks;
		ctlr->regs[Cfg] |= Te;		/* kick transmitter */
	}
	ienablelcked(ctlr, Tie);
	iunlock(&ctlr->reglock);
	qunlock(&ctlr->tlock);
}

static int
tim(void *vc)
{
	Ctlr *ctlr = (void *)vc;

	if(ctlr->tim & Tie)
		return 1;
	if(qlen(ctlr->edev->oq) == 0)
		return 0;
	return (ctlr->tdba[ctlr->tdt].status & Own) == 0;
}

static void
tproc(void *v)
{
	Ctlr *ctlr;
	Ether *edev;

	edev = v;
	ctlr = edev->ctlr;
	spllo();
	for (;;) {
		ctlr->tsleep++;
		sleep(&ctlr->trendez, tim, ctlr);
		ctlr->tim = 0;

		/*
		 * perhaps some buffers have been transmitted and their
		 * descriptors can be reused to copy Blocks out of edev->oq.
		 */
		transmit(edev);
	}
}

/* free any buffer Blocks in an array */
static void
freeblks(Block **blks, int count)
{
	Block *bp;

	if (blks == nil)
		return;
	while(count-- > 0){
		bp = blks[count];
		blks[count] = nil;
		if (bp) {
			bp->free = nil;
			freeb(bp);
		}
	}
}

static void
initrxring(Ctlr *ctlr, Rd *rdbase, int ndescs)
{
	ctlr->rdba = rdbase;
	/* rd->status = 0;	/* Own=0: prevent rcv filling by hw for now */
	memset(rdbase, 0, ndescs * sizeof(Rd));
	coherence();
}

static void
inittxring(Ctlr *ctlr, Td *tdbase, int ndescs)
{
	ctlr->tdba = tdbase;
	/* td->status = 0;	/* Own=0: available for us to fill to send */
	memset(ctlr->tdba, 0, ndescs * sizeof(Td));
	coherence();
}

static void
rxon(Ctlr *ctlr)
{
	ilock(&ctlr->reglock);
	ctlr->regs[Cfg] |= Re;
	ctlr->regs[Dmarxctl] = Ctlrstart | Rbsz<<1 | 2<<16;	/* pbs<<16 */
	ienablelcked(ctlr, Rie);
	iunlock(&ctlr->reglock);
}

static void
setdmaskip(Ctlr *ctlr)
{
	int buswidth, dmaskip;
	uint *regs;

	/* probe to find bus width */
	regs = ctlr->regs;
	regs[Xmittail] = 0xf;
	coherence();
	buswidth = (regs[Xmittail] ^ 0xf) + 1;

	/* configure dma skipping between descs */
	dmaskip = (sizeof(Rd) - 4*sizeof(long)) / buswidth;
	regs[Dmactl] = 1<<16 | dmaskip<<18;	/* pblx8 | dsl(dmaskip) */
}

/*
 * Get the receiver into a usable state.  Some of this is boilerplate
 * that could be (or is) done automatically as part of reset,
 * but we also disable unused or broken features (e.g., IP checksums).
 */
static void
rxinit(Ctlr *ctlr)
{
	uint *regs;
	ulong cfg;
	uvlong physbda;

	ctlr->edev->link = 1;
	regs = ctlr->regs;
	ilock(&ctlr->reglock);
	regs[Busmode] |= Swr;
	awaitbitpat(&regs[Busmode], Swr, 0);
	regs[Dmainten] = 0;
	coherence();
	delay(1);
	regs[Cfg] &= ~(Jd|Je|Ipc|Re);
	regs[Cfg] |= Acs;		/* exclude CRC from rcv buf & length */
	regs[Dmasts] = ~0;		/* clear all bits */

	/* see bits 18-20 of Aonsys+0xc for phy */
	regs[Dmarxctl] &= ~Ctlrstart;
	coherence();
	delay(1);

	setdmaskip(ctlr);
	regs[Cfg] |= Dm;
	coherence();
	delay(1);
	ctlr->rdfree = 0;
	cfg = regs[Pktfilt] & ~(Ra|Pm|Dbf|Huc|Hmc);
	cfg |= Hmc;
	if (Desperate) {	/* emergency use if mcasthash is wrong */
		cfg |= Pm;
		regs[Hashtop] = regs[Hashbot] = ~0;	/* accept all */
	} else
		regs[Hashtop] = regs[Hashbot] = 0;
	regs[Pktfilt] = cfg;
	iunlock(&ctlr->reglock);

	/* set up the rcv ring */
	initrxring(ctlr, ctlr->rdba, Nrd);
	ctlr->rdh = ctlr->rdt = 0;
	/* don't need to use uncached memory nor addresses on non-beagles */
	physbda = PADDR(ctlr->rdba);
	assert(physbda < 4ull*GB);
	ilock(&ctlr->reglock);
	regs[Rcvlen] = Nrd - 1;
	regs[Rcvbase] = physbda;
	regs[Rcvtail] = physbda;
	iunlock(&ctlr->reglock);
	replenish(ctlr);
	rxon(ctlr);
}

static void
replenish(Ctlr *ctlr)
{
	uint rdt, rdh, i;
	Block *bp;
	Block **rb;
	Rd *rd, *rdtail;

	i = 0;
	rdh = ctlr->rdh;
	rdtail = nil;
	for(rdt = ctlr->rdt; NEXT(rdt, Nrd) != rdh; rdt = NEXT(rdt, Nrd)){
		rd = &ctlr->rdba[rdt];
		rb = &ctlr->rb[rdt];
		if(*rb != nil){
			iprint("%æ: rx overrun\n", ctlr);
			break;
		}
		*rb = bp = rballoc();
		if(bp == nil)		/* don't have a buffer for this desc? */
			break;
		/* set up this rcv descriptor for new Block's buffer */
		assert((uintptr)PCIWADDR(bp->rp) < 4ull*GB);
		rd->addr = PCIWADDR(bp->rp);
		rd->zero = 0;
		rd->ctlcnts = 0;		// was Rbsz
		coherence();
		rd->status = Own | Rxinte | Rxbuf1v; /* hand off to hw to fill */
		rdtail = rd;
		ctlr->rdfree++;
		i++;
	}
	ctlr->rdt = rdt;
	USED(rdtail);
	if (i) {
		coherence();
		// ctlr->regs[Rcvtail] = PADDR(rdtail);	/* old */
		ctlr->regs[Rcvtail] = PADDR(&ctlr->rdba[rdt]);
		coherence();
		ienable(ctlr, Rie);
	}
}

static int
rim(void *vc)
{
	Ctlr *ctlr = (void *)vc;

	if(ctlr->rim & Rie)
		return 1;
	return (ctlr->rdba[ctlr->rdh].status & Own) == 0;
}

static void
ckcksum(Ctlr *ctlr, Rd *, Block *bp)
{
	ctlr->ixcs++;
	bp->flag |= Bpktck;
}

static int
qinpkt(Ctlr *ctlr)
{
	int passed, tosspkt;
	uint rdh, len;
	Block *bp;
	Rd *rd;

	ctlr->rim = 0;
	rdh = ctlr->rdh;
	rd = &ctlr->rdba[rdh];
	if (rd->status & Own)
		return -1;		/* wait for pkts to arrive */

	passed = tosspkt = 0;
	coherence();			/* ensure up-to-date rd->status */
	bp = ctlr->rb[rdh];
	if (bp == nil)
		panic("%æ: nil Block* from ctlr->rb", ctlr);
	/*
	 * Accept packets with no errors and buffer available.
	 */
	if (rd->status & (Own|Errsum))
		tosspkt++;
	else {
		len = rd->status & MASK(14);
		if ((int)len < 64)
			len = 64;		/* for 60-byte arps? */
		if (len > ETHERMAXTU+Crclen) {
			iprint("%æ: ignoring jumbo\n", ctlr);
			tosspkt++;
		} else {
			bp->wp += len;
			ckcksum(ctlr, rd, bp);
			notemark(&ctlr->wmrb, ainc(&nrbfull));
			/* pass pkt upstream, where it will be freed eventually */
			etheriq(ctlr->edev, bp, 1);
			passed++;
		}
	}
	if (tosspkt) {
		ainc(&nrbfull);
		freeb(bp);			/* toss bad pkt */
	}

	ctlr->rb[rdh] = nil;
	ctlr->rdh = NEXT(rdh, Nrd);
	ctlr->rdfree--;
	/*
	 * if number of rds ready for packets is too low,
	 * set up the unready ones.
	 */
	if (ctlr->rdfree <= Nrd*3/4)
		replenish(ctlr);
	return passed;
}

static void
rproc(void *v)
{
	int passed, npass;
	Ctlr *ctlr;
	Ether *edev;

	edev = v;
	ctlr = edev->ctlr;
	spllo();
	for (ctlr->rdh = 0; ; ) {
		replenish(ctlr);
		/*
		 * Prevent an idle or unplugged interface from interrupting.
		 * Allow receiver interrupts initially and again
		 * if the interface (and transmitter) see actual use.
		 */
		if (edev->outpackets > 10 || ctlr->rintr < 2*Nrd)
			ienable(ctlr, Rie);
		ctlr->rsleep++;
		sleep(&ctlr->rrendez, rim, ctlr);

		for(passed = 0; (npass = qinpkt(ctlr)) >= 0; passed += npass)
			;
		/* note how many rds had full buffers */
		notemark(&ctlr->wmrd, passed);
	}
}

static void
promiscuous(void *a, int on)
{
	Ctlr *ctlr;
	Ether *edev;

	edev = a;
	ctlr = edev->ctlr;
	ilock(&ctlr->reglock);
	if(on)
		ctlr->regs[Pktfilt] |= Pr;
	else
		ctlr->regs[Pktfilt] &= ~Pr;
	iunlock(&ctlr->reglock);
}

static ulong
mcasthash(uchar *mac)
{
	uint i, hash, rev;

	/* apparently have to complement the crc */
	hash = ~ethercrc(mac, Eaddrlen);
	/* reverse bits, take 6 top-most. equivalently, reverse bottom 6. */
	rev = 0;
	for (i = 0; i < 6; i++) {
		rev |= (hash & 1) << (5-i);
		hash >>= 1;
	}
	return rev;
}

static void
multicast(void *a, uchar *addr, int on)
{
	ulong hash, word, bit;
	Ctlr *ctlr;

	if (a == nil)
		panic("multicast: nil edev arg");
	ctlr = ((Ether *)a)->ctlr;
	if (ctlr == nil)
		panic("multicast: nil edev->ctlr");

	/*
	 * multiple ether addresses can hash to the same filter bit,
	 * so it's never safe to clear a filter bit.
	 * if we want to clear filter bits, we need to keep track of
	 * all the multicast addresses in use, clear all the filter bits,
	 * then set the ones corresponding to in-use addresses.
	 */
	hash = mcasthash(addr);
	word = Hashbot;
	if (BITMAPWD(hash) % 2 != 0)		/* mod 2 enforces bounds */
		word = Hashtop;
	bit = BITMAPBIT(hash);
	ilock(&ctlr->reglock);
	if(on)
		ctlr->regs[word] |= bit;
//	else
//		ctlr->regs[word] &= ~bit;
	iunlock(&ctlr->reglock);
}

static void
freez(void **pptr)
{
	free(*pptr);
	*pptr = nil;
}

static void
freemem(Ctlr *ctlr)
{
	int i;
	Block *bp;

	/* only free enough rcv bufs for one controller */
	for (i = Nrb; i > 0 && (bp = rballoc()) != nil; i--){
		bp->free = nil;
		freeb(bp);
	}
	freez(&ctlr->rdba);
	freez(&ctlr->tdba);
	freez(&ctlr->rb);
	freez(&ctlr->tb);
}

/* don't discard all state; we may be attached again later */
static int
detach(Ctlr *ctlr)
{
	uint *regs;

	regs = ctlr->regs;
	assert(regs);
	ilock(&ctlr->reglock);
	regs[Busmode] |= Swr;
	awaitbitpat(&regs[Busmode], Swr, 0);
	regs[Intenab] = regs[Dmainten] = 0;
	regs[Intsts] = regs[Dmasts] = regs[Dma0sts] = ~0;
	regs[Cfg] &= ~(Te|Re);
	regs[Dmarxctl] &= ~Ctlrstart;
	regs[Dmatxctl] &= ~Ctlrstart;
	iunlock(&ctlr->reglock);
	delay(10);			/* let dma stop */
	ctlr->attached = 0;
	return 0;
}

static void
shutdown(Ether *edev)
{
	detach(edev->ctlr);
	/* don't freemem; kprocs are using existing rings and we may reattach */
}

extern uchar ether0mac[];

#include <ip.h>

static void
setmacs(Ctlr *ctlr)
{
	int i;
	uint *regs;
	ulong top, bot;
	uchar *ra;
	static int ctlrno;

	regs = ctlr->regs;
	ra = ctlr->ra;
	ilock(&ctlr->reglock);
	bot = regs[Addrlo0];
	top = regs[Addrhi0];
	if ((bot || top & MASK(16)) &&
	    (bot != ~0u || (top & MASK(16)) != MASK(16))) {
		ra[0] = bot;
		ra[1] = bot>>8;
		ra[2] = bot>>16;
		ra[3] = bot>>24;
		ra[4] = top;
		ra[5] = top>>8;
		fmtinstall('E', eipfmt);
		iprint("%æ: mac address %E aleady set\n", ctlr, ra);
	} else {
		iprint("%æ: setting mac address to default\n", ctlr);
		memmove(ra, ether0mac, sizeof ra);
		ra[Eaddrlen-1] += ctlrno++;		/* big endian mac */
		regs[Addrhi0] = ra[5]<<8  | ra[4];
		regs[Addrlo0] = ra[3]<<24 | ra[2]<<16 | ra[1]<<8 | ra[0];
	}
	for (i = Addrlo0+1; i <= Addrlo0+2*15; i++)
		regs[i] = 0;
	iunlock(&ctlr->reglock);
	if (ctlr->edev)
		multicast(ctlr->edev, ra, 1);
}

/*
 * may be called from discover with ctlr only partially populated.
 */
static int
reset(Ctlr *ctlr)
{
	uint *regs;

	if(detach(ctlr)){
		print("%æ: reset timeout\n", ctlr);
		return -1;
	}

	regs = ctlr->regs;
	if (regs == nil) {
		print("%æ: nil regs\n", ctlr);
		return -1;
	}
	/* if unknown, load mac address from non-volatile memory, if present */
	setmacs(ctlr);
	readstats(ctlr);		/* zero stats by reading regs */
	memset(ctlr->stats, 0, sizeof ctlr->stats);
	return 0;
}

/*
 * Get the transmitter into a usable state.  Much of this is boilerplate
 * that could be (or is) done automatically as part of reset (hint, hint).
 */
static void
txinit(Ctlr *ctlr)
{
	uint *regs;
	uvlong physbda;

	regs = ctlr->regs;
	ilock(&ctlr->reglock);
	regs[Cfg] &= ~Te;
	regs[Dmatxctl] &= ~Ctlrstart;
	iunlock(&ctlr->reglock);
	delay(1);

	/* set up tx queue ring */
	assert(ctlr->tdba != nil);
	inittxring(ctlr, ctlr->tdba, Ntd);
	ctlr->tdt = 0;
	ctlr->tdh = Ntd - 1;
	/* don't need to use uncached memory nor addresses on non-beagle */
	physbda = PADDR(ctlr->tdba);
	assert(physbda < 4ull*GB);
	ilock(&ctlr->reglock);
	regs[Xmitlen] = Ntd - 1;
	regs[Xmitbase] = physbda;
	regs[Xmittail] = physbda;
	coherence();

	regs[Dmatxctl] = Ctlrstart | 2<<16;		/* pbl<<16 */
	coherence();
	regs[Cfg] |= Te;
	iunlock(&ctlr->reglock);
}

static void
allocall(Ctlr *ctlr)
{
	int i;
	Block *bp;

	/* discard any buffer Blocks left over from before detach */
	freeblks(ctlr->tb, Ntd);
	freeblks(ctlr->rb, Nrd);

	if (ctlr->rdba == nil)
		ctlr->rdba = mallocalign(Nrd * sizeof(Rd), Descalign, 0, 0);
	if (ctlr->tdba == nil)
		ctlr->tdba = mallocalign(Ntd * sizeof(Td), Descalign, 0, 0);
	if (ctlr->rdba == nil || ctlr->tdba == nil)
		error(Enomem);
	assert(PADDR(ctlr->rdba) < 4ull*GB);
	assert(PADDR(ctlr->tdba) < 4ull*GB);

	ctlr->rb = malloc(Nrd * sizeof(Block *));
	ctlr->tb = malloc(Ntd * sizeof(Block *));
	if (ctlr->rb == nil || ctlr->tb == nil)
		error(Enomem);
	assert(PADDR(ctlr->rb) < 4ull*GB);
	assert(PADDR(ctlr->tb) < 4ull*GB);

	/* add enough rcv bufs for one controller to the pool */
	for(i = 0; i < Nrb; i++){
		bp = allocb(Rbsz);
		if(bp == nil)
			error(Enomem);
		assert(PADDR(bp) < 4ull*GB);
		bp->free = rbfree;
		freeb(bp);
	}
	aadd(&nrbfull, Nrb);		/* compensate for adecs in rbfree */
}

static void
etherkproc(Ether *edev, void (*kp)(void *), char *sfx)
{
	char buf[KNAMELEN];

	snprint(buf, sizeof buf, "#l%d%s", edev->ctlrno, sfx);
	kproc(buf, kp, edev);
}

static void
startkprocs(Ctlr *ctlr)
{
	char procname[16];
	Ether *edev;

	if (ctlr->procsrunning)
		return;
	edev = ctlr->edev;
	snprint(procname, sizeof procname, "xmit");
	etherkproc(edev, tproc, procname);
	snprint(procname, sizeof procname, "recv");
	etherkproc(edev, rproc, procname);
	ctlr->procsrunning = 1;
}

/*
 * could be attaching again after a detach.
 * ether interrupt will have been enabled by etherprobe.
 */
static void
attach(Ether *edev)
{
	Ctlr *ctlr;
	static Lock attlock;

	ctlr = edev->ctlr;
	qlock(&ctlr->alock);
	if (ctlr->attached) {
		qunlock(&ctlr->alock);
		return;
	}
	if(waserror()){
		reset(ctlr);
		freemem(ctlr);
		qunlock(&ctlr->alock);
		nexterror();
	}
	if(ctlr->rdba == nil)
		allocall(ctlr);
	/* don't change nrbfull; it's shared by all controllers */
	initmark(&ctlr->wmrb, Nrb, "rcv Blocks not yet processed");
	initmark(&ctlr->wmrd, Nrd-1, "rcv descrs processed at once");
	initmark(&ctlr->wmtd, Ntd-1, "xmit descr queue len");

	ilock(&attlock);
	rxinit(ctlr);
	txinit(ctlr);
	startkprocs(ctlr);
	ctlr->attached = 1;
	iunlock(&attlock);
	qunlock(&ctlr->alock);
	poperror();
}

static Intrsvcret
interrupt(Ureg*, void *ve)
{
	int icr, icrreg, im, wake;
	int ointen;
	uint *regs;
	Ctlr *ctlr;

	coherence();
	ctlr = ((Ether *)ve)->ctlr;
	regs = ctlr->regs;
	assert(regs);
	if (!ctlr->attached)
		return Intrnotforme;

	if ((regs[Dma0sts] & (Tie|Rie)) == 0)	/* polling optimisation */
		return Intrnotforme;

	ilock(&ctlr->reglock);
	icrreg = regs[Dma0sts];
	ointen = regs[Dmainten];
	USED(ointen);
	regs[Dmainten] = 0;		/* disable all my intrs */
	coherence();
	im = ctlr->im;
	icr = icrreg & im & (Tie|Rie);
	wake = icr;

	/* alternatively, have Curtd or Currd changed? */
	if(icr & Rie)
		ctlr->rintr++;
	if(icr & Tie){
		ctlr->txtick = 0;
		ctlr->tintr++;
	}
	ctlr->rim |= icr & Rie;
	ctlr->tim |= icr & Tie;
	im &= ~icr;

	/* enable only intr sources we didn't service and are interested in */
	ctlr->im = im;
	if (im & (Tie|Rie))
		regs[Dmainten] = im | Aie | Nie;
	regs[Dmasts] = regs[Dmasts];	/* clear Dmasts */
	regs[Dma0sts] = regs[Dma0sts];	/* ack the interrupts */
	iunlock(&ctlr->reglock);

	/* now that registers have been updated, wake sleepers */
	if(wake & Rie)
		wakeup(&ctlr->rrendez);
	if(wake & Tie)
		wakeup(&ctlr->trendez);
	return Intrforme;
}

/*
 * map device p and populate a new Ctlr for it.
 * add the new Ctlr to our list.
 */
static Ctlr *
newctlr(void *regs)
{
	uintptr io;
	void *mem;
	Ctlr *ctlr;

	if (regs == nil)
		return nil;
	io = (uintptr)regs;
	mem = vmap(io, 64*KB);
	if(mem == nil){
		print("#l%d: etherdwmac4: can't map regs %#p\n", nctlr, regs);
		return nil;
	}

	ctlr = malloc(sizeof *ctlr);
	if(ctlr == nil) {
		vunmap(mem, 64*KB);
		error(Enomem);
	}
	ctlr->regs = (uint*)mem;
	ctlr->physreg = (uint*)io;
	ctlr->prtype = "dwmac4";
	if(reset(ctlr)){
		print("%æ: can't reset\n", ctlr);
		free(ctlr);
		vunmap(mem, 64*KB);
		return nil;
	}
	ctlrtab[nctlr++] = ctlr;
	return ctlr;
}

static void
discover(void)
{
	int i;

	fmtinstall(L'æ', etherfmt);
	for (i = 0; i < nelem(soc.ether); i++)
		if (soc.ether[i])
			newctlr(soc.ether[i]);
}

/*
 * called from etherprobe.  upon return, etherprobe will enable interrupts
 * for edev->irq iff non-negative.
 */
static int
adddwmac4(Ether *edev)
{
	int i;
	Ctlr *ctlr;
	static int ctlrno;

	if (edev == nil)
		panic("etherdwmac4 pnp: nil edev arg");
	if(nctlr == 0)
		discover();
	ctlr = nil;
	for(i = 0; i < nctlr; i++){
		ctlr = ctlrtab[i];
		if(ctlr == nil || ctlr->flag & Factive)
			continue;
		if(edev->port == 0 || edev->port == (uintptr)ctlr->physreg)
			break;
	}
	if (i >= nctlr || ctlr == nil)
		return -1;
	ctlr->flag |= Factive;

	ctlr->edev = edev;		/* point back to Ether */
	edev->ctlr = ctlr;
	edev->port = (uintptr)ctlr->physreg;	/* for printing */
	/* a negative irq would delay interrupt enable until attach */
	edev->irq = ioconf("ether", ctlrno++)->irq;
	edev->pcidev = nil;
	edev->tbdf = BUSUNKNOWN;
	edev->mbps = 1000;
	edev->maxmtu = ETHERMAXTU;
	memmove(edev->ea, ctlr->ra, Eaddrlen);

	edev->arg = edev;
	edev->attach = attach;
	edev->detach = shutdown;
	edev->transmit = transmit;
	edev->interrupt = interrupt;
	edev->ifstat = ifstat;
	edev->shutdown = shutdown;
	edev->ctl = nil;
	edev->multicast = multicast;
	edev->promiscuous = promiscuous;
	return 0;
}

void
etherdwmac4link(void)
{
	addethercard("dwmac4", adddwmac4);
}
