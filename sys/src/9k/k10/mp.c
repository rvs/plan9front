#include "u.h"
#include "../port/lib.h"
#include "mem.h"
#include "dat.h"
#include "fns.h"

#include "apic.h"
#include "mp.h"

#undef	DBGFLG
#define	DBGFLG 0
#undef	DBG
#define DBG(...) do { if(!DBGFLG) {} else { dbgprint(__VA_ARGS__); \
	delay(1000); } } while (0)

/*
 * MultiProcessor Specification Version 1.[14].
 */
typedef struct {				/* MP Floating Pointer */
	uchar	signature[4];			/* "_MP_" */
	uchar	addr[4];			/* PCMP */
	uchar	length;				/* 1 */
	uchar	revision;			/* [14] */
	uchar	checksum;
	uchar	feature[5];
} _MP_;

typedef struct {				/* MP Configuration Table */
	uchar	signature[4];			/* "PCMP" */
	uchar	length[2];
	uchar	revision;			/* [14] */
	uchar	checksum;
	uchar	string[20];			/* OEM + Product ID */
	uchar	oaddr[4];			/* OEM table pointer */
	uchar	olength[2];			/* OEM table length */
	uchar	entry[2];			/* entry count */
	uchar	apicpa[4];			/* local APIC address */
	uchar	xlength[2];			/* extended table length */
	uchar	xchecksum;			/* extended table checksum */
	uchar	reserved;

	uchar	entries[];
} PCMP;

typedef struct {
	char	type[6];
	int	polarity;			/* default for this bus */
	int	trigger;			/* default for this bus */
} Mpbus;

static Mpbus mpbusdef[] = {
	{ "PCI   ", IPlow, TMlevel, },
	{ "ISA   ", IPhigh, TMedge, },
};
static Mpbus* mpbus[Nbus];
int mpisabusno = -1;

static void
mpintrprint(char* s, uchar* p)
{
	char buf[128], *b, *e;
	static char format[] =
		" type %d flags %#ux bus %d IRQ %d APIC %d INTIN %d\n";

	b = buf;
	e = b + sizeof(buf);
	b = seprint(b, e, "mpparse: intr:");
	if(s != nil)
		b = seprint(b, e, " %s:", s);
	seprint(b, e, format, p[1], L16GET(p+2), p[4], p[5], p[6], p[7]);
	print(buf);
}

static u32int
mpmkintr(uchar* p)
{
	u32int v;
	Apic *apic;
	int n, polarity, trigger;

	/*
	 * Check valid bus, interrupt input pin polarity
	 * and trigger mode. If the APIC ID is 0xff it means
	 * all APICs of this type so those checks for useable
	 * APIC and valid INTIN must also be done later in
	 * the appropriate init routine in that case. It's hard
	 * to imagine routing a signal to all IOAPICs, the
	 * usual case is routing NMI and ExtINT to all LAPICs.
	 */
	if(mpbus[p[4]] == nil){
		mpintrprint("no source bus", p);
		return 0;
	}
	if(p[6] != 0xff){
		if(Napic < 256 && p[6] >= Napic){
			mpintrprint("APIC ID out of range", p);
			return 0;
		}
		switch(p[0]){
		default:
			mpintrprint("INTIN botch", p);
			return 0;
		case PcmpIOINTR:
			apic = &ioapic[p[6]];
			if(!apic->useable){
				mpintrprint("unuseable APIC", p);
				return 0;
			}
			if(p[7] >= apic->nrdt){
				if (DBGFLG)
					mpintrprint("IO INTIN out of range", p);
				return 0;
			}
			break;
		case PcmpLINTR:
			apic = &xapic[p[6]];
			if(!apic->useable){
				mpintrprint("unuseable APIC", p);
				return 0;
			}
			if(p[7] >= nelem(apic->lvt)){
				mpintrprint("LOCAL INTIN out of range", p);
				return 0;
			}
			break;
		}
	}
	n = L16GET(p+2);
	if((polarity = (n & PcmpPOMASK)) == 2 ||
	    (trigger = ((n>>2) & PcmpPOMASK)) == 2){
		mpintrprint("invalid polarity/trigger", p);
		return 0;
	}

	/*
	 * Create the low half of the vector table entry (LVT or RDT).
	 * For the NMI, SMI and ExtINT cases, the polarity and trigger
	 * are fixed (but are not always consistent over IA-32 generations).
	 * For the INT case, either the polarity/trigger are given or
	 * it defaults to that of the source bus;
	 * whether INT is Fixed or Lowest Priority is left until later.
	 */
	v = Im;
	switch(p[1]){
	default:
		mpintrprint("invalid type", p);
		return 0;
	case PcmpINT:
		switch(polarity){
		case 0:
			v |= mpbus[p[4]]->polarity;
			break;
		case PcmpHIGH:
			v |= IPhigh;
			break;
		case PcmpLOW:
			v |= IPlow;
			break;
		}
		switch(trigger){
		case 0:
			v |= mpbus[p[4]]->trigger;
			break;
		case PcmpHIGH:
			v |= TMedge;
			break;
		case PcmpLOW:
			v |= TMlevel;
			break;
		}
		break;
	case PcmpNMI:
		v |= TMedge|IPhigh|MTnmi;
		break;
	case PcmpSMI:
		v |= TMedge|IPhigh|MTsmi;
		break;
	case PcmpExtINT:
		v |= TMedge|IPhigh|MTei;
		break;
	}

	return v;
}

static void
mpparse(PCMP *pcmp)
{
	u32int lo;
	uchar *e, *p;
	int devno, i, n;

	p = pcmp->entries;
	e = (uchar *)pcmp + L16GET(pcmp->length);
	while (p < e)
		switch (*p) {
		default:
			print("mpparse: unknown PCMP type %d (e-p %#lld)\n",
				*p, (vlong)(e - p));
			for (i = 0; p < e; i++) {
				if (i && ((i & 0x0f) == 0))
					print("\n");
				print(" %#2.2ux", *p++);
			}
			print("\n");
			break;
		case PcmpPROCESSOR:
			/*
			 * Initialise the APIC if it is enabled.
			 * p[1] is the APIC ID, the memory mapped address comes
			 * from the PCMP structure as the addess is local to the
			 * CPU and identical for all. Indicate whether this is
			 * the bootstrap processor.
			 */
			DBG("mpparse: APIC %d pa %#ux useable %d\n",
				p[1], L32GET(pcmp->apicpa), p[3] & PcmpEN);
			if (p[3] & PcmpEN)
				apicinit(p[1], L32GET(pcmp->apicpa),
					p[3] & PcmpBP);
			p += 20;
			break;
		case PcmpBUS:
			DBG("mpparse: bus: %d type %6.6s\n",
				p[1], (char *)p + 2);
			if (mpbus[p[1]] != nil) {
				print("mpparse: bus %d already allocated\n", p[1]);
				p += 8;
				break;
			}
			for (i = 0; i < nelem(mpbusdef); i++) {
				if (memcmp(p + 2, mpbusdef[i].type, 6) != 0)
					continue;
				if (memcmp(p + 2, "ISA   ", 6) == 0) {
					if (mpisabusno != -1) {
						print("mpparse: bus %d already have ISA bus %d\n",
							p[1], mpisabusno);
						continue;
					}
					mpisabusno = p[1];
				}
				mpbus[p[1]] = &mpbusdef[i];
				break;
			}
			if (mpbus[p[1]] == nil)
				print("mpparse: bus %d type %6.6s unknown\n",
					p[1], (char * )p + 2);

			p += 8;
			break;
		case PcmpIOAPIC:
			/*
			 * Initialise the IOAPIC if it is enabled.
			 * p[1] is the APIC ID, p[4-7] is the memory-mapped
			 * address.
			 */
			DBG("mpparse: IOAPIC %d pa %#ux useable %d\n",
				p[1], L32GET(p + 4), p[3] & PcmpEN);
			if (p[3] & PcmpEN)
				ioapicinit(p[1], L32GET(p + 4));

			p += 8;
			break;
		case PcmpIOINTR:
			/*
			 * p[1] is the interrupt type;
			 * p[2-3] contains the polarity and trigger mode;
			 * p[4] is the source bus;
			 * p[5] is the IRQ on the source bus;
			 * p[6] is the destination APIC;
			 * p[7] is the INITIN pin on the destination APIC.
			 */
			if (p[6] == 0xff) {
				mpintrprint("routed to all IOAPICs", p);
				p += 8;
				break;
			}
			if ((lo = mpmkintr(p)) == 0) {
				p += 8;
				break;
			}
			if (DBGFLG)
				mpintrprint(nil, p);

			/*
			 * Always present the device number in the style
			 * of a PCI Interrupt Assignment Entry. For the ISA
			 * bus the IRQ is the device number but unencoded.
			 * May need to handle other buses here in the future
			 * (but unlikely).
			 */
			devno = p[5];
			if (memcmp(mpbus[p[4]]->type, "PCI   ", 6) != 0)
				devno <<= 2;
			ioapicintrinit(p[4], p[6], p[7], devno, lo);

			p += 8;
			break;
		case PcmpLINTR:
			/*
			 * Format is the same as IOINTR above.
			 */
			if ((lo = mpmkintr(p)) == 0) {
				p += 8;
				break;
			}
			if (DBGFLG)
				mpintrprint(nil, p);

			/*
			 * Everything was checked in mpmkintr above.
			 */
			if (p[6] == 0xff) {
				for (i = 0; i < Napic; i++) {
					if (!ioapic[i].useable ||
					    ioapic[i].addr != nil)
						continue;
					ioapic[i].lvt[p[7]] = lo;
				}
			} else
				ioapic[p[6]].lvt[p[7]] = lo;
			p += 8;
			break;
		}

	/*
	 * There's nothing of real interest in the extended table,
	 * should just move along, but check it for consistency.
	 */
	p = e;
	e = p + L16GET(pcmp->xlength);
	while (p < e)
		switch (*p) {
		default:
			n = p[1];
			print("mpparse: unknown extended entry %d length %d\n",
				*p, n);
			for (i = 0; i < n; i++) {
				if (i && ((i & 0x0f) == 0))
					print("\n");
				print(" %#2.2ux", *p++);
			}
			print("\n");
			break;
		case PcmpSASM:
			DBG("address space mapping\n");
			DBG(" bus %d type %d base %#llux length %#llux\n",
				p[2], p[3], l64get(p + 4), l64get(p + 12));
			p += p[1];
			break;
		case PcmpHIERARCHY:
			DBG("bus hierarchy descriptor\n");
			DBG(" bus %d sd %d parent bus %d\n", p[2], p[3], p[4]);
			p += p[1];
			break;
		case PcmpCBASM:
			DBG("compatibility bus address space modifier\n");
			DBG(" bus %d pr %d range list %d\n",
				p[2], p[3], L32GET(p + 4));
			p += p[1];
			break;
		}
}

static int
sigchecksum(void* address, int length)
{
	uchar *p, sum;

	sum = 0;
	for(p = address; length-- > 0; p++)
		sum += *p;
	return sum;
}

static void*
sigscan(uchar* address, int length, char* signature)
{
	uchar *e, *p;
	int siglength;

	DBG("check for %s in system base memory @ %#p\n", signature, address);

	e = address+length;
	siglength = strlen(signature);
	for(p = address; p+siglength < e; p += 16)
		if(memcmp(p, signature, siglength) == 0)
			return p;
	return nil;
}

/* returns number of bytes of free base (conventional) memory */
uintptr
freebasemem(void)
{
	return L16GET((uchar *)(KZERO+0x413)) * KB;
}

void*
sigsearch(char* signature)
{
	uintptr p;
	void *r;

	/*
	 * Search for the data structure:
	 * 1) within the first KiB of the Extended BIOS Data Area (EBDA), or
	 * 2) within the last KiB of system base memory if the EBDA segment
	 *    is undefined, or
	 * 3) within the BIOS ROM address space between 0xf0000 and 0xfffff
	 *    (but will actually check 0xe0000 to 0xfffff).
	 */
	if((p = (uintptr)BIOSSEG(L16GET((uchar *)EBDAADDR))) != 0)
		if((r = sigscan((void *)p, LOWMEMEND - p, signature)) != nil)
			return r;

	p = freebasemem();
	if (0)
		iprint("free base mem %#p = %,lld\n", p, p);
	if(p == 0)
		/* hack for virtualbox: look in last KiB below 0xa0000 */
		p = LOWMEMEND;
	else
		p += KZERO;
	if((r = sigscan((uchar *)p - KB, KB, signature)) != nil)
		return r;

	r = sigscan(BIOSSEG(0xe000), 0x20000, signature);
	if (r)
		return r;

	/* desperate last attempt for vbox; found at 0x9fc00 = 654336 */
	r = sigscan(KADDR(0), MB, signature);
	if (r) {
		iprint("sigsearch's last resort match at %#p\n", r);
		vmbotch(Virtualbox, "sigsearch's last resort match");
	}
	return r;
}

void	(*mpacpifunc)(void);		/* set in mpacpi.c */

static void
trympacpi(void)
{
	if (mpacpifunc != nil)
		(*mpacpifunc)();
}

void
mpsinit(void)
{
	uchar *p;
	int i, n;
	_MP_ *mp;
	PCMP *pcmp;

	if((mp = sigsearch("_MP_")) == nil) {
		vmbotch(Virtualbox, "there no MPS table!  WTF?");
		return;
	}
	if(DBGFLG){
		DBG("_MP_ @ %#p, addr %#ux length %ud rev %d",
			mp, L32GET(mp->addr), mp->length, mp->revision);
		for(i = 0; i < sizeof(mp->feature); i++)
			DBG(" %2.2#ux", mp->feature[i]);
		DBG("\n");
	}
	if(mp->revision != 1 && mp->revision != 4)
		return;
	if(sigchecksum(mp, mp->length*16) != 0)
		return;

	if((pcmp = vmap(L32GET(mp->addr), sizeof(PCMP))) == nil)
		return;
	if(pcmp->revision != 1 && pcmp->revision != 4){
		vunmap(pcmp, sizeof(PCMP));
		return;
	}
	n = L16GET(pcmp->length) + L16GET(pcmp->xlength);
	vunmap(pcmp, sizeof(PCMP));

	if((pcmp = vmap(L32GET(mp->addr), n)) == nil)
		return;
	if(sigchecksum(pcmp, L16GET(pcmp->length)) != 0){
		vunmap(pcmp, n);
		return;
	}
	if(DBGFLG){
		DBG("PCMP @ %#p length %#ux revision %d\n",
			pcmp, L16GET(pcmp->length), pcmp->revision);
		DBG(" %20.20s oaddr %#ux olength %#ux\n",
			(char*)pcmp->string, L32GET(pcmp->oaddr),
			L16GET(pcmp->olength));
		DBG(" entry %d apicpa %#ux\n",
			L16GET(pcmp->entry), L32GET(pcmp->apicpa));

		DBG(" xlength %#ux xchecksum %#ux\n",
			L16GET(pcmp->xlength), pcmp->xchecksum);
	}
	if(pcmp->xchecksum != 0){
		p = ((uchar*)pcmp) + L16GET(pcmp->length);
		i = sigchecksum(p, L16GET(pcmp->xlength));
		if(((i+pcmp->xchecksum) & 0xff) != 0){
			print("extended table checksums to %#ux\n", i);
			vunmap(pcmp, n);
			return;
		}
	}

	/*
	 * Parse the PCMP table and set up the datastructures
	 * for later interrupt enabling and application processor
	 * startup.
	 *
	 * if hyperthreaded virtual cpus are only listed in acpi, not PCMP,
	 * we won't start them.  A small loss, since they are mostly imaginary.
	 */
	mpparse(pcmp);

	/* scan ACPI's MADT for any CPUs that PCMP missed. */
//	if (CRASH_OK)
//		trympacpi();			/* not ready for prime time */

	apicdump();
	ioapicdump();
}
