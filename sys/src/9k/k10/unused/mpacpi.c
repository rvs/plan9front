/*
 * minimal acpi support for multiprocessors, from 9/pc.
 *
 * avoids AML but that's only enough to discover
 * the processors, not the interrupt routing details.
 */
#include "u.h"
#include "../port/lib.h"
#include "mem.h"
#include "dat.h"
#include "fns.h"
#include "io.h"
#include "mp.h"
#include "mpacpi.h"
#include "apic.h"

/* if we use this outside l64get, 8c says: out of fixed registers */
#define L64GET(p)	((uvlong)L32GET((uchar *)(p)+4) << 32 | L32GET(p))

enum {
	/* apic types */
	Apiclproc,
	Apicio,
	Apicintrsrcoverride,
	Apicnmisrc,
	Apiclnmi,
	Apicladdroverride,
	Apicios,
	Apicls,
	Apicintrsrc,
	Apiclx2,
	Apiclx2nmi,
};

#define dprint(...)	if(mpdebug) print(__VA_ARGS__); else USED(mpdebug)

/* from mp.c */
int	mpdebug = 1;		// TODO
int	mpnewproc(Apic *apic, int apicno, int flags);

Apic	*bootapic;

static int nprocid;

uvlong
l64get(uchar *p)
{
	return L64GET(p);
}

typedef struct Aclprocmadt Aclprocmadt;
struct Aclprocmadt {
	uchar	type;
	uchar	len;
	uchar	cpuid;
	uchar	lapicid;
	uchar	flags[4];
};

static int
mpacpiproc(Aclprocmadt *procmadt, ulong laddr, ulong **vladdrp)
{
	int f;
	unsigned id;
	char *already;
	Apic *apic;

	id = procmadt->lapicid;
	/* cpu not enablable or lapic id out of range? */
	if((L32GET(procmadt->flags) & Alprocen) == 0 || id > MaxAPICNO)
		return -1;

	already = "";
	f = 0;
	apic = &xapic[id];
	dprint("\tmpacpiproc: apic %#p\n", apic);
	apic->paddr = (void *)laddr;
	if (nprocid++ == 0) {
		f = PcmpBP;
		*vladdrp = vmap((uintptr)apic->paddr, 1024);
		if(*vladdrp == nil)
			print("proc apic %d: failed to map %#p\n",
				id, apic->paddr);
		bootapic = apic;
	}
	apic->addr = (uint *)*vladdrp;
	if(apic->addr == nil)
		already = "(fail)";

	if(apic->flags & PcmpUsed)
		already = "(on)";
	else if (mpnewproc(apic, id, f) < 0) {
		iprint("mpacpiproc: mpnewproc failed for apic %d\n", id);
		return -1;
	}

	dprint("\tapic proc %d/%d apicid %d flags%s%s %s\n", nprocid-1,
		apic->machno, id, f & PcmpBP? " boot": "",
		f & PcmpEN? " enabled": "", already);
	USED(already);

	/* this spu's lapic will be initialised in apiconline, on that cpu. */
	return 0;
}

static void
mpacpicpus(Madt *madt)
{
	int i, n, len;
	ulong laddr;
	ulong *vladdr;
	uchar *p;
	Aclprocmadt *procmadt;

	laddr = L32GET(madt->addr);
	dprint("APIC mpacpicpus(%#p) lapic addr %#lux, flags %#ux\n",
		madt, laddr, L32GET(madt->flags));

	n = L32GET(&madt->sdthdr[4]);
	p = madt->structures;
	vladdr = nil;
	dprint("\t%d structures at %#p\n", n, p);
	/* byte 0 is assumed to be type, 1 is assumed to be length */
	for(i = offsetof(Madt, structures[0]); i < n; i += len, p += len) {
		procmadt = (Aclprocmadt *)p;
		len = procmadt->len;
		switch(procmadt->type){
		case Apiclproc:
			if (mpacpiproc(procmadt, laddr, &vladdr) < 0)
				return;
			break;
		}
	}
}
//	case PcmpPROCESSOR:
//		Apic *apic;
//
//		if(!(p->flags & PcmpEN) || p->apicno > MaxAPICNO)
//			return nil;
//		apic = &mpapic[p->apicno];
//		apic = mpnewproc0(apic, p->apicno, p->flags) < 0? nil: apic;
//		if(apic){
//			/*
//			 * Must take a note of bootstrap processor APIC
//			 * now as it will be needed in order to start
//			 * the application processors later and there's
//			 * no guarantee that the bootstrap processor
//			 * appears first in the table before the others.
//			 */
//			apic->addr = va;
//			apic->paddr = pcmp->lapicbase;
//			if(apic->flags & PcmpBP)
//				bpapic = apic;
//			++*cpusonp;
//		}

/* returns nil iff checksum is bad */
static void *
mpacpirsdchecksum(void* addr, int length)
{
	uchar sum;

	sum = checksum(addr, length);
	return sum == 0? addr: nil;
}

/* call func for each acpi table found */
/* will these tables even exist under uefi? */
static void
mpacpiscan(void (*func)(uchar *))
{
	int asize, i, tbllen, sdtlen;
	uintptr dhpa, sdtpa;
	uchar *tbl, *sdt;
	Rsd *rsd;

	dprint("ACPI...");
	if((rsd = sigsearch("RSD PTR ")) == nil) {
		dprint("none\n");
		return;
	}

	dprint("rsd %#p physaddr %#ux length %ud %#llux rev %d oem %.6s\n",
		rsd, L32GET(rsd->raddr), L32GET(rsd->length),
		l64get(rsd->xaddr), rsd->revision, (char*)rsd->oemid);

	if(rsd->revision >= 2){			/* 2000 */
		if(mpacpirsdchecksum(rsd, 36) == nil)
			return;
		asize = 8;
		sdtpa = l64get(rsd->xaddr);
	} else {				/* pre-2000 antique: 32-bit */
		print("acpi version is < 2; retire this antique.\n");
		if(mpacpirsdchecksum(rsd, 20) == nil)
			return;
		asize = 4;
		sdtpa = L32GET(rsd->raddr);
	}

	if((sdt = vmap(sdtpa, 8)) == nil)
		return;
	if((sdt[0] != 'R' && sdt[0] != 'X') || memcmp(sdt+1, "SDT", 3) != 0){
		vunmap(sdt, 8);
		return;
	}
	sdtlen = L32GET(sdt + 4);
	vunmap(sdt, 8);

	if((sdt = vmap(sdtpa, sdtlen)) == nil)
		return;
	if(mpacpirsdchecksum(sdt, sdtlen) != nil)
		for(i = 36; i < sdtlen; i += asize){
			if(asize == 8)
				dhpa = l64get(sdt+i);
			else
				dhpa = L32GET(sdt+i);
	
			if((tbl = vmap(dhpa, 8)) == nil)
				continue;
			tbllen = L32GET(tbl + 4);
			vunmap(tbl, 8);
	
			if((tbl = vmap(dhpa, tbllen)) == nil)
				continue;
			if(mpacpirsdchecksum(tbl, tbllen) != nil)
				(*func)(tbl);
			vunmap(tbl, tbllen);
		}
	vunmap(sdt, sdtlen);
}

static int
apicset(Apic *apic, int type, uint apicno, int flags)
{
iprint("apicset: apic %d type %d flags %#o\n", apicno, type, flags); // TODO
	if(apicno > MaxAPICNO)
		return -1;
	if (flags & PcmpEN)		// TODO
		/* sets apic->machno, apic->useable */
		apicinit(apicno, (uintptr)apic->paddr, flags & PcmpBP);
#ifndef PLAN9K
	apic->type = type;
#endif
	USED(type);
	apic->flags = flags | PcmpEN | PcmpUsed;
	return 0;
}

static int mpmachno;

static int
mpnewproc0(Apic *apic, int apicno, int flags)
{
	/* sets apic->machno */
	if (apicset(apic, PcmpPROCESSOR, apicno, flags) < 0)
		return -1;
#ifndef PLAN9K
	apic->lintr[1] = apic->lintr[0] = ApicIMASK;	/* TODO */
	/* apicset already set apic->machno */
	if(apic->flags & PcmpBP)
		apic->machno = 0;
	else
		apic->machno = ++mpmachno;
#endif
	return 0;
}

int
mpnewproc(Apic *apic, int apicno, int flags)
{
	if(apic->flags & PcmpUsed) {
		print("mpnewproc: apic already enabled\n");
		return -1;
	}
	return mpnewproc0(apic, apicno, flags);
}

static void
mpacpitbl(uchar *p)
{
	/* for now, just activate any idle cpus */
	if (memcmp(p, "APIC", 4) == 0)
		mpacpicpus((Madt *)p);
}

static void
mpacpi(void)
{
	mpdebug |= getconf("*debugmp") != nil;
	mpacpiscan(mpacpitbl);
}

void	(*mpacpifunc)(void) = mpacpi;
