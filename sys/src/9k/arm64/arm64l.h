
#define NREGS	32		/* general-purpose regs, R0 through R31 */

#define USER	26			/* up-> */
#define MACH	27			/* m-> */
#define ARG	0
#define TMP	2

#define XLEN	8
#define RET	RETURN
#define FENCE	DSB $BARRFULL
#define FENCE_I	ISB $BARRFULL

#define BARRFULL 017
#define BARRIERS  DMB $BARRFULL; DSB $BARRFULL; ISB $BARRFULL

#define UREGSIZE (XLEN*(32+6))	/* room for pc, regs, csrs, mode, scratch */

#define MRSN1(sysargs, rt) WORD $(0xd5000000 | (sysargs) | (rt) | 1<<21)
#define MSRN1(rs, sysargs) WORD $(0xd5000000 | (sysargs) | (rs))

#define	SYSARG5(op0, op1, Cn, Cm, op2) ((op0)<<19 | (op1)<<16 | (Cn)<<12 | (Cm)<<8 | (op2)<<5)
#define SCTLR_EL1	SYSARG5(3, 0, 1, 0, 0)

#define IMASK	(1<<7)		/* I bit */
#define DAIFSHIFT 6

#define	MMUON	(1<<0)
#define DCACHEON (1<<2)
#define ICACHEON (1<<12)

#define PsrMusr		0x00000010		/* mode */
#define PsrMfiq		0x00000011
#define PsrMirq		0x00000012
#define PsrMsvc		0x00000013	/* `protected mode for OS' */
#define PsrMmon		0x00000016	/* `secure monitor' (trustzone hyper) */
#define PsrMabt		0x00000017
#define PsrMund		0x0000001B
#define PsrMsys		0x0000001F	/* `privileged user mode for OS' (trustzone) */
#define PsrMask		0x0000001F

#define PsrThumb	0x00000020		/* beware hammers */
#define PsrDfiq		0x00000040		/* disable FIQ interrupts */
#define PsrDirq		0x00000080		/* disable IRQ interrupts */
#define PsrDasabt	0x00000100		/* disable asynch aborts */
#define PsrBigend	0x00000200		/* E: big-endian data */

#define PsrGe		0x000f0000		/* `>=' bits */
#define PsrMbz20	0x00f00000		/* MBZ: 20 - 23 */
#define PsrJaz		0x01000000		/* java mode */
#define PsrIT		0x0600fc00		/* IT: if-then thumb state */
#define PsrQ		0x08000000		/* cumulative saturation */

#define PsrV		0x10000000		/* overflow */
#define PsrC		0x20000000		/* carry/borrow/extend */
#define PsrZ		0x40000000		/* zero */
#define PsrN		0x80000000		/* negative/less than */

#define PsrMbz		(PsrQ|PsrJaz|PsrMbz20|PsrThumb|PsrBigend|PsrDasabt)


#define MTCP	MCR
#define MFCP	MRC


#define CONDITION(inst)	((ulong)(inst) >> 28)
#define OPCODE(inst)	(((inst)>>24) & MASK(4))	/* co-proc opcode */
#define CPUREGN(inst)	(((inst)>>16) & MASK(4))	/* Rn */
#define CPUREGD(inst)	(((inst)>>12) & MASK(4))	/* Rd */
#define COPROC(inst)	(((inst)>> 8) & MASK(4))	/* co-proc number */

#define VFPREGN(inst)	(((inst)>>16) & MASK(4))	/* Vn */
#define VFPREGD(inst)	(((inst)>>12) & MASK(4))	/* Vd */

#define ISCPOP(op)	((op) == 0xE || ((op) & ~1) == 0xC)
#define ISFPOP(cp)	((cp) == CpOFPA || (cp) == CpDFP || (cp) == CpFP)

#define CpOFPA		1			/* ancient 7500 FPA */
#define CpFP		10			/* float VFP and config */
#define CpDFP		11			/* double VFP */
#define CpSC		15			/* System Control */

#define	CpID		0			/* ID and cache type */
#define	CpCONTROL	1			/* miscellaneous control */
#define	CpTTB		2			/* Translation Table Base(s) */
#define	CpDAC		3			/* Domain Access Control */
#define	CpFSR		5			/* Fault Status */
#define	CpFAR		6			/* Fault Address */
#define	CpCACHE		7			/* cache/write buffer control */
#define	CpTLB		8			/* TLB control */
#define	CpCLD		9			/* L2 Cache Lockdown, op1==1 */
#define CpTLD		10			/* TLB Lockdown, with op2 */
#define CpVECS		12			/* vector bases, op1==0, Crm==0, op2s (cortex) */
#define	CpPID		13			/* Process ID */
#define CpDTLB		15			/* TLB, L1 cache stuff (cortex) */

#define CpTTB0		0			/* secure ttb */
#define CpTTB1		1			/* non-secure ttb (v7) */
#define CpTTBctl	2			/* v7 */

#define TTBIRGNwb	(1<<6)			/* inner write-back */
#define TTBNos		(1<<5)			/* inner shareable */
#define TTBRGNnowralloc	(1<<4)			/* outer no write alloc */
#define TTBRGNwb	(1<<3)			/* outer write-back */
#define TTBShare	(1<<1)			/* shareable */
#define TTBIRGNnowralloc (1<<0)			/* inner no write alloc */

#define TTBlow		(TTBIRGNwb|TTBRGNwb|TTBShare|TTBNos)

#define CpDFSR		0			/* data fault status */
#define CpIFSR		1			/* instruction fault status */

#define CpDFAR		0			/* data fault address */
#define CpIFAR		2			/* instruction fault address */

#define CpIDidct	0
#define CpIDidmmfr	1			/* memory model features */

#define CpIDid		0			/* main ID */
#define CpIDct		1			/* cache type */
#define CpIDtlb		3			/* tlb type (cortex) */
#define CpIDmpid	5			/* multiprocessor id (cortex) */

#define CpIDcsize	1			/* cache size (cortex) */
#define CpIDcssel	2			/* cache size select (cortex) */

#define CpIDcasize	0			/* cache size */
#define CpIDclvlid	1			/* cache-level id */

#define CpIDidmmfr0	4			/* base of 4 regs */

#define CpMainctl	0		/* sctlr */
#define CpAuxctl	1
#define CpCPaccess	2

#define CpCmmu		(1<<0)	/* M: MMU enable */
#define CpCalign	(1<<1)	/* A: alignment fault enable */
#define CpCdcache	(1<<2)	/* C: data and unified caches on */
#define CpC15ben	(1<<5)	/* cp15 barrier enable */
#define CpCbigend	(1<<7)	/* B: big-endian data */
#define CpCsw		(1<<10)	/* SW: SWP(B) enable (deprecated in v7) */
#define CpCpredict	(1<<11)	/* Z: branch prediction (armv7) */
#define CpCicache	(1<<12)	/* I: instruction cache on */
#define CpChv	(1<<13)		/* V: use high vectors at 0xffff0000 */
#define CpCrr	(1<<14)		/* RR: round robin vs random cache replacement */
#define CpCha	(1<<17)		/* HA: hw access flag enable */
#define CpCwxn	(1<<19)		/* WXN: write implies XN (VE) */
#define CpCuwxn	(1<<20) /* UWXN: force XN for unpriv. writeable regions (VE) */
#define CpCfi	(1<<21)		/* FI: fast intrs */
#define CpCve	(1<<24)		/* VE: intr vectors enable */
#define CpCee	(1<<25)		/* EE: exception endianness: big */
#define CpCnmfi	(1<<27)		/* NMFI: non-maskable fast intrs. (RO) */
#define CpCtre	(1<<28)		/* TRE: TEX remap enable */
#define CpCafe	(1<<29)		/* AFE: access flag (ttb) enable */
#define CpCte	(1<<30)		/* TE: thumb exceptions */

#define CpCsbz (1<<31 | CpCte | CpCafe | CpCtre | 1<<26 | CpCee | CpCve | \
#define CpCsbo (3<<22 | 1<<18 | 1<<16 | CpCrr | CpCsw | 1<<6 | \

#define CpACparity	(1<<9)
#define CpACca1way	(1<<8)	/* cache in a single way */
#define CpACcaexcl	(1<<7)	/* exclusive cache */
#define CpACsmpcoher	(1<<6)	/* SMP: scu keeps caches coherent; */
#define CpAClwr0line	(1<<3)	/* write full cache line of 0s; see Fullline0 */
#define CpACl1pref	(1<<2)	/* l1 prefetch enable */
#define CpACl2pref	(1<<1)	/* l2 prefetch enable */
#define CpACmaintbcast	(1<<0)	/* FW: broadcast cache & tlb maint. ops */

#define CpCONTROLscr	1

#define CpSCRscr	0		/* secure configuration */
#define CpSCRdebug	1		/* undocumented errata workaround */

#define CpCACHEinviis	1		/* inv i-cache to inner-sharable (v7) */
#define CpCACHEinvi	5		/* inv i-cache to pou, branch table */
#define CpCACHEinvd	6		/* inv data or unified to poc */
#define CpCACHEva2pa	8		/* va -> pa translation (cortex) */
#define CpCACHEwb	10		/* writeback to poc */
#define CpCACHEwbsepou	11		/* wb data or unified by mva to pou */
#define CpCACHEwbi	14		/* writeback+invalidate to poc */

#define CpCACHEall	0		/* entire (not for invd nor wb(i) on cortex) */
#define CpCACHEse	1		/* single entry by va */
#define CpCACHEsi	2		/* set/index (set/way) */
#define CpCACHEinvuis	3		/* inv. u. tlb to is */
#define CpCACHEwait	4		/* wait (prefetch flush on cortex) */
#define CpCACHEflushbtc 6		/* flush branch-target cache (cortex) */
#define CpCACHEflushbtse 7		/* ⋯ or just one entry in it (cortex) */

#define CpTLBinvuis	3		/* unified on all inner sharable cpus */
#define CpTLBinvi	5		/* instruction (deprecated) */
#define CpTLBinvd	6		/* data (deprecated) */
#define CpTLBinvu	7		/* unified */

#define CpTLBinv	0		/* invalidate all */
#define CpTLBinvse	1		/* invalidate by mva */
#define CpTLBasid	2		/* by ASID (cortex) */
#define CpTLBallasid	3		/* by mva, all ASIDs (cortex) */

#define CpCLDena	12		/* enables */
#define CpCLDcyc	13		/* cycle counter */
#define CpCLDuser	14		/* user enable */

#define CpCLDenapmnc	0
#define CpCLDenacyc	1

#define CpCLDl2		0		/* l2 cache */

#define CpCLDl2aux	2		/* auxiliary control */

#define CpCl2ecc	(1<<28)			/* use ecc, not parity */
#define CpCl2noldforw	(1<<27)			/* no ld forwarding */
#define CpCl2nowrcomb	(1<<25)			/* no write combining */
#define CpCl2nowralldel	(1<<24)			/* no write allocate delay */
#define CpCl2nowrallcomb (1<<23)		/* no write allocate combine */
#define CpCl2nowralloc	(1<<22)			/* no write allocate */
#define CpCl2eccparity	(1<<21)			/* enable ecc or parity */
#define CpCl2inner	(1<<16)			/* inner cacheability */

#define CpTLDlock	0			/* TLB lockdown registers */
#define CpTLDpreload	1			/* TLB preload */

#define CpTLDi		0			/* TLB instr. lockdown reg. */
#define CpTLDd		1			/* " data " " */

#define CpVECSbase	0

#define CpVECSnorm	0			/* (non-)secure base addr */
#define CpVECSmon	1			/* secure monitor base addr */

#define CpDTLBmisc	0

#define CpDTLBmisc1	0
#define CpDTLBcbar1	4	/* config base address (periphbase for scu) */

#define CpDTLBpower	0	/* power control */
#define CpDTLBdiag	1	/* `undocumented diagnostic register' */

#define CpDTLBcbar2	0	/* config base address (periphbase for scu) */

#define L1pagesmbz	(0<<4)			/* L1 page tables: must be 0 */
#define Noexecsect	(1<<4)			/* L1 sections: no execute */
#define Fault		0x00000000		/* L[12] pte: unmapped */

#define Coarse		(L1pagesmbz|1)		/* L1: page table */
#define Section		(L1pagesmbz|2)		/* L1 1MB */
#define L1wralloc	(1<<12)			/* L1 TEX */
#define L1sharable	(1<<16)
#define L1nonglobal	(1<<17)			/* tied to asid */
#define Nonsecuresect	(1<<19)			/* L1 sections */

#define Large		0x00000001		/* L2 64KB */
#define Noexecsmall	1			/* L2 4KB page: no execute */
#define Small		0x00000002		/* L2 4KB */
#define Buffered	0x00000004		/* L[12]: 0 write-thru, 1 -back */
#define Cached		0x00000008		/* L[12] */
#define L2wralloc	(1<<6)			/* L2 TEX (small pages) */
#define L2apro		(1<<9)			/* L2 AP: read only */
#define L2sharable	(1<<10)
#define L2nonglobal	(1<<11)			/* tied to asid */
#define Dom0		0

#define L1ptedramattrs	(Cached | Buffered | L1wralloc | L1sharable)
#define L2ptedramattrs	(Cached | Buffered | L2wralloc | L2sharable)

#define Noaccess	0			/* AP, DAC */
#define Krw		1			/* AP */
#define Uro		2			/* AP */
#define Urw		3			/* AP */
#define Client		1			/* DAC */
#define Manager		3			/* DAC */

#define AP(n, v)	F((v), ((n)*2)+4, 2)
#define L1AP(ap)	(AP(3, (ap)))
#define L2AP(ap)	(AP(0, (ap)))		/* armv7 */
#define DAC(n, v)	F((v), (n)*2, 2)

#define PteP		1<<8			/* Present/valid */
#define PteR		1<<7			/* Read */
#define PteW		0			/* read/Write */
#define PtenX		1LL<<54			/* no-Execute */
#define PteU		1<<6
#define PtenG		1<<9			/* not Global; used for lower addresses */

#define PteRSW		0			/* reserved for sw use */
#define PtePS		0			/* TODO: super page leaf */

#define Ptepma		0			/* none: cacheable */
#define Ptenc		(1LL<<61)		/* non-cacheable memory */
#define Pteio		(2LL<<61)		/* non-cacheable i/o space */

#define PteLeaf		(PteR|PteW|PtenX)	/* if 0, next level; non-0 leaf */
#define Pteleafmand	0
#define Pteleafvalid	(Pteleafmand|PteP)
#define PteAttrs	(MASK(6)|PteRSW)
