/* Zbb extension, as found in sifive u74 */
#define CLZ(rs1, rd) \
	WORD $(0x30<<25 | 0<<20 | (rs1)<<15 | 1<<12 | (rd)<<7 | 0x13)

ARG=8

/* Zbb extension, as found in sifive u74 */
TEXT _clzzbb(SB), 1, $-4			/* int _clzzbb(uvlong) */
	CLZ(ARG, ARG)
	RET
