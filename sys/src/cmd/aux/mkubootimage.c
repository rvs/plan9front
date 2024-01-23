/*
 * Quick and dirty u-boot mkImage utility for gzipped Plan 9 kernels.
 * See https://github.com/u-boot/u-boot/blob/master/include/image.h
 */

#include <u.h>
#include <libc.h>
#include <ip.h>
#include <flate.h>

enum {
	MAGIC		= 0x27051956,
	OS_OPENBSD	= 1,
	OS_LINUX	= 5,		/* only this starts all harts */
	OS_UBOOT	= 17,
	OS_PLAN9	= 23,

	IMAGETYPE_STAND	= 1,
	IMAGETYPE_KERNEL= 2,		/* only this starts all harts */
	IMAGETYPE_SCRIPT= 6,

	ARCH_ARM	= 2,
	ARCH_RISCV	= 26,

	COMPRESSION_NONE= 0,
	COMPRESSION_GZIP,
	COMPRESSION_BZIP2,
	COMPRESSION_LZMA,		/* maybe lzip? */
	COMPRESSION_LZO,
	COMPRESSION_LZ4,
	COMPRESSION_ZSTD,
	COMPRESSION_GOK,

	ADDR		= 0x80000000,	/* load address & default entry point */
	CRC_POLY	= 0xEDB88320,
};

typedef struct hdr Hdr;
struct hdr {
	uint	magic;
	uint	headercrc;
	uint	time;
	uint	size;
	uint	addr;		/* data load address */
	uint	epaddr;		/* entry point address */
	uint	datacrc;
	uchar	os, arch, imagetype, compression;
	char	name[32];
};
typedef struct scripthdr Scripthdr;
struct scripthdr {
	uint	size;
	uint	zero;
};

typedef struct Archmap Archmap;
struct Archmap {
	char	*objtype;
	int	archid;
};
Archmap archmap[] = {
	"arm",		ARCH_ARM,
	"arm64",	ARCH_ARM,
	"riscv",	ARCH_RISCV,
	"riscv64",	ARCH_RISCV,
};

ulong loadaddr = ADDR;
ulong *crctab;
char filemag[4];	/* for magic compression number */

static int copy(int, int, uint*);

static void
usage(void)
{
	fprint(2, "Usage: %s [-t] [-l loadaddr] [9xxx[.gz] [uImage]]\n", argv0);
	exits("usage");
}

static int
magtotype(uchar *buf)
{
	if(buf[0]==0x1f && buf[1]==0x8b)
		return COMPRESSION_GZIP;
	else if(memcmp(buf, "BZh", 3) == 0)
		return COMPRESSION_BZIP2;
	else if(memcmp(buf, "LZIP", 4) == 0)
		return COMPRESSION_LZMA;		/* we hope */
	else
		return COMPRESSION_NONE;
}

void
main(int argc, char **argv)
{
	int fin = 0, fout = 1, script = 0;
	uint crc, n;
	char *objtype;
	Archmap *amp;
	Dir *dir;
	Hdr *h;
	Scripthdr sh;

	objtype = getenv("objtype");
	if (objtype == nil)
		objtype = getenv("cputype");
	ARGBEGIN {
	case 'l':
		loadaddr = atoll(EARGF(usage()));
		break;
	case 't':
		script = 1;
		break;
	default:
		usage();
		break;
	}ARGEND
	switch (argc) {
	case 2:
		if ((fout = create(argv[1], OWRITE, 0666)) < 0)
			sysfatal("%r");
		/* fall through */
	case 1:
		if ((fin = open(argv[0], OREAD)) < 0)
			sysfatal("%r");
		/* fall through */
	case 0:
		break;
	default:
		usage();
	}

	/* emit the u-boot image header */
	if ((dir = dirfstat(fin)) == nil)
		sysfatal("%s: %r", argv[1]);
	h = mallocz(sizeof *h, 1);
	hnputl(&h->magic, MAGIC);
	hnputl(&h->time, dir->mtime);
	n = (uint)dir->length;
	memset(&sh, 0, sizeof sh);
	if (script){
		hnputl(&sh.size, n);
		n += sizeof sh;
		h->imagetype = IMAGETYPE_SCRIPT;
	} else {
		hnputl(&h->addr, loadaddr);
		hnputl(&h->epaddr, loadaddr);	/* +0x20 or 0x28 for a.out */
		h->imagetype = IMAGETYPE_KERNEL;
//		h->os = OS_UBOOT;		/* starts hart 1 only */
		h->os = OS_LINUX; /* bootm starts all risc-v harts on icicle */
		h->arch = ARCH_RISCV;		/* default */
	}
	hnputl(&h->size, n);

	for (amp = archmap; amp < archmap + nelem(archmap); amp++)
		if (objtype && strcmp(objtype, amp->objtype) == 0) {
			h->arch = amp->archid;
			break;
		}

	if (readn(fin, filemag, sizeof filemag) != sizeof filemag)
		sysfatal("input shorter than %d bytes", (int)sizeof filemag);
	h->compression = magtotype((uchar *)filemag);
	strncpy(h->name, dir->name, sizeof(h->name));

	crctab = mkcrctab(CRC_POLY);

	if (seek(fout, sizeof *h, 0) < 0)
		sysfatal("can't seek on fd %d", fout);
	crc = 0;
	if (script) {
		crc = blockcrc(crctab, crc, &sh, sizeof sh);
		if (write(fout, &sh, sizeof sh) != sizeof sh)
			sysfatal("error writing script header");
	}
	crc = blockcrc(crctab, crc, filemag, sizeof filemag);
	if (write(fout, filemag, sizeof filemag) != sizeof filemag)
		sysfatal("error writing output file");
	if (copy(fin, fout, &crc) < 0)
		sysfatal("error writing output file");
//	if (script)
//		crc = 0;			/* no crc */
	hnputl(&h->datacrc, crc);
	hnputl(&h->headercrc, blockcrc(crctab, 0, h, sizeof *h));

	if (seek(fout, 0, 0) != 0 || write(fout, h, sizeof *h) != sizeof *h)
		sysfatal("error writing u-boot header");
	exits(0);
}

static int
copy(int fin, int fout, uint *acrc)
{
	char buf[64*1024];
	uint crc;
	int n;
	
	crc = *acrc;
	while ((n = read(fin, buf, sizeof buf)) > 0) {
		crc = blockcrc(crctab, crc, buf, n);
		if (write(fout, buf, n) != n)
			return -1;
	}
	if (n < 0)
		return -1;
	*acrc = crc;
	return 0;
}
