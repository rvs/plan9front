#include	"u.h"
#include	"../port/lib.h"
#include	"mem.h"
#include	"dat.h"
#include	"fns.h"
#include	"../port/error.h"

#include	<a.out.h>

static ulong
l2be(long l)
{
	uchar *cp;

	cp = (uchar*)&l;
	return (cp[0]<<24) | (cp[1]<<16) | (cp[2]<<8) | cp[3];
}


static void
readn(Chan *c, void *vp, long n)
{
	char *p;
	long nn;

	p = vp;
	while(n > 0) {
		nn = c->dev->read(c, p, n, c->offset);
		if(nn == 0)
			error(Eshort);
		c->offset += nn;
		p += nn;
		n -= nn;
	}
}

static void
setbootcmd(int argc, char *argv[])
{
	char *buf, *p, *ep;
	int i;

	buf = malloc(1024);
	if(buf == nil)
		error(Enomem);
	p = buf;
	ep = buf + 1024;
	for(i=0; i<argc; i++)
		p = seprint(p, ep, "%q ", argv[i]);
	*p = 0;
	ksetenv("bootcmd", buf, 1);
	free(buf);
}

void
rebootcmd(int argc, char *argv[])
{
	Chan *c;
	Exec exec;
	ulong magic, text, rtext, entry, data, size;
	uvlong stva;
	uchar *p;

	if(argc == 0)			/* just `halt'? */
		exit(0);

	c = namec(argv[0], Aopen, OEXEC, 0);
	if(waserror()){
		cclose(c);
		nexterror();
	}

	memset(&exec, 0, sizeof exec);
	readn(c, &exec, sizeof(Exec));
	magic = l2be(exec.magic);

	/*
	 * AOUT_MAGIC is sometimes defined like this:
	 * #define AOUT_MAGIC	V_MAGIC || magic==M_MAGIC
	 * so we can only use it in a fairly stylized manner.
	 */
	if((magic == AOUT_MAGIC) || isokkernel && isokkernel(magic)) {
		entry = l2be(exec.entry);
		text = l2be(exec.text);
		data = l2be(exec.data);
	} else {
		error(Ebadexec);
		notreached();
	}
	/*
	 * for 64-bit kernels, consume the uvlong start address, even if
	 * we don't yet use it.
	 */
	stva = 0;
	if(magic & HDR_MAGIC)
		readn(c, &stva, sizeof stva);

	/* round text out to page boundary */
	rtext = PGROUND(entry+text) - entry;
	size = rtext + data;
	p = malloc(size);
	if(p == nil)
		error(Enomem);

	if(waserror()){
		free(p);
		nexterror();
	}

	readn(c, p, text);
	readn(c, p + rtext, data);

	ksetenv("bootfile", argv[0], 1);
	setbootcmd(argc-1, argv+1);

	reboot((void*)entry, p, size);

	panic("return from reboot!");
}
