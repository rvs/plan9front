/*
 * byte serialization
 */
#include "u.h"
#include "../port/lib.h"
#include "mem.h"
#include "dat.h"
#include "fns.h"

ulong
legetl(void *vp)
{
	uchar *p;

	p = vp;
	return p[3]<<24 | p[2]<<16 | p[1]<<8 | p[0];
}

ulong
begetl(void *vp)
{
	uchar *p;

	p = vp;
	return p[0]<<24 | p[1]<<16 | p[2]<<8 | p[3];
}

void *
leputl(void *vp, ulong l)
{
	uchar *p;

	p = vp;
	*p++ = l; l >>= 8;
	*p++ = l; l >>= 8;
	*p++ = l; l >>= 8;
	*p++ = l;
	return p;
}

void *
beputl(void *vp, ulong l)
{
	uchar *p, *ep;

	p = vp;
	p += sizeof l;
	ep = p;
	*--p = l; l >>= 8;
	*--p = l; l >>= 8;
	*--p = l; l >>= 8;
	*--p = l;
	return ep;
}

void *
leputvl(void *vp, uvlong l)
{
	uchar *p;

	p = vp;
	*p++ = l; l >>= 8;
	*p++ = l; l >>= 8;
	*p++ = l; l >>= 8;
	*p++ = l; l >>= 8;
	*p++ = l; l >>= 8;
	*p++ = l; l >>= 8;
	*p++ = l; l >>= 8;
	*p++ = l;
	return p;
}

void *
beputvl(void *vp, uvlong l)
{
	uchar *p, *ep;

	p = vp;
	p += sizeof l;
	ep = p;
	*--p = l; l >>= 8;
	*--p = l; l >>= 8;
	*--p = l; l >>= 8;
	*--p = l; l >>= 8;
	*--p = l; l >>= 8;
	*--p = l; l >>= 8;
	*--p = l; l >>= 8;
	*--p = l;
	return ep;
}
