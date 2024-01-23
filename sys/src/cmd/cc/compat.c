#include	"cc.h"
#include	"compat"

/*
 * fake mallocs
 */
void*
malloc(uintptr n)
{
	return alloc(n);
}

void*
calloc(uintptr m, uintptr n)
{
	return alloc(m*n);
}

void*
realloc(void *o, uintptr n)
{
	uintptr m;
	void *a;

	if(n == 0)
		return nil;
	if(o == nil)
		return alloc(n);
	a = alloc(n);
	m = (char*)a - (char*)o;
	if(m < n)
		n = m;
	memmove(a, o, n);
	return a;
}

void
free(void*)
{
}

/* needed when profiling */
void*
mallocz(uintptr size, int clr)
{
	void *v;

	v = alloc(size);
	if(clr && v != nil)
		memset(v, 0, size);
	return v;
}

void
setmalloctag(void*, uintptr)
{
}

void
setrealloctag(void*, uintptr)
{
}
