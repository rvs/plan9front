/* count leading zero bits */
#include <u.h>
#include <libc.h>

int	_clz(uvlong);

int
clz(uvlong n)
{
	return _clz(n);
}
