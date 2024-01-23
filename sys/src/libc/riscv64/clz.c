/* count leading zero bits */
#include <u.h>
#include <libc.h>
#include <tos.h>
#include <cpucap.h>

int	_clz(uvlong n);
int	_clzzbb(uvlong n);

int
clz(uvlong n)
{
	if (_tos && _tos->cpucap & Capclz)
		return _clzzbb(n);
	return _clz(n);
}
