/*
 * fake interface to missing sbi (bios/firmware) for rv64
 */
#include "u.h"
#include "../port/lib.h"
#include "mem.h"
#include "dat.h"
#include "fns.h"

int nosbi = 1;

vlong
sbicall(uvlong, uvlong, uvlong, Sbiret *, uvlong *)
{
	return -1;
}
