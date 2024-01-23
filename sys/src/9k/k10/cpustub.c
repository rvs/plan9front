/*
 * stub to allow linking with vga support, notably devdraw, devmouse and screen
 * but without vga.
 */
#include "u.h"
#include "../port/lib.h"
#include "mem.h"
#include "dat.h"
#include "fns.h"
#include "io.h"

#define	Image	IMAGE
#include <draw.h>
#include <memdraw.h>
#include <cursor.h>
#include "screen.h"

void
drawactive(int)
{
}

void
vgablank(VGAscr *, int)
{
}
