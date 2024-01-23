/*
 * high-watermark measurements
 */
#include	"u.h"
#include	"../port/lib.h"
#include	"mem.h"
#include	"dat.h"
#include	"fns.h"

void
initmark(Watermark *wp, int max, char *name)
{
	memset(wp, 0, sizeof *wp);
	wp->max = max;
	wp->name = name;
}

void
notemark(Watermark *wp, int val)
{
	ilock(wp);
	/* enforce obvious limits */
	if (val < 0)
		val = 0;
	else if (val > wp->max)
		val = wp->max;

	if (val > wp->highwater) {
		wp->highwater = val;
		/*
		 * `>=' because with rings, we may not be able to fill the last
		 * slot.
		 */
		if (val >= wp->max-1 && wp->curr < val)
			wp->hitmax++;
	}
	wp->curr = val;
	iunlock(wp);
}

char *
seprintmark(char *buf, char *ebuf, Watermark *wp)
{
	return seprint(buf, ebuf, "%s:\thighwater %d/%d curr %d hitmax %d\n",
		wp->name, wp->highwater, wp->max, wp->curr, wp->hitmax);
}
