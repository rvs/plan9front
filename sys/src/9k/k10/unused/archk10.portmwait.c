#ifdef unused
/* interface to the port code */

void
portmonitor(void *a)
{
	if (waitfor == k10waitfor)
		monitor(a, 0, 0);
}

/*
 * only if mwait can be interrupted will we be able to
 * detect lock loops at high PL.
 *
 * this may seem to be a form of optimizing the idle loop, but mwait
 * should consume less power and fewer instructions on many-core systems
 * with more lock contention (e.g., NPROC=24 mk).
 */
void
portmwait(void)
{
	if(sys->haveintrbreaks)
		mwait(Intrbreaks, 0);
	else if (islo())
		mwait(0, 0);
}
#endif
