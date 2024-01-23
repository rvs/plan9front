/*
 * (i)(un)lock, canlock - spinlocks with test-and-set, for short-lived locks
 *
 * recently added more coherence calls to push changes to other cpus sooner.
 */
#include "u.h"
#include "../port/lib.h"
#include "mem.h"
#include "dat.h"
#include "fns.h"

#include "../port/edf.h"

enum {
	Lockdebug = 0,
	LOCKCYCLES = 0,
};

/* there are races updating all of these, so they are only approximate */
uvlong	maxlockcycles;
uintptr	maxlockpc;
uvlong	maxilockcycles;
uintptr	maxilockpc;
struct
{
	ulong	locks;
	ulong	glare;
	ulong	inglare;
} lockstats;

static void
lockloop(Lock *l, uintptr pc)
{
	Proc *p;

	p = l->p;
	iprint("lock %#p loop key %#ux pc %#p held by pc %#p proc %d\n",
		l, l->key, pc, l->pc, p? p->pid: 0);
	dumpaproc(up);
	if(p != nil)
		dumpaproc(p);
}

static void
userlock(Lock *lck, char *func, uintptr pc)
{
	panic("%s: nil or user-space Lock* %#p; called from %#p", func, lck, pc);
}

/*
 * Priority inversion, yield on a uniprocessor;
 * on a multiprocessor, another processor will unlock.
 * Not a common code path.
 */
static void
edfpriinv(Lock *l, uintptr pc)
{
	print("inversion %#p pc %#p proc %d held by pc %#p proc %d\n",
		l, pc, up? up->pid: 0, l->pc, l->p? l->p->pid: 0);
	up->edf->d = todget(nil);	/* yield to process with lock */
}

/* call once we have l->key (via TAS) */
#define TAKELOCK(l, callpc) { \
	if(up) \
		up->lastlock = l; \
	l->pc = callpc; \
	l->p = up; \
	l->isilock = 0; \
	if (LOCKCYCLES) cycles(&l->lockcycles); \
	coherence();		/* make changes visible to other cpus */ \
}

/* returns with Lock l held and, if up is non-nil, up->nlocks non-zero */
int
lock(Lock *l)
{
	int x;
	vlong i;
	uintptr pc;
	Edf *edf;

	if (Lockdebug) {
		/*
		 * we can be called from low addresses before the mmu is on.
		 */
		pc = getcallerpc(&l);
		if (l == nil)
			userlock(l, "lock", pc);
		if (l->noninit)
			panic("lock: lock %#p is not a lock, from %#p", l, pc);
	}
	lockstats.locks++;
	if(up)
		ainc(&up->nlocks);	/* prevent being scheded while locked */
	if(TAS(&l->key) == 0){
		TAKELOCK(l, getcallerpc(&l));
		return 0;		/* got it on first try */
	}

	/* slow but less common path; there's contention */
	pc = getcallerpc(&l);
	if(up) {
		x = adec(&up->nlocks);	/* likely allow being scheded */
		if (x < 0)
			panic("lock: ref %d < 0 after adec; callerpc=%#p",
				x, pc);
		if(l->p == up)
			panic("lock: deadlock acquiring lock held by same proc,"
				" from %#p", pc);
	}
	lockstats.glare++;
	edf = sys->nonline <= 1 && up? up->edf: nil;
	for(;;){
		lockstats.inglare++;
		/*
		 * don't use monitor/mwait for l->key; an interrupt could
		 * ilock and change the single per-cpu monitor address used
		 * by lock, for example.
		 */
		i = 0;
		while (l->key != 0) {			/* lock's busy */
			if(edf && edf->flags & Admitted)
				edfpriinv(l, pc);
			if(i++ > 100000000){
				i = 0;
				lockloop(l, pc);
			}
		}

		/* we believe that the lock is free; try again to grab it. */
		if(up)
			ainc(&up->nlocks);
		if(TAS(&l->key) == 0){
			TAKELOCK(l, pc);
			return 1;	/* got it, but not on first try */
		}

		/* still contending */
		if(up) {
			x = adec(&up->nlocks);
			if (x < 0)
				panic("lock: ref %d < 0 after adec; callerpc=%#p",
					x, pc);
		}
	}
}

/* waits for the lock at entry PL, returns at high PL */
void
ilock(Lock *l)
{
	int lo, i;
	uintptr pc;
	Mreg s;

	pc = getcallerpc(&l);
	if (Lockdebug) {
		if (l == nil)
			userlock(l, "ilock", pc);
		if (l->noninit)
			panic("ilock: lock %#p is not a lock (noninit = %#lux),"
				" from %#p", l, l->noninit, pc);
	}
	lockstats.locks++;

	lo = islo();
	s = splhi();
	if(TAS(&l->key) != 0){
		lockstats.glare++;
		/*
		 * if called splhi on a uniprocessor, the loop on l->key!=0
		 * below will run at splhi, and thus cannot succeed as
		 * nothing can change l->key, unless another cpu is spinning
		 * up concurrently, or dma changes l->key, or iunlock writing
		 * 0 to l->key has been vastly delayed in a write buffer.
		 */
		if(!lo && sys->nmach <= 1 && sys->nonline <= 1)
			iprint("ilock: lock %#p: no way out, from %#p splhi\n",
				l, pc);
		/*
		 * Cannot also check l->pc, l->m, or l->isilock here
		 * because they might just not be set yet, or
		 * (for pc and m) the lock might have just been unlocked.
		 */
		do {
			lockstats.inglare++;
			/*
			 * we may be waiting for an iunlock in or triggered
			 * from an interrupt service routine and could be
			 * a uniprocessor.
			 */
			splx(s);
			i = 0;
			while (l->key != 0)		/* lock's busy */
				if(i++ > 100000000){
					i = 0;
					lockloop(l, pc);
				}
			splhi();
		} while (TAS(&l->key) != 0);	/* try to grab lock again */
	}
	/* we have the lock (l->key) */
	if (m)
		m->ilockpc = pc;
	l->sr = s;
	l->m = m;

	if(up)
		up->lastilock = l;
	l->pc = pc;
	l->p = up;
	l->isilock = 1;
	if (LOCKCYCLES)
		cycles(&l->lockcycles);
	coherence();		/* make changes visible to other cpus */

	if (m)
		m->ilockdepth++; /* increment after acquiring lock */
}

int
canlock(Lock *l)
{
	int x;
	uintptr pc;

	pc = getcallerpc(&l);
	if (Lockdebug && l == nil)
		userlock(l, "canlock", pc);
	if(up)
		ainc(&up->nlocks);
	if(TAS(&l->key) != 0){		/* failed to acquire the lock? */
		if(up) {
			x = adec(&up->nlocks);
			if (x < 0)
				panic("canlock: ref %d < 0 after adec; callerpc=%#p",
					x, pc);
		}
		return 0;
	}

	TAKELOCK(l, pc);
	l->m = m;
	return 1;
}

#define GIVELOCKBACK(l) { \
	l->m = nil; \
	l->p = nil; \
	coherence(); \
	/* actual release; data protected by this Lock and the Lock itself */ \
	/* must be current before release. */ \
	l->key = 0; \
	coherence(); \
}

void
unlock(Lock *l)
{
	int x;

	if (LOCKCYCLES) {
		uvlong cyc;

		cycles(&cyc);
		l->lockcycles = cyc - l->lockcycles;
		if(l->lockcycles > maxlockcycles){
			maxlockcycles = l->lockcycles;
			maxlockpc = l->pc;
		}
	}
	if (Lockdebug) {
		uintptr pc;

		pc = getcallerpc(&l);
		if (l == nil)
			userlock(l, "unlock", pc);
		if(l->key == 0)
			iprint("unlock: not locked: pc %#p\n", pc);
		if(l->isilock)
			iprint("unlock of ilock: pc %#p, held by %#p\n", pc, l->pc);
	}

	/*
	 * only the Lock-holding process should release it.  otherwise,
	 * the wrong up->nlocks will be decremented.  a Lock may have been
	 * acquired when up was nil and is being released after setting it,
	 * but Locks are not supposed to be held for long (in particular,
	 * across sleeps or sched calls).
	 *
	 * if l->p is nil, up->nlocks should not have been incremented when
	 * locking, thus should not be decremented here.
	 * l->p == up should always be true.
	 */
	if (l->p != up)
		iprint("unlock: l->p changed: pc %#p, acquired at pc %#p, "
			"lock p %#p != unlock up %#p\n", getcallerpc(&l),
			l->pc, l->p, up);
	GIVELOCKBACK(l);
	/*
	 * Call sched if the need arose while locks were held.
	 */
	if (up) {
		x = adec(&up->nlocks);	/* allow scheding again */
		if (x < 0)
			panic("unlock: ref %d < 0 after adec; callerpc=%#p",
				x, getcallerpc(&l));
		/*
		 * Call sched if the need arose while locks were held, but
		 * don't do it from interrupt routines, hence the islo() test.
		 */
		if (up->nlocks == 0 && up->delaysched && islo())
			sched();
	}
	/* contenders for this lock will be spinning, no need to wake them */
}

void
iunlock(Lock *l)
{
	Mreg s;

	if (LOCKCYCLES) {
		uvlong x;

		cycles(&x);
		l->lockcycles = x - l->lockcycles;
		if(l->lockcycles > maxilockcycles){
			maxilockcycles = l->lockcycles;
			maxilockpc = l->pc;
		}
	}
	if (Lockdebug) {
		if (l == nil)
			userlock(l, "iunlock", getcallerpc(&l));
		if(l->key == 0)
			print("iunlock: not locked: pc %#p\n", getcallerpc(&l));
		if(!l->isilock)
			print("iunlock of lock: pc %#p, held by %#p\n",
				getcallerpc(&l), l->pc);
	}
	if(islo())
		print("iunlock while lo: pc %#p, held by %#p\n",
			getcallerpc(&l), l->pc);
	if(Lockdebug && l->m != m)
		print("iunlock by cpu%d, locked by cpu%d: pc %#p, held by %#p\n",
			(m? m->machno: -1), (l && l->m? l->m->machno: -1),
			getcallerpc(&l), l->pc);

	s = l->sr;
	if (m)
		m->ilockdepth--;	/* decrement before lock release */
	if(up)
		up->lastilock = nil;
	GIVELOCKBACK(l);
	/* contenders for this lock will be spinning, no need to wake them */
	splx(s);
}
