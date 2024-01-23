.nr PS 12
.nr VS 14
.FP palatino
.
.TL
Widening C Library Size Arguments in
.I "Plan 9"
and
.I 9k
.AU
Geoff Collyer
.
.SH
Integer types in 64-bit ports
.LP
.I 9k
is a 64-bit capable version of Plan 9.
.I 9k
as delivered had widened many
.I ulong s
to
.I uintptr ,
which can hold a pointer,
as part of making the kernel 64-bit clean,
but it did not address actually exploiting
the larger address space, particularly in user processes.
.LP
For 32-bit Plan 9 (and potentially
.I 9k )
systems,
.I uintptr
is
.I ulong .
For 64-bit systems,
.I uintptr
is
.I uvlong .
Thus changing
.I ulong
in a declaration to
.I uintptr
is a no-op on 32-bit systems, thus harmless.
.LP
An obvious case of wanting
.I libc
functions to have their size arguments
(and internal integers)
widened is
.I malloc .
If one wants, as with the kernel's
.I Pages
array,
to allocate an array that may be larger than 2 or 4 GB,
.I malloc
itself (in the kernel),
.I mallocz
(in user space),
or the user
will likely invoke
.I memset
to zero such an array.
To avoid making a special case of such arrays
(per Intel's x86 compiler mistakes),
.I memset
also needs to have its final argument widened to
.I uintptr
(and
.CW sizeof 's
result should have the same type,
which requires only a very small change to the C compiler common code).
.
.SH
False economy
.LP
POSIX's
.I size_t
and Plan 9's analogous
.I usize
really need to be
.I uintptr
or not be used at all
(Native Plan 9 is not bound by POSIX).
If they are shorter than
.I uintptr ,
one gets into the world of pain
from Intel's past
mixing 16- and 32-bit pointers
in a short-sighted attempt at efficiency.
Think
.I near ,
.I far ,
etc. pointers.
It is far better to accept perceived short-term
efficiency losses by using 64-bit integers on 64-bit ports.
The long-term trends are to machines with larger memories and wider CPUs.
It's difficult to buy a 32-bit-only Unix-capable system
or one with less than 1GB of RAM these days.
The long dominance of 32-bit systems was temporary.
.QS
.ps +2
.vs +2
.B N.B.:
This has virtually no effect on 32-bit systems
(since
.I uintptr
is
.I ulong ),
so doesn't break compatibility with existing
Plan 9 systems,
while allowing programs that wish to allocate vast arrays of buffers
(e.g.,
.I fossil
or
.I venti )
to do so straight-forwardly.
.ps
.vs
.QE
.
.SH
Trade-offs
.LP
If you have no need of large address spaces, you need not adopt
the change to
.I malloc
and
.I memset 's
final arguments.
.QS
.ps +2
.vs +2
.B N.B.:
If you have more than about 8GB of RAM, you need large address spaces
(for
.I Page
structs).
.QE
This is an entirely optional change, and not an integral part of
.I 9k ,
but be aware that you will need to use an alternate
.CW libc/riscv64/memset.s
(provided)
on
.CW riscv64
systems if you choose to not widen arguments.
.QS
.ps +2
.vs +2
.B N.B.:
As long as your
.CW libc.h 's
declaration of
.I memset ,
etc.
agrees with the implementations of
.I memset ,
etc.
in your C library, either choice of argument width will work.
.QE
.\" Note that using
.\" .I usize
.\" uniformly instead of
.\" .I ulong
.\" for sizes, and widening that
.\" would break binary compatibility
.\" with existing
.\" .CW amd64
.\" binaries
.\" (e.g., in the dump)
.\" in the system call interface.
