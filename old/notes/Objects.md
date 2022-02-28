
# Runtime objects for functional languages

The goal is to design from scratch runtime objects for strict/lazy functional
languages, optimizing for performance, flexibility and simplicity. General goals and
design principles:

- Simplicity. GHC supports a veritable zoo of runtime objects, but it turns out
  that many of these can be merged. Uniform, simple layout schemes also simplify
  GC code and make it faster. Data which is irrelevant to GC and RTS should not
  be baked into runtime objects but should be treated as generic boxed/unboxed
  payload.
- Support for precise GC. Although reference counting has been recently picked
  up for [Lean 4 and Koka
  runtimes](https://www.microsoft.com/en-us/research/uploads/prod/2020/11/perceus-tr-v1.pdf),
  and benchmarks are pretty good, I'm fairly confident that precise GC with
  sufficient memory allowance should be superior in performance, and is easier
  to compile and implement. In particular, I know that throwing free memory at
  the GHC arena can speed up programs by several times, and in the refcounting
  benchmarks GHC was used with the tiny default arena sizes. In certain cases,
  where we get destructive updates on unique references, refcounting can be
  superior, and refcounting is better in terms of memory management
  pauses. However, my goal here is explicitly to not support realtime tasks; for
  that, memory management based on regions and lifetimes would be better suited
  anyway. In terms of GC throughput and mutator overhead, precise GC should be
  reliably better than refcounting.
- Meshing well with sophisticated staged compilation and monomorphization in
  upstream languages. This means that objects should be flexible enough to
  allow precise layout control by programmers, and/or aggressive layout
  optimization by compilers.
- Performance. Objects should be small and the number of indirections should be
  minimal.
- Only 64 bit architectures are supported.
- Supporting C as compilation target. This means that GHC's "tables besides code"
  optimization is not used.

## Design

### Object granularity

The smallest size of an object is one machine word or 64 bits. While modern
hardware is fairly good at handling unaligned memory access, I see little reason
to go sub-word. Pointers must be word-sized anyway, and given advanced
monomorphization and layout control, it should be possible to pack multiple
sub-word types together to get final representations with word granularity.

### Runtime objects

There are altogether four different kinds of runtime objects:

- Small dynamic objects
- Large dynamic objects
- Static objects
- Indirections

These together support ADTs, boxed/unboxed arrays, closures, thunks,
indirections, statically allocated constructors, static reference tables and
partial applications.

Some features from GHC are not (yet) supported:
- Selector thunks.
- Dirty bits for potential read/write barriers.



    | type: 4 | nptrs: 10 | ptrs: 10 | nptr payload: nptrs*64 | ptr payload: ptrs*64


#### Small dynamic objects

These are the most frequent objects at runtime. Layout:

    | type | nptrs | ptrs | embedded | nptr payload | ptr payload
        2     11      11       40        nptrs*64      ptrs*64

`type` has a concrete value in the 0-3 range which marks the objects as a small
dynamic one. `ptrs` is the number of GC pointers in the object, `nptrs` is the
number of unboxed words. `embedded` is space for various unboxed data, which can
be used in different ways depending on what kind of object we want to represent.

Representing things with small dynamic objects:

- ADT constructors which contain at most 2047 boxed and 2047 unboxed words.  The
  constructor tag is stored in `embedded`. Constructor tags are irrelevant to
  GC, hence they are not treated specially in layouts, and are viewed as
  generic unboxed payload.
- Dynamic closures. For a closure, we need to store:
  + A code pointer: this is simply the first `nptr` payload. Why is it `nptr`,
    isn't a code pointer a pointer? It's not a pointer for GC purposes, since it
    points to static data and cannot be followed by GC. So we just store it as
    unboxed payload.
  + Code arity: stored in `embedded`.
  + Captured environment: stored in the payload section.
  + Static reference table: if the closure refers to any top-level value (which
    is not a function), we create a *static object* which stores these
    references, and we put a pointer to the SRT table into the `ptr` payload
    section of the closure. If the SRT table is empty, we simply omit it from
    the closure! If the SRT table is non-empty, the closure gets an extra SRT
    pointer as payload, and the GC will simply traverse it, because the GC sees
    it as an ordinary static object.
- Thunks: exactly the same as dynamic closures. Thunk update overwrites the header word
  with an indirection to the result.

We don't allow closures and thunks that exceed representable environment
sizes.

Differences from GHC:

- There is no distinction between partial applications and closures.
- Single-word dynamic objects are possible by storing user data in
`embed`. For example, a boxed `Int32` or `Char` may only use a single word.
- There is no pointer tagging.
- There are no info pointers. Hence, there is no dereference overhead on
  switching on constructor tags, or during GC. There are also no statically
  stored info tables, which should result in smaller executable sizes.

It might be problematic in a multi-threaded setting to destructively overwrite
the header in a thunk update, without synchronization. In that case, thunks have
to be an object type which is separate from small objects, and we use the same
solution as in GHC, namely to leave a word empty in the thunk for the forwarding
pointer.

#### Large dynamic objects

These are like small objects, but they have effectively unlimited payload size, and
`nptrs` denotes number of *bytes* instead of words. Layout:

    | type | nptrs | ptrs |   nptr payload     | ptr payload
        2      62     64         nptrs*8          ptrs*64
		                    (padded to words)

Hence, the header here takes up two words.

In principle, everything which is representable using small objects is also
representable using large objects. For ADTs this is fine, because if we know
statically that an ADT has a large runtime representation, then mutator code can
rely on this information. In contrast, with closures and thunks it is a bit
awkward to allow both small and large objects, because then mutator code must
branch on whether something is small or large.

Large objects can represent immutable arrays, and also objects which contain
unpacked fields with statically unknown size. For example, we may unpack an
array field into a different constructor, in which case the length of the array
may get merged into the `ptrs`/`nptrs` data (mutator code would have to take into
account the sizes of other fields to recover the length of the unpacked array).

#### Indirections

These represent
- Forwarding pointers for copying GC
- Evaluated thunks

Layout:

    | type | forward ptr |
	   2         62

The type tag should be 0. This object is just a single pointer to another
object. During copying GC, and indirection signals that the object is already
copied. In thunk forcing, the header of the thunk is overwritten with the
pointer to the result.

Since GC has exactly the same action on forced thunks and evacuated objects
(shortcuts the indirection), we merge the two into the same object type.

#### Static objects

Static objects have exactly the same layout as small dynamic objects, but they are allocated
in static storage, so the GC does not copy them. They can represent
- static closures
- static reference tables (SRTs)
- statically allocated constructors

**Static closures** are the same as their dynamic counterparts, except they capture
nothing from local scope, hence they can be allocated statically. For example,
dynamically passing a top-level function to a higher-order function passes a
pointer to the static closure. Mutator code can assume that any function is a
static or dynamic closure, and since they have the same layout, there's no need to
branch on object type at runtime.

**SRTs** store a list of pointers to certain top-level values. The purpose of SRTs
is to let the GC know which top-level values are referenced from functions, and
thus be able to determine which ones are kept alive by closures. In short, SRTs
are captured top-level environments.

Since top-level capturing is statically known, compilers are also free to
exploit sharing between partially different SRTs, using linked list or tree
structures instead of flat arrays.

List/tree structures can be also used to circumvent the 2048 size limit for flat
pointer arrays in static objects, by linking multiple chunks, although it's not
realistic to exceed the limit in real-world programs.

**Static constructors** could be used to have GHC-like static storage for small
Int-s and Char-s.
