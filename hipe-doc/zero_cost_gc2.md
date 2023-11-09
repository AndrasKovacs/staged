
# Garbage collection with zero cost at non-GC time

Every once in a while I investigate low-level backend options for PL-s, although
so far I haven't actually written any such backend for my projects. Recently
I've been looking at precise garbage collection in popular backends, and I've
been (like on previous occasions) annoyed by limitations and compromises.

I was compelled to think about a system which accommodates precise relocating GC
as much as possible. In one extreme configuration, described in this note, there
is literally zero overhead of GC when GC is not running. In this case, just by
looking at mutator machine code or heap objects, we see no signs that GC is
being used. There are no runtime checks, no discernible root spilling, no data
headers, no tag bits, no nothing, just a heap pointer which apparently points to
an infinite memory. Such "zero-cost GC" would be "zero-cost" in the same sense
as "zero-cost exceptions" in C++.

We'll see that this can be achieved by using three features at the same time:
1. **relocation of roots in registers**
2. **tag-free GC**
3. **page fault handling**

Out of these, I think that (1) could be quite generally applicable, and it
also simplifies some things. I haven't seen it in implementations, personally.
I also really like (2) and I wonder why it's not more popular in production.
(3) is a bit more ambiguous, it may or may not be possible or worth to do depending on
platform and RTS constraints.

Disclaimer: I'm not an expert in GC or RTS-es. Please feel free to correct
me. Please also tell me if I'm reinventing anything or entertaining ideas
that are already known to be silly or impractical.

Although this note is a *thought experiment*, I do think right now that
implementing it in full would be a good idea.

## 1. Register relocation

Looking at GC features in LLVM, I saw that the [abstraction of
relocation](https://llvm.org/docs/Statepoints.html#id7) is the following: *every
possibly GC-calling function takes live GC roots as extra arguments, and returns
their relocated versions as extra outputs*. In other words, every statepoint
invalidates all GC roots.

This struck me as kind of bad. Designing a pipeline with relocation support
from the ground up, I'd instead go with:

**Relocation is not even observable in the IR.**

This is justified if *all* roots are relocated, including the ones in registers
or perhaps roots stored in obfuscated ways. In that case, if the IR only
uses operations which are invariant under relocation, relocation is truly
invisible.

We can put a root in a preserved register, call a function, get a relocation,
return from the function, and the register will contain the relocated root.
There's no reason to spill anything to memory just because of relocation!

Of course, this only works if root locations are tracked all the way down to
machine code. But this should not be too outrageous because LLVM already tracks
lots of things all the way down to machine code, e.g. in the existing
[statepoint](https://llvm.org/docs/Statepoints.html) and [patch
point](https://llvm.org/docs/StackMaps.html) features. In short, as an RTS
implementor I just want to precisely know where GC roots are at every safepoint,
in an architecture-dependent way. Architecture-dependency is fine because we
already have plenty of it in production RTS-es.

Let's look at a concrete "zero-cost" implementation. First, the basic idea in a
nutshell. At the GC entry point we know for sure that some stack entries and
registers are roots, but we don't know anything about callee-preserved
registers. These might be just not used, or spilled to stack (and later
restored), and in either case we have no idea if there's a GC root there. But if
we walk the stack, eventually we learn the locations of all roots, since we
learn about callee-preserved registers when we move up in the stack. In more detail:

- The IR faithfully tracks where GC roots are stored.
- The code generator emits static metadata for each statepoint (function calls,
  heap allocations). The GC is able to walk the stack by iterating through
  statepoint metadata associated to return addresses. This is standard practice
  in C++ "zero-cost exceptions".
- Statepoints can be function calls or GC entry points (heap allocations).
- At each GC entry point, we know:
  - The location of the caller's return address.
  - Locations of live roots on the stack and in registers.
  - For each callee-saved register, if it's spilled, its location on the stack.
- At each function call statepoint, we know:
  - The location of the the up-stack caller's return address.
  - For each up-stack callee-saved register, if it's spilled, its location on the stack.
  - Locations of live roots on the stack and in registers that are preserved by the call
    down the stack.
- When the GC is called, we create two tables:
  - One table contains the values of all registers on GC entry. When GC finishes
    we reload the table contents into real registers.
  - One table records where registers are spilled by callee-saving during execution,
    between a statepoint and the GC entry point. On GC entry, this table records
	that nothing is spilled.
- GC first processes the GC entry point, then walks the stack and processes the
  function call statepoints.
- At the GC entry point, we relocate roots on the stack and in registers, and
  record spill locations in the spill table.
- At each function call statepoint
  - We relocate roots on the stack.
  - For each root in a callee-saved register, if it has not been spilled, we relocate
    the register, otherwise relocate where it's spilled on stack.
  - We record new callee-save spills in the spill table.

In summary, what we get:
- The IR has to track roots but not relocations. This should be a significant
  simplification and also reduce IR bloat.
- There is no root spilling and reloading. IR optimization can pretend that
  there's no relocation. This would also play along nicely with *interprocedural
  register allocation*.
- The GC has to implement architecture-dependent register relocation.

## 2. Tag-free GC

There's nothing new here; tag-free GC is an old idea that has been investigated
in detail. I give a short summary.

In conventional precise GC implementation, heap objects are annotated with
enough information so that the GC can find all root pointers. There are many
variations.

- [In OCaml](https://dev.realworldocaml.org/runtime-memory-layout.html), every
  word has a tag bit which marks heap references. This makes it possible to
  freely mix unboxed 63-bit-sized data and pointers, and allows OCaml to only
  annotate heap objects with their sizes. On the other hand, integer operations
  on tagged 63-bit values are more awkward.
- [In GHC](https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/rts/storage/heap-objects),
  heap objects are prefixed with a pointer to the actual metadata, called "info table".
  There are several different info table layouts, but it distinguishes boxed and unboxed
  words in memory. So GC tag bits are not used and word-sized unboxed data is natively
  handled.

Finding the best metadata layout is a big topic in itself. In GHC, the info
pointer setup is pretty much a legacy burden from the "tagless" early days, and
IMO nowadays it's more sensible to directly pack metadata in objects instead of
getting them from behind a pointer. One word is enough for the vast majority of
runtime objects and when it's not enough we can just use two tag words. But let's
get back to the topic.

The basic idea of **tag-free GC** is that it's enough to know the types of roots
on the stack. Starting from there, the types (layouts) of all live heap objects
can be deduced. So there is no need to tag heap objects in general.

This clearly requires some degree of typing discipline. It's also a general
phenomenon that the more fancy our type system is, the more runtime type
information we need.

- A truly zero-cost setup is only possible with monomorphic simple types.
  Here, the type of each root on the stack is a concrete monotype which
  can be looked up from static storage.
- With runtime polymorphism, functions may be called with statically unknown
  types. For each such call, a runtime type rep has to be passed. At GC time
  we can learn the dynamic type of each root from the dynamically passed
  type information.
- Values of existential types has to store type reps on the heap as well.

I'm personally a fan of combining runtime monotypes with [staging](https://andraskovacs.github.io/pdfs/2ltt.pdf). This gets us
ample polymorphism at compile time but keeps the RTS simple. So for me it's
fine if I only concern myself with monotypes in the runtime language.

*Closures* are nicely compatible with tag-free GC, we only have to use GHC's
"tables-next-to-code" optimization. This means that metadata is reachable from
the same code pointer that's stored in the closure; the code is prefixed by
metadata. In modern LLVM this is also available as [prefix
data](https://llvm.org/docs/LangRef.html#prefix-data).

It's a natural refinement of tag-free GC to use *GC functions* instead of
type description data.

- In usual GC code, object types are essentially "interpreted", as there's
  a big case switch on object types.
- With GC functions, we generate machine code that performs
  evacuation/relocation, for each type in the program. For lists, the GC
  function only matches on `nil` and `cons` and copies cells over to the
  to-space. So this is a "compiled" GC.

GC functions increase code size, but if we think about it, this is not much
different from basic "deriving" pragmas that Rust or Haskell programmers put on
almost all type definitions (or the
[Typeable](https://hackage.haskell.org/package/base-4.18.0.0/docs/Data-Typeable.html)
instance that is derived by GHC for every type).

Tagged GC is usually implemented using breadth-first copying ([Cheney's
algorithm](https://en.wikipedia.org/wiki/Cheney%27s_algorithm)). Tag-free GC is
more natural with recursive depth-first copying, in which case type information is
stored on the stack, but it also works breadth-first with an auxiliary array of
type reps. In tagged GC, the breadth-first copying uses constant space, but this
is only possible because objects are tagged all the time. Tag-free GC only needs
extra storage during GC. It would be also interesting to allow per-datatype GC
copying strategies, perhaps configured by programmers. This is not possible
with the standard monolithic tagged GC implementations.

Tag-freedom makes it possible to reduce heap usage significantly. Consider lambda
terms in Haskell:

```haskell
    data Tm = Var Int | App Tm Tm | Lam Tm
```

A Church-coded natural number `N` in Haskell takes up `6 + 5*N` words.  With
tag-freedom and constructor tags stored in pointers, it's `3 + 3*N` instead.  If
we use 32-bit `Int` in `Var`, we can unbox the `Var` case, and we get `2 + 2*N`.

In a monomorphized language with tag-free GC, we can do many heap-packing tricks
to compress objects. For example, a `List (Maybe Int)` can be either a null
pointer (`Nil`), or a tagged pointer to two words (`Cons`), where the pointer
tag is actually the `Maybe` constructor tag and the head `Int` is unboxed. In
GHC, the heap size of `[Maybe Int]` is `N * 5` words. In the monomorphized
tag-free setup, it's `N * 2` instead.


## 3. Page fault handling

The last sign of GC that we want to eradicate from programs is **heap overflow
checking**. This checks whether we have enough space in the arena, for each
chunk of heap allocation, and serves as entry point for GC.

If you look at the Cmm or assembly output of GHC, you can see that a significant
portion of the total code size is just heap checking or stack checking (which in
GHC is also essentially heap-allocated and GC-d). We want to get rid of these
checks.

We do this by using protected pages at the end of each heap arena (called *guard
pages*). When we try to write to a guard page, we get a page fault that can be
handled from RTS code and trigger GC.

Page fault handling is also an old idea, and it's used in modern
production. Again there's an [LLVM
feature](https://llvm.org/docs/FaultMaps.html) for this. Some references:

- I was told that [rustgc](https://github.com/Manishearth/rust-gc) triggers GC by page faulting.
- A [blogpost](https://medium.com/@MartinCracauer/generational-garbage-collection-write-barriers-write-protection-and-userfaultfd-2-8b0e796b8f7f) about using guard pages for write barriers.
- A [post](http://jcdav.is/2015/10/06/SIGSEGV-as-control-flow/) about Hotspot Java's usage
  of page fault handling for null pointer checks.

Let's look at a concrete zero-cost-GC setup.

The heap pointer is threaded through every heap-allocating function in some
register, like a state in a state monad. Alternatively, OCaml and native GHC
both just pin the heap pointer to a register and keep it there. Pinning is
simpler but less efficient, because sometimes we do a lot of work without
doing heap allocation, and then we want to free up the heap register for other
work.

Assume that we want to allocate N words.

- If N is statically known to be smaller than the guard page size, we write the
  **last word** of the N words first, and that write serves as the single
  statepoint for the whole allocation. Here, GC can ensure that we get at least
  N words worth of extra heap, so every other write in the N batch is ensured to
  not page fault. In contrast, writing words **in order** would be the absolute
  worst strategy here, because we may get the page fault at any of the write
  instructions, so we'd need a statepoint for each of them.
- If N is statically known to be larger than the guard page size, or it's only
  dynamically known, it makes sense to just insert an explicit heap check in
  the code. These cases are very rare in comparison.

A fun side effect of this setup is that the end-of-heap pointer doesn't have to
be kept around at execution. In GHC, this is called the "heap limit", or HpLim,

GHC on x64 permanently pins four registers in total:

- The heap pointer.
- The stack pointer.
- The end-of-stack pointer.
- BaseReg points to a structure of thread-local data which includes HpLim.

What's the leanest alternative setup? It seems that we need **two** registers at
the minimum, for the heap and stack pointers.

- If we use page-faulting heap and stack checks, we don't need the end-of-heap
  and the end-of-stack.
- If we know that the stack pointer is aligned to some statically known 2^N
  bytes, we can store a pointer to the thread-local data in the beginning of
  each stack segment, and we can compute its location from the stack pointer.

### Potential issues

First, page fault handling is extremely expensive compared to plain branching.
I measured 4-5 microseconds for handling SIGSEGV on my Linux. So we have to make
sure that fault handling is sufficiently rare. For heap checks, I ran some
back-of-the-envelope calculation for one of my [cubical type theory
benchmarks](https://github.com/AndrasKovacs/cctt). There using 8MB arena sizes,
signal handling would take 0.4% of the total runtime. With increased arena sizes
this goes down, obviously. My CPU has 32MB L3 cache, and 0.1% of runtime for
signal handling with 32MB arena sizes is very reasonable.

Second, I actually have no idea how page fault handling works out in a
multi-threaded RTS - I'm not expert enough currently. I've read that SIGSEGV-s
cannot be queued. I *suppose* that GHC's stop-the-world scheme would still work,
because if any thread page faults we stop all threads and GC everything, and
it's enough to handle just one SIGSEGV out of multiple simultaneous ones.
If you're an expert reading this, please enlighten me!

Third, page faulting stack checks can be problematic if we have lightweight
threads. We want to make stack chunks big enough so that page fault handling can
be rare. But if we allocate zillions of light threads, each with a generous
reserved stack size, we can easily subscribe beyond the size of physical memory.
I've read that that's not good, but I don't know exactly how - again experts
please enlighten me!

So, it might be a viable option to have guard pages for heap allocation
but not for stack allocation.

### Small case study

Let's look at a concrete example now. We compile the following with GHC

```haskell
    {-# language Strict #-}
    {-# options_ghc -O1 -fworker-wrapper-cbv #-}

    module Foo (foo) where

    data Tree = Leaf Int | Node Tree Tree

    foo :: Tree -> Tree
    foo (Leaf x) = Leaf (x + 100)
    foo (Node l r) = Node (foo l) (foo r)
```

[`-fworker-wrapper-cbv`](https://downloads.haskell.org/ghc/latest/docs/users_guide/using-optimisation.html#f-platform-independent-flags) is a recent addition to GHC. It is cheap in terms of
compile times and plays very nicely with `Strict`. In short, it removes all laziness
overhead from statically known calls of strict functions. Here, `foo` will have
a worker function which assumes that its input is not a thunk, so its code has
no thunk check. Thunk checking is instead moved to each call site of `foo`, but
in a `Strict` program the call site arguments will be usually also known to be
non-thunks.

- Printing assembly with `-S`, we get **47** instructions in the output. We have two heap checks,
  one stack check and one constructor tag branch.
- Deleting stack checks, heap checks, GC calls and constructor tag writes, we get **25** instructions.

Here's the adjusted, cut-down code. I did not bother rearranging heap writes to
first write the last word.

```
    Foo_zdwfoo_info:
    .LcE0:                           ## check constructor tag in the pointer
        movq %r14,%rax
        andl $7,%r14d
        cmpq $1,%r14
        je .LcEa
    .LcDW:                           ## Node case
        movq $.LcEg_info,-16(%rbp)   ## Explicitly push return address for recursive call
        movq 6(%rax),%r14
        movq 14(%rax),%rax
        movq %rax,-8(%rbp)
        addq $-16,%rbp
        jmp Foo_zdwfoo_info
    .LcEa:                           ## Leaf case
        movq 7(%rax),%rax
        addq $100,%rax
        movq %rax,(%r12)
        leaq -7(%r12),%rbx
        jmp *(%rbp)                  ## Return to address stored at top of stack
    .LcEg_info:
    .LcEg:                           ## Node case, second recursive call
        movq $.LcEs_info,(%rbp)
        movq 8(%rbp),%r14
        movq %rbx,8(%rbp)
        jmp Foo_zdwfoo_info
    .LcEs_info:
    .LcEs:                           ## Node case, return
        movq 8(%rbp),%rax
        movq %rax,-8(%r12)
        movq %rbx,(%r12)
        leaq -14(%r12),%rbx
        addq $16,%rbp
        jmp *(%rbp)
```

This code could be still improved:

- GHC does not use `ret` or `call`, instead it pushes return addresses
  explicitly to the stack, then jumps to them. This allows some optimizations
  where we return to some other point than the caller. I believe that avoiding
  native returns is a **bad idea**. Native calls and returns benefit from the
  hardware return address stack. Without it, the generic branch target predictor
  can still handle the "emulated" returns, but it will use prediction buffer
  resources which could have gone to better use in actual indirect
  branches. Tricks involving non-matched calls/returns don't seem super
  important in any case. GHC could try to use native calls by default and only
  do jump-returns when it wants to use non-matched return tricks. By the way,
  using `ret` would shave off 2 more instructions here.
- A few more instructions could be shaved off by having slightly better code generation.

In summary, it's entirely feasible that this particular code would be **cut down
to half size** in a hypothetical system with zero-cost GC and modestly improved
code generation.

In more realistic and more complicated code, I eyeball the size reductions to be
still quite spectacular. In typical compiler/interpreter workloads where we
walk and build AST-s many times, the reduction is similar. In LLVM-friendly
workloads, where we have lots of tail-recursive loops and numerics, there's not
much reduction.

The tag-free `Tree` also sees a pretty spectacular size reduction compared to
GHC.

## 4. Conclusion

I think that it's a good general approach to separate GC concerns and non-GC
program concerns as much as possible. We get big size reductions in mutator
code, and the lack of relocations in IR is a nice simplification. I've seen
discussions about avoiding putting GC statepoints in hot code. Well, in the
current setup it just doesn't matter; GC statepoints have no cost in mutator code. If we use
segmented stacks, stack allocation statepoints in hot code can be still costly,
but this could be fixed by using stack copying instead of linked segments, or
just setting the stack segment size to be large enough, or just using C-style
fixed stack sizes.

I think it's a good idea to separate GC and non-GC concerns as much as possible,
in order to make them *separately* as fast as possible, *assuming that we're
not latency-constrained*. In a latency-constrained situation the mutator and the
GC will get inevitably all tangled up.

I've personally only ever programmed in non-latency-constrained ways, so that's
I'm most interested in. I also think that *not using GC* when we're
latency-constrained is a very good idea. However, sometimes we're just
forced to use GC by existing infrastructure and codebases, and then
it's clearly good if we have the option of low-latency GC.
