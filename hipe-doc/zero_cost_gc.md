
NOTE: Make it shorter, only write about zero-cost gc



<!-- TODO: -->

<!-- Rerun benchmarks: -->
<!-- - https://xzpeter.org/userfaultfd-wp-latency-measurements/ -->

<!-- Write simple church tree bench in Haskell -->
<!-- - Compare GC costs with signal handling costs -->
<!-- - Estimate code size reduction from signals, via cmm. -->


<!-- ~4.2 usec sigsegv handling -->

<!-- 5.475 sec runtime -->
<!-- 11,791,023,528 bytes heap alloc -->
<!-- 8MB arena size -->
<!-- 5502 collections -->

<!-- 5502 * 4.2 usec = ~23.1 ms signal handling -->

<!-- 0.4 % of runtime is in signal handling -->

# Garbage collection with zero cost at non-GC time

Every 1-2 years I feel the urge to write a low-level backend for some of my
prototype languages (which are usually just plainly AST-interpreted). At such
times, I usually go over LLVM, C and STG to see the possibilities and
trade-offs. So far I haven't written any working backend, because each time I'm
too bothered by the limitations and workarounds, and/or I have to go and work on
something else.

In particular, precise relocating GC is always a painful point. Here, targeting
LLVM or C we immediately face annoying limitations. I have the impression that the
enduring popularity of reference counting is partially due to the relative ease
of supporting it in LLVM and C backends, compared to precise GC.

## Targeting C

Here, the only reasonably precise GC solution is to manage some kind of stack
manually. There are several variations on this.

### Using a "shadow stack" of GC roots alongside the native stack

We embed structs containing GC pointers into the native stack frames, and
arrange these frames into a linked list. An easy way to implement this is to
pass an extra argument to every function, namely the pointer to the last shadow
stack frame. We link a new stack-allocated shadow frame to the shadow stack
whenever we want to allocate GC roots. We store the live GC roots on the shadow
stack before every function call or heap allocation.

A bonus here, compared to other solutions that we'll see, is that the compiler
knows how to optimize the native stack and the native function calls and
returns. Also, the shadow stack only needs to contain GC roots but no unboxed
data.

This advantage is also the biggest disadvantage. We don't have full control over
the layout and allocation of the stack, and the stack manipulation primitives
that we get from the backend may not be adequate.

If we want to use segmented stacks, that's supported by Clang and GCC. We can hack
some form of exceptions using longjmp, but that may not be precisely what we
want. Stack copying could be problematic or just impossible if we allow pointers
to data on the stack. Continuation primitives could be likewise infeasible.

This is IMO the reason that I don't see major functional language backends using
this kind of shadow stacks, and they instead go for fully managed stacks (MLton,
GHC, Erlang).

A slight variation on this setup is to store shadow stack frames contiguously
and separately from the native stack. This is fundamentally quite similar, as we
are still using native stack frames and managed shadow frames at the same time.

There are some other disadvatages that also appear in LLVM. First, the compiler
thinks that every heap-allocating function invalidates the whole memory. C
compilers already assume that functions external to the compilation unit can
write any memory, but here we get this even in a single unit. This has an
obvious negative impact on code generation. And there's no way to tell the
compiler exactly which things can be possibly relocated. In LLVM, there's also
no way to be fine-grained enough about what memory functions can write.

Second, we (as the code generator) have to set up tail calls to make it
*possible* for the compiler to actually emit tail calls, and even then we
might not be able to guarantee all the tails calls that are possible in
e.g. Scheme or Haskell. Basically, if we have a call that looks like a tail
call, we need to pop off the appropriate shadow stack frames when making the
call.

### Ditching the native stack and only using a managed stack

We also push return addresses manually to the managed stack. The native stack is
relegated to being a scratchpad of limited size. Now we have full control over
stack layout and allocation. We can implement any kind of stack primitives.

However, by not using native return instructions, we partially cripple the
backend optimizer, and we don't take advantage of return address prediction in
the hardware.

In C backends, the biggest problem is the following: *C compilers don't support
enough tail calls!* Not even `musttail` in Clang works, because it only allows
tail calls when the caller and the callee functions receive arguments with same
memory layout. The problem is C's caller-cleanup system that's required by
varargs. It's just not compatible with general tail calls.

So to make this work in C, we must use trampolining for function calls. That's
awful.

Interestingly though, MLton has a trampolining C backend in this style, and it
is supposed to be pretty fast. How does that work? I've not dug very deep into
it, but my understanding is that since MLton does whole program
defunctionalization with whole program control flow analysis, it can a) set up
trampolines with quite precise sets of closures, which can be also represented
as unboxed sum types b) it can coalesce code into large blocks which contain
known branching on defunctionalized closures instead of unknown function calls.

In short, MLton minimizes function call overhead by getting rid of function
calls through whole program analysis. If I'm correct the old C backend for GHC
also uses trampolining, but it's very slow and crappy.

Another nasty feature of this setup is that *all* live data must be spilled to
the stack on *every* function call. The reason: the only way codegen can
communicate live data across calls is to explicitly put them on the stack. And
as soon as we put them on the stack, it becomes impossible for the backend to
move them anywhere else because of the assumption that GC invalidates the entire
memory.

Let's illustrate this a bit more. First, using native returns, we can write
`<prog1>; x = f(y, z); <prog2>` where `prog2` can refer to all variables defined
in `prog1`, and the compiler decides how to store them across the function
call. Using fully managed stacks, we instead have:

```
    void code1() {
      <prog1>
      <push live values>
      push code2
      tail call f(y, z);
	}

    void code2(x){
      <prog2>
	}
```
I assume an implicit mutable stack above. When `f` returns, it does so by
calling the `code2` continuation on the top of the stack with `x`. So
the only way `code2` can access `<live vars>` is to load them from the stack.

Now, can a sufficiently smart compiler figure out that `f` can actually only
return to `code2`, and optimize as if we had a native function call? I would
guess *yes*, and I wouldn't be surprised if LLVM or GCC could do this in some
cases. I don't expect though that this can be done *robustly*. But this is moot,
since if `f` can do heap allocation, LLVM thinks that it can write to memory
behind the stack pointer, so LLVM will never store the pushed live values in
registers, even if those values are unboxed.

Indeed, looking at the LLVM backends for Erlang and GHC, they both use fully
managed stacks and custom calling conventions which disable callee-save
registers altogether. If there are no preserved registers, the way to make the
best out of the situation is to just disable calee-saving and pass function args
in registers a lot more aggressively.

## Targeting LLVM

The previous two strategies work in LLVM similarly as in C, but fully managed
stacks work much better, because we can choose a calling convention with proper
tail calls, and we don't need trampolines.

In my current opinion, picking the GHC calling convention in LLVM is overall a
pretty good choice. The actual GHC LLVM backend has a mixed reputation due to
long compile times. Checking out the IR output, it seems that GHC does produce a
large amount of bloat. For example GHC compiles `foo (x :: Int#) = x` to 1118
bytes of textual IR. It find it likely that much of the bloat could be cut out
with more careful and optimized codegen. Part of the bloat is that every
function formally takes all STG registers as arguments, even if they are unused.
I also see some random-looking bloat, like loading return addresses from the
stack in 4 instructions instead of 2 (which I think would achieve the same).

When we do use the native stack, LLVM also looks a bit better than C. We can
specify at least some read/write properties of functions. Concretely, a pure
function which may call GC can be specified to only write "argmem" (TODO), that
is, memory reachable from pointer arguments. I *think* that if we pass the
shadow stack as a pointer, this would properly cover relocations. Unfortunately,
there is nothing more fine-grained than this; if a function has other pointer
arguments, unboxed data behind that would be also considered
invalidated. Moreover, if we have something like a type-level distinction of
immutable and mutable data, that can't be captured in LLVM attributes. Effectful
"IO" functions in LLVM must be always specified to write all memory.

### Built-in GC

There's substantial built-in GC support in LLVM, but I'm rather skeptical of it,
and I don't think it's overall worth to use it. It's extra complication and
another point of dependency for something that might not be much faster than
hand-rolled GC as I described before. Or we must have fully managed stacks, in
which case built-in GC features cannot be used anyway. Also,

- LLVM GC features seem to be barely used in practice. GHC, Erlang and MLton
  backends don't use any of these features (because of managed stacks).
- Documention seems to have not changed over the last 5 years, since I first
  looked at it, nor have the features. Statepoints are still "experimental"
  and the exact same notes on limitations and future plans are still there.
  It looks like GC features are simply not being developed.

Moreover, the basic abstraction of relocation in LLVM is just not the proper
thing. LLVM models relocation as invalidation. Every function takes all live
roots as extra arguments, and returns their new values as extra function
outputs. This not only bloats the IR with relocations, but also prevents GC
roots in callee-preserved registers, which should be *perfectly possible and
desirable*.

This brings us to the topic of zero-cost GC.

# Zero-cost GC

By "zero-cost" I mean that GC has no impact on runtime mutator performance, and
also places no restriction on compile-time optimization of mutator programs.
This analogous to "zero-cost exceptions".

- Mutator programs see an infinite heap into which we can copy data.
  There are no heap overflow checks in machine code.
- Relocation is not explicitly represented in IR, and it is not even observable
  by optimization or register allocation. GC roots are not special at all, and
  they are not spilled to memory any more than anything else.
- Runtime objects store no data which describes their layout. This is called
  "tag-free GC" in literature. Data structures have no storage overhead over the
  same structures in non-GC-d settings.

However:

- Executables may contain extra data ("side tables"). This data should be stored
  separately from mutator code as a matter of principle, but sometimes it's
  worth to make concessions (classic example: the "tables next to code" layout
  in GHC).
- GC itself is, of course, not zero cost. We might even make GC somewhat more
  expensive in order to minimize costs in the mutator.

## Unobservable relocation

Instead of LLVM's relocation abstraction, we use the following principle:

**Relocation should be unobservable.**

Relocation should update GC roots everywhere, including in registers. If truly
all GC roots are updated and all runtime or IR operations are stable under
relocation, relocation is not even observable by an IR optimizer, and there's no
reason to explicitly represent it.

As a result, the compiler can reason as if GC calls *accessed no memory*, and
there's no impact on register allocation and code generation quality.

We can put a root in a preserved register, call a function, get a relocation,
return from the function, and the register will contain the relocated root.
No reason to spill anything just because of relocation!

Relocating registers requires some deep compiler support and plenty of
platform-specific grease, but I don't see any fundamental issue.

- Stack walking needs two extra input tables.
- One contains the values of all registers on GC entry. When we learn about
  roots in registers during stack walking, we modify them in the table.  In the
  end, we load the table back into the actual registers.
- The other one contains for each register whether it has been callee-saved
  to the stack, and if so, its stack address.
- We walk the stack, looking up side table data at each statepoint.
- At each statepoint, if there are roots on the stack, we just relocate them.
- If there are roots in registers, if registers are


<!-- At every GC call, we need the locations of roots. The compiler can produce this -->
<!-- information and put it in a "side table". A GC call first saves all registers to -->
<!-- memory, then during stack walking it learns about roots stored in registers, -->
<!-- then it reloads the relocated registers before returning. -->
