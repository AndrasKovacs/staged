
# Lightweight region memory management in a two-stage language

A big part of my recent work is about PL theory and design, trying to get good
combinations of predictable high performance and high-level abstractions.
Staged compilation is an important part of it:

- [Staged Compilation With Two-Level Type Theory](https://andraskovacs.github.io/pdfs/2ltt.pdf)
- [Closure-Free Functional Programming in a Two-Level Type Theory](https://dl.acm.org/doi/10.1145/3674648)

I've been doing some prototyping and design exploration. There's no usable
standalone implementation but I do want to get to that point eventually.

*Garbage collection* is present in all of my designs, simply because the
starting point is functional programming with ADTs and closures, and the most
convenient setup for this is to have GC.

If we aim for the best performance, GC can be crucial for some workloads, but
it's more common that we explicitly *don't* want to use a GC. Most
latency-critical programs are written in C, C++ or Rust, without GC.

Naturally, I'm interested in improving memory management performance, but there
are some trade-offs and tensions.

First, *type safety* is non-negotiable for me, both at runtime and compile
time. By the latter I mean that well-typed code-generating programs must yield
well-typed code. So every design choice has to accommodate type safety.

Second, two-level type theory (2LTT) is extremely expressive and convenient for
two-stage compilation. But: systems that are memory-safe and GC-free tend to be
[*sub-structural*](https://en.wikipedia.org/wiki/Substructural_type_system). This
means that there are restrictions on the usage of variables. Sub-structural
object languages haven't been researched for 2LTT-s, and I don't think that they
work very well there.

The nicest thing about 2LTT is that we never have to care about *scoping* for
the object language. At compile time, we can freely combine object expressions
without regard for their free variables, and we only need to care about typing.

If the object language is sub-structural, this doesn't work anymore. If a linear
variable is consumed in some subterm, it can't be used in another subterm. So if
we're writing metaprograms, aiming to generate substructural programs, we have
to keep track of free variable occurrences in expressions. This is anything but
lightweight if we want to maintain full type safety.

**Summary of my proposal**:

- We can use *regions* to reduce GC workload, by skipping copying and scanning
  of structures that are stored in regions. The more we know about the
  allocation patterns in our program, the more GC work we can remove.
- Regions are quite liberal: they can be stored in existentials and closures,
  they may contain pointers to the GC-d heap and other regions, and there's no
  sub-structural typing.
- Objects stored in a region are alive as long as the region itself is alive.
- We use *tag-free garbage collection*, aggressive unboxing and bit-stealing to
  improve locality and to shrink runtime objects.
- We use per-datatype GC routines that exploit static information about regions.

I expand on the design in the following.

## Basics

We start from the setup in [Closure-Free Functional
Programming](https://dl.acm.org/doi/10.1145/3674648), but I shall reiterate it
here as well.

- There's a compile-time (meta) language, which is dependently typed.
- There's an object language, which is *polarized* and simply typed.

The metalanguage is highly expressive and has fancy types, and we can write
metaprograms in it that generate object programs. The object language is easy to
compile and optimize, but it's often tedious to directly program in.

Concretely, the system looks like a dependently typed language with some
universes and "staging" operations.

- `MetaTy : MetaTy` is the universe of compile-time types (or: metatypes). It
  has itself as type; this is a logically inconsistent feature that you may know
  as "type-in-type" from other languages. We have it as a simplification and
  convenience feature.
- `Ty : MetaTy` is the universe of object types.
- `ValTy : MetaTy` and `CompTy : MetaTy` are both cumulative sub-universes of `Ty`,
  e.g. if we have `A : ValTy` then we also have `A : Ty`.
- `ValTy` contains *value types*; these are primitive types, ADTs and
  closures. At runtime, a value lives in dynamic memory (stack or heap).
- `CompTy` contains *computation types*; these are functions and finite products
  of computations. At runtime, a computation is represented by an address
  pointing to a chunk of machine code in the executable.

The polarization to `ValTy` and `CompTy` is used to control *closures*. There is
a separate type former for closures, and we *only* get runtime closures if we
use it.

- Function domain types must be value types.
- ADT constructor fields must be value types.
- For `A : CompTy`, we have `Close A : ValTy`
- For `t : A` we have `close t : Close A`.
- For `t : Close A`, we have `open t : A`.

An example:
```
    data List (A : ValTy) := Nil | Cons A (List A)

    map : Close (Int -> Int) -> List Int -> List Int
    map f xs := case xs of
      Nil       -> Nil
      Cons x xs -> Cons (open f x) (mapPlus f xs)
```
The function argument to `map` has to be wrapped in a closure;
it's not possible to have `(Int -> Int) -> List Int -> List Int`
because function domains must be value types.

It's also not possible to have a polymorphic object-level `map`, because there's
polymorphism in the object language. Fortunately, we can abstract over anything
in the metalanguage. For that, we need *staging operations*:

- For `A : Ty`, we have `↑A : MetaTy`, pronounced "lift `A`", as the type of
  metaprograms which produce `A`-typed programs.
- For `t : A : Ty`, we have `<t> : ↑A`, pronounced "quote `t`", as the metaprogram
  which immediately returns `t`.
- For `t : ↑A`, we have `~A`, pronounced "splice `t`", which runs the metaprogram
  and inserts its output in an object program.
- `<~t>` is definitionally equal to `t`.
- `~<t>` is definitionally equal to `t`.
- Splicing binds stronger that function application, e.g. `f ~x` is parsed
  as `f (~x)`.
- Lifting, quoting and splicing is the only way to mix programs at different
  stages.

A polymorphic `map` is now written as
```
    map : {A B : ValTy} → (↑A → ↑B) → ↑(List A) → ↑(List B)
    map f as = <letrec go as := case as of Nil       -> Nil
                                           Cons a as -> Cons ~(f <a>) (go as);
                go ~as>
```

- The braces for `{A B : ValTy}` mark implicit arguments in the style of Agda.
  In GHC's more verbose style it would be like `forall (a :: ValTy) (b :: ValTy)`.
- The semicolon at the end of `go`'s definition is used to delimit the defined
  thing in a `letrec`. We have non-recursive `let` too, as `let x : A := t; u`.
  Non-recursive `let` can shadow previously defined names.
- A meta-level definition uses `=` while an object-level one uses `:=`.

The previous monomorphic `map` can be reproduced:

```
    monoMap : Close (Int -> Int) -> List Int -> List Int
    monoMap f xs := ~(map (λ x. <open f ~x>) <xs>)
```
At compile time, the splices are replaced with generated code, and we get the same
code that we defined before.

However, it's not the best idea to have a `map` which takes a closure as
argument. We might as well inline the function arguments to skip the runtime
closure:

```
    monoMapPlus : List Int → List Int
    monoMapPlus xs = ~(map (λ x. <~x + 10>) <xs>)
```

### Stage inference

The staging operations in the examples so far caused some noise. It's possible
to have *stage inference* as an elaboration feature, and we can skip pretty much
all quotes and splices, and also most lifts. The following works:

```
    map : {A B : ValTy} → (A → B) → List A → List B
    map f as = letrec go as := case as of Nil       -> Nil
                                          Cons a as -> Cons (f a) (go as);
               go as
```
Note that we still have to write `=` and `:=` for definitions at the different
stages. I have found that as soon as we pin down the stages of let-definitions,
almost everything else becomes unambiguously inferable.

## Regions

Let's add regions to the mix.

- `Loc : MetaTy` is the type of memory locations.
- `Region : MetaTy` is the type of region identifiers. It's a subtype of `Loc`,
  so that we can implicitly coerce from `Region` to `Loc`.
- `Hp : Loc` represents the general GC-d heap.
- In the object language, polymorphic functions over `Region` are computation
  types.  For example `(r : Region) → List r Int → List r Int` is in `CompTy`
  (we'll see lists in regions shortly). We also have `{r : Region} → ...`.
- The object language has existential regions, written as `(r : Region) × A`,
  which is a value type.
- `let r : Region; t` creates a new region and binds it to `r` in `t`.

We also need a way to put things in regions. We extend ADT declarations with
location specification. Example:

```
    data List (l : Loc)(A : ValTy) :=
        Nil
      | Cons@l A (List l A)
```

`Cons@l` specifies that a cons cell is a pointer to a pair of `A` and `List l A`,
allocated in `l`.

`Nil` has no location specification, which means that it's *unboxed*, i.e. we
don't need any indirection to store an empty list; at runtime it could be just a
null integer.

For any type, all values of the type must have the same "flat" size in memory.
In the case of lists, a `Nil` would be a null value and a `Cons` would be a
pointer to a pair, so that checks out.

We can also define Rust-style unboxed sums:

```
    data Foo := Foo1 Int | Foo2 Int Int
```

Since neither `Foo1` nor `Foo2` specify a location, they are both unboxed.  At
runtime, we need a tag bit to distinguish the two constructors, and we also need
to pad out `Foo1` to have the same size as `Foo2`. So, `Foo` requires 1+64+64
bits of storage. Depending on the contexts in which we use `Foo`, this data can be
stored differently; see "bit-stealing" a bit later on. We assume word
granularity for runtime objects, so a `Foo` value takes up three words at most.

Recursive fields can only be placed under a pointer. The following is rejected
by the compiler:

```
    data List A := Nil | Cons A (List A)
```

Unlike in Rust, we can mix unboxed and boxed constructors in a declaration (as
we saw for lists already). Take lambda terms, using De Bruijn indices:

```
    data Tm = Var Int32 | Lam@Hp Tm | App@Hp Tm Tm
```

This representation is far more efficient than what we get in Haskell
and OCaml.

- `Var Int32` is a single word, containing two bits for the constructor tag and
  32 bits for the integer.
- `Lam@Hp Tm` is a pointer tagged with two bits, pointing to a `Tm` on the heap.
- `App@Hp Tm Tm` is a pointer tagged with two bits, pointing to two `Tm`-s on the heap.

There's also one bit reserved for GC in every pointer; I'll write more about
GC implementation later.

Consider `App (Var 0) (Var 1)`. In Haskell and OCaml, `App` has three words, one
header and two pointer for the fields, and `Var 0` and `Var 1` each contain two
words, one header and one unboxed field. That's a total of **seven** words on
the heap.

In 2LTT, it's **two** words on the heap instead! The `App` pointer itself has
the constructor tag, it points to a tag-free pair of two words, and the two
words are unboxed `Var`-s.

### Bit-stealing

It would be already quite efficient to *uniformly* represent ADT-s, but we can
do more layout compression. Let's take `List (Maybe Int)`:

```
    data List A := Nil | Cons@Hp A (List A)

    data Maybe A := Nothing | Just A
```
`List` is on the heap while `Maybe` is an unboxed sum. Using uniform representation,
we would have that `Maybe Int` takes two words (tag + payload), so a `Cons` cell
would be three words on the heap.

Using bit-stealing, we can move immutable data from constructors into free space
in pointers. In this case, `Cons` uses 0 bits for tagging (since `Nil` can be
represeted as a null value) and reserves 1 bit for GC.

The free tag space varies depending on the architecture and the exact tagging
scheme. At least, we have 2 lower bits available because of pointer alignment
(recall that reserve 1 bit for GC). At best, we have additional 16 high bits
available.

If plenty high bits are available, we might choose to only exploit high bits and
leave low bits alone; this can make tag operations use fewer machine
instructions.

In any case, we can certainly use bit-stealing for `List (Maybe Int)`. Moving
the `Maybe` tag into `Cons`, we get **two words** on the heap for a `Cons`.

### Using regions

The most important property about regions is the following:

**Objects stored in a region are alive as long as the region itself is alive.**

First I give some code examples, then explain how the region property is used to optimize GC.

Location-polymorphic mapping looks like this, fully explicitly:

```
    data List (l : Loc) (A : ValTy) := Nil | Cons@l A (List l A)

    map : {A B : ValTy}{l l' : Loc} → (↑A → ↑B) → ↑(List l A) → ↑(List l' B)
    map {A}{B}{l}{l'} f as =
       <letrec go as := case as of
                 Nil       → Nil
                 Cons a as → Cons@l' ~(f <a>) (go as);
        go ~as>
```

We can rely on a lot of inference though. We've seen stage inference before, and we can also
make location annotations on constructors implicit when they are clear from the
expected type of an expression.

```
    data List l A := Nil | Cons@l A (List l A)

    map : {A B : ValTy}{l l'} → (A → B) → List l A → List l' B
    map f as =
       letrec go as := case as of
                 Nil       → Nil
                 Cons a as → Cons (f a) (go as);
       go as
```

Concrete instantiations of `map` are object functions where the function
argument is inlined, working on lists stored at given locations.

We can recover a region-polymorphic object function as follows:

```
    myMap : {r r' : Region} → List r Int → List r' Int
    myMap {r}{r'} xs := ~(map {Int}{Int}{r}{r'} (λ x. <~x + 10>) <xs>)
```
With inference:

```
    myMap : {r r'} → List r Int → List r' Int
    myMap xs := map (λ x. x + 10) xs
```
We get an inner recursive `go` function which refers to the `r` and `r'`
parameters of the outer function. Hence, when we're in the middle of mapping,
`r` and `r'` are both reachable on the stack (or in registers). Hence, no objects
stored in `r` and `r'` can be freed by GC.

This enables a remarkable amount of GC optimization. Object types contain
information about locations, and for each type we can implement a GC strategy
which takes locations into account.

Consider `List r Int` for `r : Region`. Whenever a value of this type is
reachable, the region `r` must be reachable as well (it must be in scope, or
otherwise the type is not even well-formed). Therefore, *doing nothing* is a
valid GC strategy for this type! When `r` becomes dead, the whole region gets
freed, and with it all the `List r Int` values are freed too. When GC
processes a `Region`, it does not look at its contents, it simply marks the
region itself as alive.

Consider `List r (List Hp Int)`. When a value of this type is reachable, we know
that `r` must be also reachable, but we don't know which heap-based inner lists are
stored. Hence, GC scans the outer list, in order to reach and relocate the inner lists. But GC does not relocate the region-based cons
cells. `Hp` may use copying GC, but regions are not copied and region pointers
are stable.

Consider `List Hp (List r Int)`. GC traverses and relocates the outer cons
cells, but it doesn't look into the inner lists.

### Strict regions

It's not too rare in practice (at least in compilers and elaborators that I've
implemented) that we have some long-lived tree structure which may contain
references to the general heap. Toy example:

```
    data HpVal := HpVal@Hp Int Int
    data Tree (r : Region) := Leaf@r HpVal | Node@r Tree Tree
```

I often use the pattern where I embed the *lazy value* of a top-level definition
into core terms, right next to a top-level variable, so that I can eliminate the
cost of top-level lookups during evaluation, and also avoid passing a top-level
store around. Core terms are persistent but lazy values are quite dynamic.

It's a bit awkward if GC has to deeply traverse trees because contain heap
references. A common pattern is to replace heap pointers with integer indices
pointing to an *array* of heap values.

```
    type Index = Int
    data Tree (r : Region) := Leaf@r Index | Node@r Tree Tree
```

This is actually the more common style in PL implementations. With this, GC
doesn't scan trees anymore, but the programmer has the extra job of correctly
managing the arrays and the indices.

We extend the system with **strict regions**:

- There's `StrictRegion : ValTy → MetaTy`, and a value of `StrictRegion A` can
  be implicitly cast to `Loc`.
- We can only allocate values of type `A` in an `r : StrictRegion A`.
- When GC touches a strict region, *it scans all of its contents*.
- `let r : StrictRegion A; t` creates a new strict region.

The reason for only having values of a single type in a strict region: tag-free
GC has to know about the type (hence layout) of the region in order to traverse
it.

The typical usage of strict regions is to put *pointers to heap values* inside
them:

```
    data Box l A := Pack@l A

	data Tree (r : Region)(r' : StrictRegion (BoxHpVal) := Leaf@l (Box l' Val

    main : ()
	main =
	  let r : StrictRegion
```



### Existential regions and closures





























--------------------------------------------------------------------------------
