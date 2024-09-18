
# Lightweight region memory management in a two-stage language

A big part of my recent work is about PL theory and design, trying to get good
combinations of predictable high performance and high-level abstractions.
Staged compilation is an important part of it:

- [Staged Compilation With Two-Level Type Theory](https://andraskovacs.github.io/pdfs/2ltt.pdf)
- [Closure-Free Functional Programming in a Two-Level Type Theory](https://dl.acm.org/doi/10.1145/3674648)

There's no practically usable standalone implementation for these, although I've
been doing some prototyping and design exploration.

*Garbage collection* is a fixture in my designs. If we aim for the best
performance, GC can be crucial for some workloads, but it's more common that we
explicitly *don't* want to use a GC. Most latency-critical programs are written
in C, C++ or Rust, without GC.

Naturally, I'm interested in improving memory management performance in my
language designs, but there are some trade-offs and tensions.

First, *type safety* is non-negotiable for me, both at runtime and compile
time. By the latter I mean that well-typed code-generating programs must yield
well-typed code. So every design choice has to accommodate type safety.

Second, two-level type theory (2LTT) is extremely expressive and convenient for
two-stage compilation. But: systems that are memory-safe and GC-free tend to be
[*sub-structural*](https://en.wikipedia.org/wiki/Substructural_type_system). This
means that there are restrictions on the usage of variables; Rust is the prime
example. Sub-structural object languages haven't been researched for 2LTT-s, and
I don't think that they work very well there.

The nicest thing about 2LTT is that we never have to care about *scoping* for
the object language. At compile time, we can freely combine object expressions
without regard for their free variables, and we only need to care about typing.

If the object language is sub-structural, this doesn't work anymore. If a linear
variable is consumed in some subterm, it can't be used in another subterm. So if
we're writing metaprograms, aiming to generate substructural programs, we have
to keep track of free variable occurrences in expressions. This becomes anything
but lightweight if we want to maintain full type safety.

**Summary of my proposal**:

- We can use *regions* to shrink the GC workload, by skipping copying and
  scanning of structures that are stored in regions. The more we know about the
  allocation patterns in our program, the more GC work we can remove.
- A region is alive if any object in it is reachable, or the region itself is
  reachable. Dead regions are freed during GC.
- Regions are quite liberal: they can be stored in existentials and closures,
  they may contain pointers to the GC-d heap and other regions, and there's no
  sub-structural typing.
- We use *tag-free garbage collection*, aggressive unboxing and bit-stealing to
  improve locality and to shrink runtime objects.
- We use per-datatype GC routines that exploit static information about regions.

In expand on the design in the following.

## Basics

We start from the setup in [Closure-Free Functional
Programming](https://dl.acm.org/doi/10.1145/3674648), but I shall reiterate it
here as well.

- There's a compile-time (meta) language, which is dependently typed.
- There's an object language, which is *polarized* and simply typed.

The metalanguage is highly expressive and has fancy types, and we can write
metaprograms in it that generate object programs. The object language is easy
to compile and optimize, but it's a bit too barebones to directly program in.

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

The polarization to `ValTy` and `CompTy` is used to control *closures*. There
is a separate type former for closures, and we *only* get runtime closures if we
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

It's also not possible to have a polymorphic `map`, because there's no such
feature in the object language. Fortunately, we can abstract over anything
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

However, it's not super useful to have a `map` which takes a closure as
argument! We might as well just inline the function argument in each case,
skipping the runtime closure.

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
Note that we still have to write `=` and `:=` for definitions at the different stages.
I have found that as soon as we pin down the stages of let-definitions, almost everything else
becomes unambiguously inferable.

## Regions

Let's add regions to the mix.

- `Loc : MetaTy` is the type of memory locations.
- `Region : MetaTy` is the type of region identifiers. It's a subtype of
  `Loc`, so that we can implicitly coerce from `Region` to `Loc`.
- `Hp : Loc` represents the general GC-d heap.
- In the object language, polymorphic functions over `Region` are computation
  types.  For example `(r : Region) → List r Int → List r Int` is in `CompTy`
  (we'll see lists in regions shortly). We also have `{r : Region} → ...`.
- The object language has existential regions, written as `(r : Region) × A`,
  which is a value type.
- `region r; t` creates a new region and binds it to `r` in `t`.

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
don't need any allocation to store an empty list; at runtime it could be just a
null pointer.

Values of any type must have the same "flat" size in memory. In the case of lists,
a `Nil` would be a null pointer and a `Cons` would be a pointer to a pair, so that
checks out.

However, we can also define Rust-style unboxed sums:

```
    data Foo := Foo1 Int | Foo2 Int Int
```

Since neither `Foo1` nor `Foo2` specify a location, they are both unboxed.
At runtime, we need a tag bit to distinguish the two constructors, and we
also need to pad out `Foo1` to have the same size as `Foo2`. We also assume
that word-size is granularity of objects, so in this case `Foo` values would
have *three words*, one for the tag and two for the fields.

Recursive fields can only be placed under a pointer. The following
is rejected:

```
    data List A := Nil | Cons A (List A)
```

Unlike in Rust, we can mix unboxed and boxed constructors in a declaration (as
we saw for lists already). Take lambda terms, using De Bruijn indices:

```
    data Tm = Var Int32 | Lam@Hp Tm | App@Hp Tm Tm
```

The representation here is dramatically more efficient that what we get in
Haskell and OCaml.

- `Var Int32` is a single word, containing two bits for the constructor tag and
  32 bits for the integer.
- `Lam@Hp Tm` is a pointer tagged with 2 bits, pointing to a `Tm` on the heap.
- `App@Hp Tm Tm` is a pointer tagged with 2 bits, pointing to two `Tm`-s on the heap.
































--------------------------------------------------------------------------------
