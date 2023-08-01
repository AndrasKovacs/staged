
## High-level high-performance programming

This document is a loose collection of my thoughts on the titular topic. It
describes features and principles in hypothetical programming language
implementations, and also looks at costs of implementing them in the current
IR/backend ecosystem.
better
It is skewed towards mostly-functional programming with garbage collected
runtimes, focusing on application in compilers, interpreters, proof
assistants. Here I have strong opinions. I have less experience and expertise in
high-performance concurrency, SIMD and low-level memory management systems. A
fair number of my techniques may still apply here but I don't have strong
opinions.

The target demographic of my hypothetical language cares about performance
somewhat more than the median Haskell or OCaml programmer, and appreciates
explicit control over memory layouts and code generation. The surface language
may require more decisions from users than Haskell/OCaml but probably fewer than
Rust.

Simplicity of implementation and compilation speed is strongly taken into
account in the bang-for-buck calculation here. If we can demand a small amount
of extra effort from the programmer to outperform huge state-of-the-art
optimizers, we take that deal. We look for easy optimizations and simplifying
assumptions before we consider anything smart or complicated.

## Prioritize staging over "sufficiently smart" compilers

Compilers nowadays are, in a sense, sufficiently smart. For a quick example,
half of the speed of TinyCC comes from being compiled with an optimizing C
compiler (which is not TinyCC). This is a nice illustration that for a
non-trivial use-case in compilers, compiler optimization can double performance
with no programmer input whatsoever.

But obviously Clang and GCC are lumbering behemoths compared to TinyCC, and if
we take into account the stupendous code size difference, the 2x improvement is
not so spectacular.

The point of staging is to hand over some control over code generation to
programmers. One big benefit is to allow programmers to extend the compiler with
domain-specific optimizations, because compilers can't feasibly know about every
application domain under the sky. A more mundane application is simply to give
finer and robust control over inlining and memory layouts, in programming in
general.

- Compiler optimizations are not part of a language specification and they
  are not precisely controllable from user-facing syntax.
- Staging features should have semantics in a language specification. They
  should be tractable and exactly controllable from surface syntax.

Polymorphism with monomorphization is probably the most successful staging
feature in the wild. It has decent implementations in existing languages,
e.g. in Rust. It's pretty lightweight as far as staging features go. If we want
more substantial metaprogramming, the picture is less rosy.

- In C++ it's awful.
- In Rust it's done with traits or macros, both are far from ideal.
- Template Haskell is awful, both in typed and untyped flavor.
- Macros in lisp tradition are untyped.
- Zig metaprogramming also lacks well-typing guarantees.
- OCaml staging implementations have similar issues as typed Template Haskell.

Enter two-level type theory, abbreviated 2LTT.

- It allows a wide range of object and metalanguages, but generally speaking
  the metalanguage should be a proper type theory if we want the best package,
  namely good expressiveness plus guaranteed well-formedness of code output.
- It supports far better ergonomics than TH or macros for the purpose of
  non-trivial metaprogramming; little syntactic noise and boilerplate is
  required. We only need to sprinkle a handful of annotations in types and
  binders and then stage inference takes care of the rest pretty easily.
  Monomorphized generics can be simply a subset of 2LTT features, and we get at
  least Rust-level ergonomics without special effort.
- It has established metatheory with a proof of correctness. The proof is about
  a specific 2LTT with specific features but it's clear that it works generally.
- It can be implemented very efficiently, potentially with machine-code compiled
  metaprograms. The dumbest interpreted staging implementation is already
  very fast.

A quick summary of 2LTT:

- There is a type of static/comptime types. Let's call it Type1.
- There is a type of runtime types. Let's call it Type0.
- For `A : Type0`, we have `^A : Type1`. Meaning: `^A` is the
  type of metaprograms which generate expressions with type `A`.
- For `t : ^A`, we have `~t : A`. Meaning: `~t` runs a metaprogram
  and inserts its code output. It's pronounced "splice".
- For `A : Type0` and `t : A`, we have `<t> : ^A`. Meaning: `<t>` is
  the metaprogram which immediately returns `t` as an expression.
  Pronounced "quote".
- There must be no other way than the above operations to cross between `Type0`
  and `Type1`.
- We can have pretty much any language feature in `Type0`, and have a wide range
  of features in `Type1` as long as they stay withing the realm of well-behaved
  type theory.
- We have `<~t> ≡ t` and `~<t> ≡ t` as definitional equalities in type checking.

`~` binds stronger than function application, so `f ~x` means `f (~x)`.

Quotation and splicing may be familiar from Template Haskell, MetaOCaml or
lisps. In 2LTT, we can write explicit quotes and splices, but they are almost
always inferable without ambiguity, because stages appear in types (via the
distinction of `Type0` and `Type1`). First, let's look at some fully explicit
examples. You can also look at the tutorial file in my prototype 2LTT (TODO).

Let's assume that the runtime and static languages both support basic dependent
type theory. I use syntax and inference that's close to what I've already
implemented in my prototype.

We have the runtime identity function. No surprises here.

    id0 : {A : Type0} → A → A
      := λ x. x

The static version:

    id1 : {A : Type1} → A → A
      = λ x. x

Note that I use `:=` in `id0` but `=` in `id1`. I have found that let-bindings
are the main point of ambiguity in stage inference. `:=` forces a definition
to be runtime, `=` forces it to be compile time. We'll see a bit later how
this influences elaboration.

`id1` can be used on any type, including runtime types, in which case
it is always "inline". Assume `Bool0 : Type0`.

    foo : Bool0 := ~(id1 {^Bool0} <true0>)

With full inference:

    foo := id1 true0

Here `:=` tells us that the definition should be runtime, which makes the
elaboration to the explicit version unambiguous.

I can do this as well:

    foo = id1 true0

Output:

    foo : ^Bool0 = id1 {^Bool0} <true0>

Now this is an inline constant which inserts `true0` whenever used in runtime
code.

The inline map function:

    map : {A B : ^Type0} → (A → B) → List0 A → List0 B
       = λ f xs. letrec go nil         := nil
                        go (cons a as) := cons (f a) (go as);
                 go xs

Elaboration:

    map : {A B : ^Type0} → (^~A → ^~B) → ^(List0 ~A) → ^(List0 ~B)
       = λ f xs. <letrec go nil         := nil
                         go (cons a as) := cons ~(f <a>) (go as);
                 go ~xs>

Here, because we write `^Type0` in the type, elaboration knows that all other
function input types must be static (because all type formers are uniform in
stages), so it inserts `^`-s accordingly. We have an ambiguity here, because `^`
would make sense on the outside of function types as well:

    map : {A B : ^Type0} → (^~A → ^~B) → ^(List0 ~A → List0 ~A)

It is IMO a good default behavior to push `^` down in inference. `^(A → B)` is
rarely the preferred choice to `^A → ^B`. The two are isomorphic, but the latter
computes statically. E.g. the following `id` version is pretty useless:

    id : {A : ^Type0} → ^(~A → ~A)
	  = λ A. <λ x. x>

This inlines the identity lambda expression when used in object code, so `id
true0` is elaborated to `~(id {<Bool0>}) true0` which is staged to `(λ x. x)
true0`.
