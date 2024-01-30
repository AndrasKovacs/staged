

# Two-stage functional programming from the ground up

This document describes a programming language and develops a programming toolbox inside it. The
language itself is not yet implemented, but a significant part of it can be emulated in other
existing systems, at the cost of some extra noise and inconvenience. I adapted much of the code
described here to Typed Template Haskell and provide it as a supplement.

The main idea is shift as much as possible work from general-purpose compiler optimization to
metaprogramming.

- Metaprograms are deterministic, transparent and can be run efficiently. They require some
  effort from programmers though.
- General-purpose optimizers are less transparent, less robust and slower. They don't require
  much user input.

Historically, metaprogramming has suffered from tedium, weak expressive power and weak guarantees.
Using more recent theory and implementation techniques, metaprogramming in the form of staging can
be a radically smoother experience, where staged programs barely contain any visual noise compared
to non-staged ones.

A preview of things we develop:

- We'll develop fusion for sequence types, but unlike in Haskell, fusion is formally guaranteed to
  succeed, and it is also guaranteed to not produce any dynamic closures.
- We'll have a monad transformer library, but unlike in Haskell, the vast majority of optimization
  of monadic code happens by evaluation of metaprograms, and the most common monads (State,
  Reader, Except) are guaranteed to not introduce any new dynamic closures.

The language is based on two-level type theory (2LTT), so to keep things short I'll refer to it
simply as 2LTT in this document. Note though that many different 2LTT flavors are possible.

This 2LTT is strongly focused on compiling to robustly high-performance code.  Hence, programmers
have to manage more details than in Haskell or OCaml. However, it's still a "high-level"
garbage-collected language, and we should have less overall bureaucracy than in Rust, for example.

In this document I describe the 2LTT, focusing on examples, and usage. I'll also describe how
certain features are compiled. I'll also gradually build up a "standard" library. Then I'll shift
focus to building abstractions and libraries inside the language.

Some of the choices of syntax and its sugar are fairly arbitrary and up to personal taste, and are
not set in stone.

## 1. Introduction to 2LTT

I give an overview first to 2LTT. The key feature is the separation to two levels, the meta level
(or: compile time, static) and the object level (or: runtime, dynamic). The purpose of metaprograms
is to generate code, and *staging* means running the metaprograms and getting object code as result.

We'll look at the meta level first, then the object level, and finally the interaction between the
two.

### 1.1. Meta level

This is fairly standard dependent type theory. The exact feature set and syntax can be up to
personal taste.

- `MetaTy` is the type (universe) of meta-level types. Whenever `A : MetaTy`, `A` only exists at
  compile time. Values of metatypes also only exist at compile time.
- For simplicity, we have `MetaTy : MetaTy`.
- `MetaTy` supports Π, Σ, ⊤ and indexed inductive types, using the same syntax as in Agda.
- We use Agda syntax and Agda-style implicit arguments, with some differences.

A basic definition:

    id : {A : MetaTy} → A → A
      = λ x. x

Unlike in Agda and Haskell, we don't repeat `id` for the type declaration and the definition. We can
also use Coq-style definitions:

    id {A : MetaTy}(x : A) = x

By convention, inductive types in `MetaTy` often have `₁` as subscript. That's to disambiguate
meta-level types from object-level ones. For example:

    data ℕ₁ : MetaTy where
      zero₁ : ℕ₁
      suc₁  : ℕ₁ → ℕ₁

Meta-level inductive types can only have fields with types in `MetaTy`.

However, the subscripts may be omitted when it's obvious which `ℕ` we use.  Syntax for Π (dependent
functions) follows Agda.

There is a built-in notion of anonymous **record types**:

- `[field1 : A, field2 : B, field3 : C]` is a record type with three fields. We may have zero or
  more fields. Types can depends on fields to the left.
- Meta-level record types can only have field types in `MetaTy`.
- There is a field projection notation. If `t : [field1 : A, field2 : B field1]`, then `t.field1 :
  A` and `t.field2 : B (t.field1)`.
- Field projections bind stronger than function application, so `B t.field1` is shorthand for `B
  (t.field1)`.
- Values with records types are `(t, u, v)`, or `(field = t, field2 = u, field3 = v)`.  The unit
  type is defined as `[]` with `()` as value.
- Field names can be omitted, like in `[A, B, C]`. An unnamed field can be projected by number, so
  `t : [A, B]` implies `t.1 : A` and `t.2 : B`.

### 1.2. Object level

A 2LTT program is compiled by first staging it, which means running all metaprograms, which
generates object code, and then further processing the object code. Staging is the part of the
pipeline where programmers still have full control over compilation. This is in contrast to e.g. GHC
where programmers lose precise control immediately after their code is type checked, and then they
have to nag and cajole GHC with INLINE pragmas, compiler options and bang patterns, to generate the
desired code.

The object language is very simple compared to the metalanguage. It is easy to compile and optimize,
but tedious to write code in. Fortunately, the metalanguage is very expressive and hugely improves
the usability of the overall system, and since metaprograms only exists at compile time, we don't
have to deal with their fancy features in downstream compilation.

Let's get to the details of the object-level.

- `Ty : MetaTy` is the type of object types. Since `Ty` is in `MetaTy`, the object language can't
  abstract over object types. It's *simply typed*, there's no polymorphism and no type dependency.
- `ValTy : MetaTy` is the type of *value types* in the object language. Value types, for now, are
  just primitive machine types (like `Int64` and `Double`), record types and ADTs.
- `ValTy` is a subtype of `Ty`. So whenever `A : ValTy` then `A : Ty` as well.
- `CompTy : MetaTy` is likewise a subtype of `Ty`. It has "computation" types; at first these are
  just the function types.

Note that this system is **not** call-by-push-value, despite the above naming.

The restriction to simple types makes it much easier to generate efficient code. Most importantly,
it makes it possible to associate different memory layouts to different types, like in C, C++ or
Rust.

The **function type former**:

    A : ValTy   B : Ty
    ──────────────────
      A → B : CompTy

This implies that `→` cannot appear as function input; `→` is a *first-order* function type. As
we'll see, `→` has the extremely useful property that it can be compiled without closures, only
using *lambda-lifting*. We'll also see that closures are surprisingly rarely needed in ordinary
programming!

Field types of object-level records and ADTs must be value types. So the following is OK:

    data List (A : ValTy) : ValTy where
      Nil  : List A
      Cons : A → List A → List A

The following is not OK:

    data Foo : ValTy where
      MkFoo : (Bool → Int) → Foo

Object-level ADTs can be parameterized by anything, even meta-level types and values, but fields
must always be object-level value types. Object-level ADTs also cannot have indices, only
parameters.

We can use `unboxed data` to declare unboxed sums which are similar to enum types in Rust.

    unboxed data Maybe (A : ValTy) : ValTy where
      Nothing : Maybe A
      Just    : A → Maybe A

This will be compiled as a padded fixed-size structure with a tag field. Also like in Rust,
recursive fields of unboxed types must be guarded by an indirection. Normal `data`-declared types
are represented as variable-sized data behind an indirection.

We can use Haskell-style ADT notation as shorthand, and leave parameter types inferred. However, we
always have to mark the return universe of the type:

    unboxed data Maybe A : ValTy = Nothing | Just A
    data List A : ValTy = Nil | Cons A (List A)
    data MetaList (A : MetaTy) : MetaTy = Nil | Cons A (MetaList A)

Example for an object-level definition at the top level, involving an ADT:

    mapAdd10 : List Int → List Int
      := λ xs. letrec go := λ xs. case xs of
                 nil.       nil
                 cons x xs. cons (x + 10) (go xs);
               go xs

Things to note:

- We use `:=` instead of `=` to define things at the object level. This will be important to
  disambiguate meta-level and object-level things; or at least, in my existing 2LTT implementations
  it has been a very good design choice to have this. The reason is that definitions and `let`-s are
  the main source of *stage ambiguity*, and if they're disambiguated, we can infer a *huge amount*
  of stages and stage annotations everywhere else.
- Recursion must be always introduced with a `letrec`.
- Note that `List Int` is a concrete monotype; it would be not possible to have a polymorphic `map`
  function here.

With a bit more syntactic sugar:

    mapAdd10 (xs : List Int) : List Int
      := letrec go := λ case
           nil.       nil
           cons x xs. cons (x + 10) (go xs);
         go

We have `let` and `letrec` in general. They are delimited with `;`, instead of the `in` in Haskell.

    foo : Int :=
      let x := 10;
      let y := 20;
      let z := 30;
      x + y + z

`let` and `letrec` can be used to define values of any type (not just value types). The following
works:

    foo (x : Int) : Int :=
      let bar := λ y z. x + y + z;
      let baz := bar 10;
      bar 20

**Importantly**: we can compute things in `Ty` by `case` splitting. The following is legal:

    foo : Bool → Int → Int
      := λ b. case b of
        true.  λ x. x + 10
        false. λ x. x * 10

I'll talk more about this later. In short, the ability to eliminate to `Ty` from `ValTy` makes
metaprogramming much more convenient, but it also requires a bit more work in compiling the staging
output.

The notation for **object-level record types** is the same as on the meta level.  However, the field
assignment notation for object-level records uses `:=`, following the convention in `let`:

    foo : [field1 : Int, field2 : Int]
      := (field1 := 10, field2 := 20)

Object-level records don't support type dependency. They are compiled as *unboxed tuples*. When we
want to use boxed records instead, we can just use a wrapper ADT definition:

    data Box (A : ValTy) : ValTy = Box A

The **closure type former**:

      A : CompTy              t : A          t : Close A
    ───────────────     ─────────────────    ────────────
    Close A : ValTy     close t : Close A     open t : A

Using `Close`, we can pass computations dynamically. We define a synonym for closure-based
functions:

    _~>_ : ValTy → ValTy → ValTy
	A ~> B = Close (A → B)

Now we can write:

    map (f : Int ~> Int) : List Int → List Int
      := letrec go := λ case
            nil. nil
            cons x xs. cons (open f x) (go xs);
         go

In the following, we call `→` functions "functions", and `~>` functions "closures". Distinguishing
them is an important part of controlling code generation.


#### Compiling functions

Functions can always be compiled without closures, which is a significant
performance benefit. How can we do that, given that

    case b of true.  λ x. x + 10
              false. λ x. x * 10

is allowed? Also,

    foo (x : Int) : Int :=
      let bar := λ y z. x + y + z;
      let baz := bar 10;
      let n   := bar 10 20;
      baz n

The idea: since functions cannot escape the scope, the only possible "real" computation involving
functions is to apply them to all arguments somewhere in the scope of their binding, and get the
resulting value. This justifies treating partial applications as merely sugar for eta-expanded
definitions.

Similarly, if we have a mix of lambdas and case splits as follows

    foo : Bool → Int → Int
      := λ b. case b of
        true.  λ x. x + 10
        false. λ x. x * 10

then, since the only way we can ever compute `foo` is by calling it with all arguments, the inner
lambdas can be floated out:

    foo : Bool → Int → Int
      := λ b x. case b of
        true.  x + 10
        false. x * 10

In Haskell, using the standard function type, such transformations can degrade performance, if the
case scrutinization is expensive, and the function is called multiple times with the same first
argument. In 2LTT we need to explicitly use closure types to get such computation sharing.

After eta-expansions and `λ`-floating, local functions are either

- tail-only-called in the scope, in which case they are turned into local join points,
- or have non-tail calls in the scope, in which case they are lambda-lifted to top level.


So we compile

    foo (x : Int) : Int :=
      let bar := λ y z. x + y + z;
      let baz := bar 10;
      let n   := bar 10 20;
      baz n

to

    bar := λ x y z. x + y + z;

    foo := λ x.
      join baz := λ z. bar x 10 z;
      let n := bar x 10 20;
      baz n

Here, `baz` is only tail-called, so it's eta-expanded and kept as joint point.  `bar` is called in
non-tail position so it's lifted to the top. Of course any mildly optimizing compiler will inline
`baz` as well - I'm only illustrating lambda-lifting and call saturation here. In summary, function
calls will always be compiled to statically known saturated calls.

#### Compiling closures

The implementation of our closures is closer to Koka and Rust, and differs from OCaml and
Haskell. In OCaml and Haskell the *arity* of a closure is only known at runtime, and closure
application has to dynamically check whether we have under-application, saturated application or
over-application.

In 2LTT, if `f : A ~> B`, then `f` has exactly one argument whose memory layout is determined by the
concrete `A` type, and a call to `f` does not perform any runtime arity checking. Remember that the
object language is monomorphic, so it is naturally possible to associate different memory layouts to
different types.

This implies that multi-argument closures can be efficiently represented as `[A, B, C] ~> D` or as
`Close (A → B → C → D)`; these versions actually yield exactly the same performance and calling
convention at runtime. If we use `A ~> B ~> B`, that is meant as `A ~> (B ~> C)`, a closure
returning a closure.

CPS is significantly more efficient in 2LTT than in GHC or OCaml:
- There is close to no dynamic overhead on closure calls, compared to making static calls. GHC and
  OCaml have the arity checking overhead, and GHC additionally needs to check for thunks.
- Continuation arguments can be unboxed, and generally have varying memory representation. In GHC
  and OCaml, closure arguments can only be generic heap objects. In short, CPS prevents unboxing
  there.

#### Definitional equality

We make a bit of a theoretical detour here. Feel free to skip this part if you're more interested in
practical development.

We make a design choice that's a bit unusual, in comparison to existing 2LTT-s:

*The object language has an extremely minimal definitional equality, which only
 contains weakening. No substitution, no beta-eta rules.*

Why have this?

- The object language has nontermination, so beta conversion is undecidable.
- Runtime performance is one of the main reasons for having staging, but runtime cost is not stable
  under beta-eta.
- Runtime cost is also not stable under *substitution*.
- Nor is code size, which is another important aspect of code generation that we want to control.
- If the object language is effectful (which may be desirable, e.g. if only for primitive
  exceptions), substitution is not valid in general, i.e. it does not preserve observational
  equivalence.
- Code size and runtime cost are both stable under weakening. Fortunately, staging works just fine
  with weakening only in the object theory.

Can we have something else than weakening? The only other choice that comes to mind is injective
renamings (injective maps from variables to variables). But there's no particular benefit to
injective renamings over weakenings. The staging algorithm itself only depends on weakening.

As a result of having such a weak equational theory, a bunch of propositional identities which are
provable in the 2LTT-s of prior literature, will be unprovable here. We won't be bothered by this,
because we will barely use any propositional identity in the rest of this document.

However, I will say things like "equivalent types", and often those types will not be internally
provably equivalent. For example, the following is not an internally provable isomorphism:

    (↑A → ↑B) ≃ ↑(A → B)

That's because of the lack of object-level beta-eta. We can choose a sensible semantics though in
which the isomorphism does hold. The semantics:

- We take the standard staging model (the presheaf model over the category of object-level
  weakenings).
- Additionally, we quotient object code by observational equivalence in the model.

This does not account for runtime cost, but we usually only care about observational behavior for
program correctness. In the rest of the document I will assume this semantics when talking
about equivalences.


### 1.3. Staging operations

Let's talk about staging now. So far, object-level and meta-level features were separate, without
much interaction to speak of. Staging operations make it possible to move between stages.

1. If `A : Ty`, then `↑A : MetaTy`. This is pronounced "lift `A`". `↑A` is the type of metaprograms
   which produce `A`-typed object-level expressions.
2. If `t : A` and `A : Ty`, then `<t> : ↑A`, pronounced "quote `t`". This is the metaprogram which
   immediate returns `t` as an expression.
3. If `t : ↑A`, then `~t : A`, pronounced "splice `t`". Splicing runs a metaprogram and inserts its
   resulting expression at some point.

Additionally, we have `<~t> ≡ t` and `~<t> ≡ t` as definitional equalities; this is often needed for
type checking dependently typed metaprograms.

Splicing binds stronger than function application, so `f ~x ~y` means `f (~x) (~y)` , but it's
weaker than field projections, so `~f.x` means `~(f.x)`.

**Example**. Consider the meta identity function:

    id : (A : MetaTy) → A → A
      = λ A x. x

Thanks to staging operations, this can be used in object code too:

    n : Int := ~(id (↑Int) <10>)

Here, `id (↑Int) <10> : ↑Int` is a metaprogram which computes to `<10>` during staging. Then, we get
`~<10>` in object code, which computes to simply `10`.

Now, it would be annoying if we had to write all of these quotes and splices all the
time. Fortunately, almost all of quotes and splices are unambiguously inferable, using bidirectional
elaboration and the disambiguated stages of definitions (`=` vs `:=`). Something like the following
works fine:

    id : {A : MetaTy} → A → A
      = λ x. x

    n := id 10

Elaborating `n`:

1. We try to infer an object type for `id 10`.
2. We look up the type of `id`.
3. Since `id` has implicit function type, we insert an implicit application, so we have `id {?0} 10`
   at this point, where `?0` is a metavariable.
4. We infer type for `10`, which is just `Int : ValTy`.
5. We try to cast `10 : Int : ValTy` to `?0 : MetaTy`. The stages don't match, so we lift the left
   side to `<10> : ↑Int : MetaTy`, and unify `↑Int` with `?0`.
6. Now we have `id {↑Int} <10> : ↑Int` as output.
7. Since we had `n :=`, an object expression is expected, so we cast the result down to object-level
   by inserting a splice.
8. `n := ~(id {↑Int} <10>)` is the final result.

I have implemented fairly strong bidirectional stage inference in my 2LTT prototype, so the
inference promises that I'm making are substantiated.

**Example**. Coercive subtyping for functions:

    id : {A : MetaTy} → A → A
      = λ x. x

    f : Int → Int
      := id

This a bit weird; how can we turn the meta-level `id` to an object function? I previously
implemented this as a form of coercive subtyping, where a metafunction can be cast down to the
object-level by eta-expansion. Coercions can be inserted by bidirectional elaboration, when there's
a mismatch between inferred and expected types, in the following way.

    ────────────────
    coe A ↑A t = <t>

    ───────────────
    coe ↑A A t = ~ŧ

    ────────────────────
    coe Ty MetaTy A = ↑A

         _⇒_ is any kind of function type (object-level, meta)
    ───────────────────────────────────────────────────────────────────────────
    coe (A ⇒ B) (A' ⇒ B') t = λ x. coe (B (coe A' A x)) (B' x) (t (coe A' A x))

     [xᵢ : Aᵢ] is object-level or meta-level record
    ─────────────────────────────────────────────────
    coe [xᵢ : Aᵢ] [xᵢ : Bᵢ] t = (xᵢ = coe Aᵢ Bᵢ t.xᵢ)

In short, we can coerce between functions contra-and-covariantly, and between records covariantly,
as usual.

The `coe Ty MetaTy` rule makes it possible to omit a bunch of `↑`-s in surface syntax. For example,
an explicit type for `map` would be

    map : {A B : ValTy} → (↑A → ↑B) → ↑(List A) → ↑(List B)

But `A B : ValTy` implies that the codomain type must be in `MetaTy`, so we can the drop the lifts
in the surface syntax. When the elaborator hits `A` in `(A → <!-- B)`, we have `A : ValTy` with
`MetaTy` as expected type, so `A` is first --> <!-- silently cast to `Ty`, then to `↑A :
MetaTy`. -->

    map : {A B : ValTy} → (A → B) → List A → List B

In the previous example, `id` first gets elaborated to `id {?0} : ?0 → ?0 : MetaTy`, and we want to
coerce it to `Int → Int`. This works by eta-expansion, coercing the argument type backwards, and the
result forwards, getting as elaboration output

    f : Int → Int
      := λ x. ~(id {Int↑} <x>)

and as staging output

    f : Int → Int
      := λ x. x

When we coerce `x : Int` to have type `?0 : MetaTy`, since the stages don't match, the only coercion
rule that matches the situation yields `<x> : ↑Int`, at which point we don't have more coercion
rules, so we unify `↑Int` with `?0`. So at this point we learn what `?0` is, and then we just use
`Int ≤ ↑Int` to coerce the function body in the result.

**Example**. More coercive subtyping. Input:

    comp : {A B C : MetaTy} → (B → C) → (A → B) → A → C
      = λ f g x. f (g x)

    f : Int → Int
      := comp ((+) 10) ((+) 20)

Elaboration output:

    comp : {A B C : MetaTy} → (B → C) → (A → B) → A → C
      = λ f g x. f (g x)

    f : Int → Int
      := λ x. ~(comp {↑Int}{↑Int}{↑Int} (λ x. <10 + ~x>) (λ x. <10 + ~x>) <x>)

Staging output:

    f : Int → Int
      := λ x. 10 + (10 + x)

**Example**. List mapping, now for real, fully explicitly.

    data List (A : ValTy) : ValTy = Nil | Cons A (List A)

    map : {A B : ValTy} → (↑A → ↑B) → ↑(List A) → ↑(List B)
      := λ {A}{B} f xs.
        <letrec go := λcase
             Nil.       Nil
             Cons a as. Cons ~(f <a>) (go as);
         go ~xs>

The same with stage inference.

    map : {A B : ValTy} → (A → B) → List A → List B
      := λ f xs.
        letrec go := λcase
            Nil.       Nil
            Cons a as. Cons (f a) (go as);
        go xs

**Example**. Input:

    foo (x : Int) := map (+x)

Staging output, also after lambda-lifting.

    go x xs := case xs of
      Nil.       Nil
      Cons y xs. Cons (y + x) (go x xs)

    foo x xs := go x xs

Note that the recursive function gained an extra argument via lambda-lifting.

**Example**. Computing code from meta-level data. We do exponentiation, where the exponent is a
statically known natural number.

    data MNat : MetaTy = Zero | Suc MNat

    exp : ↑Int → MNat → ↑Int
    exp n e = case e of
      Zero.  1
      Suc e. n * exp n e

After elaboration:

    data MNat : MetaTy = Zero | Suc MNat

    exp : ↑Int → MNat → ↑Int
    exp n e = case e of
      Zero.  <1>
      Suc e. <~n * ~(exp n e)>

Now, `exp <10> (Suc (Suc Zero))` is staged to `<10 * (10 * 1)>`.


## 2. Basic concepts of code generation

### 2.1 Representability and binding time improvement

A type `A : MetaTy` is **representable** if it's equivalent to some `↑B`.

A representable metatype can be represented by an object type. The point is that representable types
which are *not* immediately of form `↑A` tend to support more computation at compile time, and thus
tend to be more useful in metaprogramming. An element of `↑A` is just a dead piece of code.

Moving from some type to an equivalent type which supports more static computation, is called
**binding time improvement** in the partial evaluation and staging literature.

Let's look at representability in the following.

The meta-level unit type is represented by the object unit type. This is pretty obvious.

`A → B` is representable exactly if `A` and `B` are representable. This is just the isomorphism
`↑(A → B) ≃ (↑A → ↑B)`, with components

    to   = λ f. λ a. <~f ~a>
	from = λ f. <λ a. ~(f <a>)>

This is the simplest example of binding time improvement. We can see that `λ a. <~f ~a>` contains a
meta-binder while `<λ a. ~(f <a>)>` contains an object binder. Hence the "binding time" is improved
by `to`. If we define an identity function as

    id : (A : Ty) → ↑A → ↑A

that's more useful than

    id : (A : Ty) → ↑(A → A)

The first `id` can be used in object code as `~(id A <x>)` which is definitionally `x`. The second
`id` can be used as `~(id A) <x>` which is definitionally `(λ x. x) x`, i.e. a useless beta-redex.

Finite products of representables are representable. In particular, `↑[A, B] ≃ [↑A, ↑B]`, with

    to   = λ x. (<(~x).1>, <(~x).1>)
	from = λ x. <~x.1, ~x.2>

This is also a form of binding time improvement, although this particular definition can be
dangerous. That's because it can duplicate computation. For example, for some `f : A → [B,C]`

    from (to <f x>) ≡ <((f x).1, (f x).2)>

That is, roundtripping performs an eta-expansion. How can we get rid of duplicate computations here?
If we stick to `[↑A, ↑B]` as the improved version of `↑[A, B]`, we can't get rid of it!
Fortunately, there's another improved version where the de-duplication is possible. This brings
us to the next section.

### 2.2 The code generation monad

Forcing an object-level computation is done by generating a `let`-binding. We can do `<let y := ~x;
...>`, and if we only refer to `y` in `...`, that means that only a *variable* is possibly
duplicated, which is already evaluated at this point at runtime, and we don't duplicate arbitrarily
expensive expressions. This is called **let-insertion** in staging literature.

Unfortunately, when defining `from : ↑[A, B] → [↑A, ↑B]`, we can't insert a `let`, because the
result a meta-level product, not a lifted type.

The **code generation monad `Gen`** is as follows:

    newtype Gen (A : MetaTy → MetaTy) = Gen {unGen : {R : Ty} → (A → ↑R) → ↑R}

We use completely standard Haskell-like definitions of functors and monads:

    class Functor (F : MetaTy → MetaTy) where
      _<$>_ : {A B : MetaTy} → (A → B) → F A → F B

    class Functor F => Applicative F where
      pure  : {A : MetaTy} → A → F A
      _<*>_ : {A B : MetaTy} → F (A → B) → F A → F B

    class Applicative M => Monad M where
      (>>=) : {A B : MetaTy} → M A → (A → M B) → M B

We shall use typeclasses only at the metalevel, and we'll use roughly the
same syntax and features as in Haskell. We'll also use Haskell's do-notation.

We can see that `Gen` is a monad indeed:

    instance Functor Gen where
	  f <$> Gen g = Gen λ k. g (λ a. k (f a))

    instance Applicative Gen where
	  pure a = Gen λ k. k a
	  Gen gf <*> Gen ga = Gen λ k. gf (λ f. ga (λ a. k (f a)))

	instance Monad Gen where
	  Gen ga >>= f = Gen λ k. ga (λ a. (unGen (f a) k))

`Gen A` can be viewed as an action whose effect is to produce some object code, returning `A` as
result value. `Gen` can be only run when the result type is an object type:

    runGen : Gen ↑A → ↑A
	  = λ ga. unGen ga (λ a. a)

One key feature in `Gen` is that object-level `let` can be always inserted. I call this
operation `gen`; it could be `genLet` but `gen` is just shorter.

    gen : ↑A → Gen ↑A
	  = λ a. Gen λ k. <let x = ~a; ~(k <x>)>

**Example:**

    myAction : Gen ↑Int
	  = do x ← gen <10 + 10>
	       y ← gen <10 * 10>
		   pure (x + y)

Now, `runGen myAction` is `<let x = 10 + 10; let y = 10 * 10; x + y>`. This example is pretty silly,
but the power of `Gen` is that we can `gen` even when we're computing a meta-level thing! In `Gen`,
we can generate any kind of object code and object binders, because we know that we'll return object
code in the end. But in the meanwhile we're also free to use arbitrary meta-level constructions.

One way to derive `Gen` is to consider the object-level identity functor:

    newtype Identity (A : Ty) = Identity A

Then ponder the binding-time improvement of `↑(Identity A)`. If we had polymorphic types in the
object language, we could use Church-coding:

    ↑(Identity A) ≃ ↑((R : Ty) → (A → R) → R)

Then we could use the representability of functions to get:

    (R : ↑Ty) → (↑A → ↑~R) → ↑~R

We don't have object-level polymorphism, but the end result is still expressible as:

    ↑(Identity A) ≃ (R : Ty) → (↑A → ↑R) → ↑R

which is precisely `Gen ↑A`. Hence, we have that `Gen ↑A ≃ ↑A` for all `A`. Note that we rely on
parametricity here, i.e. the inability to inspect the structure of object types.

Now we can get back to the improvement of product types. Let's have that the improvement of
`↑[A, B]` is `Gen [↑A, ↑B]`.

    to : ↑[A, B] → Gen [↑A, ↑B]
	  = λ x. do {x ← gen x; pure (<(~x).1>, <(~x).1>)}

Or, without monadic notation:

    to : ↑[A, B] → Gen [↑A, ↑B]
	  = λ x. Gen λ k. <let x := ~x; ~(k (<(~x).1>, <(~x).1>))>

Also:

    from : Gen [↑A, ↑B] → ↑[A, B]
	  = λ x. runGen do {(a, b) <- x; pure <(~a, ~b)>}

Without do-notation:

    from : Gen [↑A, ↑B] → ↑[A, B]
	  = λ x. unGen x (λ (a, b). <(~a, ~b)>)

Roundtripping works as follows:

    from (to x) = <let x := ~x; (x.1, x.2)>

`from (to x)` is also not the best code, because it includes an unnecessary eta-expansion in the
result. But that's only a constant-time overhead, while the previous roundtripping could duplicate
arbitrary computation.

#### Gen-representability





--
