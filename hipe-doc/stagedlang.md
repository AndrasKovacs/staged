
## Nice Strongly Typed Staged Programming

This document describes a programming language based on two-level type theory
(2LTT), which is not yet implemented and which I would personally like to use
for practical programming. Features:

- Robust performance guarantees, based on types.
- Plenty of control over code generation and memory layouts, in the surface language.
- Good ergonomics and strong inference (of types, values and staging annotations) in
  the surface language.

Overall, because this 2LTT is more performance-focused than Haskell or OCaml,
there are more details to keep in mind and manage. However, it's still a
"high-level" garbage-collected language, and we have significantly less
bureaucracy than in Rust.

In the following I describe language features, how to use them, and how to
compile them. I focus on examples. Programming abstractions and tiny libraries
will be also built up along the way.

Some of the choices of syntax and its sugar are fairly arbitrary and up to
personal taste, and are not set in stone.

Let's call the language simply 2LTT for now, for the sake of brevity. Keep in
mind though that there are many possible variations on the 2LTTs that appeared
in previous papers.

### Meta level

There's a dependently typed language layer for compile-time programming.

- `MetaTy` is the type of meta-level types. Whenever `A : MetaType`, `A` only
  exists at compile time.
- For simplicity, we have `MetaTy : MetaTy`.
- `MetaTy` supports Π, Σ, ⊤ and indexed inductive types, using the same syntax
  as in Agda.
- We use Agda syntax and Agda-style implicit arguments, with some differences.

A basic definition:

    id : {A : MetaTy} → A → A
	  = λ x. x

Unlike in Agda and Haskell, we don't repeat `id` for the type declaration and
the definition. We can also use Coq-style definitions:

    id {A : MetaTy}(x : A) = x

By convention, inductive types in `MetaTy` often have `₁` as subscript. That's
to disambiguate meta-level types from object-level ones. For example:

    data ℕ₁ : MetaTy where
	  zero₁ : ℕ₁
	  suc₁  : ℕ₁ → ℕ₁

Meta-level inductive types can only have fields with types in `MetaTy`.

However, the subscripts may be omitted when it's obvious which `ℕ` we use.
Syntax for Π (dependent functions) follows Agda.

There is a built-in notion of anonymous record types:

- `[field1 : A, field2 : B, field3 : C]` is a record type with three fields.  We
  may have zero or more fields. Types can depends on fields to the left.
- Meta-level record types can only have field types in `MetaTy`.
- There is a field projection notation. If `t : [field1 : A, field2 : B field1]`, then
  `t.field1 : A` and `t.field2 : B (t.field1)`.
- Field projections bind stronger than function application, so `B t.field1` is
  shorthand for `B (t.field1)`.
- Values with records types are `(t, u, v)`, or `(field = t, field2 = u, field3 = v)`.
  The unit type is defined as `[]` with `()` as value.
- Field names can be omitted, like in `[A, B, C]`. An unnamed field can be projected
  by number, so `t : [A, B]` implies `t.1 : A` and `t.2 : B`.

### Object level

A 2LTT program is compiled by first *staging* it, which means running all
metaprograms, which generate object code, and then further processing the object
code. Staging is the part of the pipeline where programmers still have full
control over compilation. This is in contrast to e.g. GHC where programmers lose
precise control immediately after their code is type checked, and then they have
to nag and cajole GHC with INLINE pragmas, compiler options and bang patterns,
to generate desired code.

The object language is very simple and dumb compared to the metalanguage. It is
easy to compile and optimize, but tedious to write code in. Fortunately, the
metalanguage is very expressive and hugely improves the usability of the overall
system, and since metaprograms only exists at compile time, we don't have to
deal with their fancy features in downstream compilation.

Let's get to the details of the object-level.

- `Ty : MetaTy` is the type of object types. Since `Ty` is in `MetaTy`, the
  object language can't abstract over object types. It's *simply typed*, there's
  no polymorphism and no type dependency.
- `ValTy : MetaTy` is the type of *value types* in the object language. Value
  types, for now, are just primitive machine types (like `Int64` and `Double`),
  record types and ADTs.
- `ValTy` is a subtype of `Ty`. So whenever `A : ValTy` then `A : Ty` as well.

The **function type former** is as follows:

    A : ValTy   B : Ty
	──────────────────
	    A → B : Ty

This implies that `→` cannot be a function input; `→` is a *first-order*
function type. As we'll see, `→` has the extremely useful property that it can
be compiled without closures, only using *lambda-lifting*. We'll also see that
closures are surprisingly rarely needed in ordinary function programming!

Field types of object-level records and ADTs must be value types. So the
following is OK:

    data List (A : ValTy) : ValTy where
	  Nil  : List A
	  Cons : A → List A → List A

The following is not OK:

    data Foo : ValTy where
	  MkFoo : (Bool → Int) → Foo

Object-level ADTs can be parameterized over anything, even meta-level types and
values, but fields must always be object-level value types. Object-level ADTs
also cannot have indices, only parameters.

We can use `unboxed data` to declared unboxed sums, similarly to enum types in Rust.

    unboxed data Maybe (A : ValTy) : ValTy where
	  Nothing : Maybe A
	  Just    : A → Maybe A

We can use Haskell-style ADT notation as shorthand, and leave parameter types
inferred. However, we always have to mark the return universe of the type:

    unboxed data Maybe A : ValTy = Nothing | Just A
	data List A : ValTy = Nil | Cons A (List A)
	data MetaList (A : MetaTy) : MetaTy = Nil | Cons A (MetaList A)

This will be compiled as a padded fixed-size structure with a tag field. Also
like in Rust, recursive fields of unboxed types must be guarded by an indirection.
Normal `data`-declared types are represented as variable-sized data behind an
indirection.

Example for an object-level definition at the top level, involving an ADT:

    mapAdd10 : List Int → List Int
	  := λ xs. letrec go := λ xs. case xs of
	             nil.       nil
				 cons x xs. cons (x + 10) (go xs);
	           go xs

Things to note:

- We use `:=` instead of `=` to define things at the object level. This will be
  important to disambiguate meta-level and object-level things; or at least, in
  my existing 2LTT implementations it has been a very good design choice do have
  this. The reason is that definitions and `let`-s are the main source of *stage
  ambiguity*, and if they're disambiguated, we can infer a *huge amount* of
  stages and stage annotations everywhere else.
- Recursion must be always introduced with a `letrec`.
- Note that `List Int` is a concrete monotype; it would be not possible to
  have a polymorphic `map` function here.

With a bit more syntactic sugar:

    mapAdd10 (xs : List Int) : List Int
	  := letrec go := λ case
	       nil.       nil
		   cons x xs. cons (x + 10) (go xs);
	     go

We have `let` and `letrec` in general. They are delimited with `;`, instead of the `in`
in Haskell.

    foo : Int :=
	  let x := 10;
	  let y := 20;
	  let z := 30;
	  x + y + z

`let` and `letrec` can be used to define values of any type (not just value types).
The following works:

    foo (x : Int) : Int :=
	  let bar := λ y z. x + y + z;
	  let baz := bar 10;
	  bar 20

**Importantly**: we can compute things in `Ty` by `case` splitting.  The
following is legal:

    foo : Bool → Int → Int
	  := λ b. case b of
	    true.  λ x. x + 10
		false. λ x. x * 10

I'll talk more about this later. In short, the ability to eliminate to `Ty` from
`ValTy` makes metaprogramming much more convenient, but it also requires a bit
more work in compiling the staging output.

The notation for **object-level record types** is the same as on the meta level.
However, the field assignment notation for object-level records uses `:=`,
following the convention in `let`:

    foo : [field1 : Int, field2 : Int]
	  := (field1 := 10, field2 := 20)

Object-level records don't support type dependency. They are compiled as
*unboxed tuples*. When we want to use boxed records instead, we can just
use a wrapper ADT definition:

    data Box (A : ValTy) : ValTy = Box A

The **closure type former**:

    A : ValTy   B : ValTy
	────────────────────
       A ~> B : ValTy

We overload `λ` and application for closures as well. Now we can write:

    map (f : Int ~> Int) : List Int → List Int
	  := letrec go := λ case
	        nil. nil
			cons x xs. cons (f x) (go xs);
	     go

In the following, we call `→` functions "functions", and `~>` functions
"closures".


### Compiling object-level functions

I said that functions can be compiled without closures. How can we do that,
given that

    case b of true.  λ x. x + 10
	          false. λ x. x * 10

is allowed? Also,

    foo (x : Int) : Int :=
	  let bar := λ y z. x + y + z;
	  let baz := bar 10;
	  let n   := bar 10 20;
	  baz n

The idea: functions can only ever be used by applying them to all arguments and
extracting the end result value. They also cannot escape their scope. Partial
applications in local `let`-s are considered as syntactic sugar for eta-expanded
definitions.

Hence, every `λ` which is under a `case`, can be floated out. So, we compile

    foo : Bool → Int → Int
	  := λ b. case b of
	    true.  λ x. x + 10
		false. λ x. x * 10

to

    foo : Bool → Int → Int
	  := λ b x. case b of
	    true.  x + 10
		false. x * 10

In Haskell, such transformation can degrade performance, if the case
scrutinization is expensive, and the function is called multiple times with the
same first argument. In that case, we would prefer to compute the expensive
casing just once, return a closure, and then happily apply that closure multiple
times.

In 2LTT, we'd need to explicitly use closure types to make this
possible. Function types in 2LTT only ever compute when fully applied. It does
not matter how cases and function lambdas are ordered, because whenever a
function is called, we have all arguments available.

After eta-expansions and `λ`-floating, all local functions are in a
lambda-liftable form. The functions which are only tail-called are kept
around as local join points, otherwise they are lifted out to the top.

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

Here, `baz` is only tail-called, so it's eta-expanded and kept as joint point.
`bar` is called in non-tail position so it's lifted to the top. Of course any
mildly optimizing compiler will inline `baz` as well - I'm only illustrating
lambda-lifting and call saturation here. In summary, function calls will always
be compiled to statically known saturated calls.

### Compiling closures

The implementation of our closures is closer to Koka and Rust, and differs from
OCaml and Haskell. In OCaml and Haskell the *arity* of a closure is only known
at runtime, and closure application has to dynamically check whether we have
under-application, saturated application or over-application.

In 2LTT, if `f : A ~> B`, then `f` has exactly one argument whose memory layout
is determined by the concrete `A` type, and a call to `f` does not perform any
runtime arity checking. Remember that the object language is monomorphic, so it
is naturally possible to associate different memory layouts to different types.

This implies that a multi-argument closure is usually efficiently represented as
`[A, B, C] ~> D`, as a closure which takes an unboxed tuple as argument. In
contrast, `A ~> B ~> C` is meant as `A ~> (B ~> C)`, a closure returning a
closure.

CPS is significantly more efficient in 2LTT than in GHC or OCaml:
- There is close to no dynamic overhead on closure calls, compared to making
  static calls. GHC and OCaml have the arity checking overhead, and GHC
  additionally needs to check for thunks.
- Continuation arguments can be unboxed, and generally have varying memory
  representation. In GHC and OCaml, closure arguments can only be generic heap
  objects. In short, CPS prevents unboxing there.

### Staging operations

Let's talk about staging now. So far, object-level and meta-level features
were separate, without much interaction to speak of. Staging operations
make it possible to move between stages.

1. If `A : Ty`, then `↑A : MetaTy`. This is pronounced "lift `A`". `↑A` is the
   type of metaprograms which produce `A`-typed object-level expressions.
2. If `t : A` and `A : Ty`, then `<t> : ↑A`, pronounced "quote `t`". This
   is the metaprogram which immediate returns `t` as an expression.
3. If `t : ↑A`, then `~t : A`, pronounced "splice `t`". Splicing runs
   a metaprogram and inserts its resulting expression at some point.

Additionally, we have `<~t> ≡ t` and `~<t> ≡ t` as definitional equalities; this
is often needed for type checking dependently typed metaprograms.
examples.

Splicing binds stronger than function application, so `f ~x ~y` means `f (~x) (~y)`
, but it's weaker than field projections, so `~f.x` means `~(f.x)`.

**Example**. Consider the meta identity function:

    id : (A : MetaTy) → A → A
	  = λ A x. x

Thanks to staging operations, this can be used in object code too:

    n : Int := ~(id (↑Int) <10>)

Here, `id (↑Int) <10> : ↑Int` is a metaprogram which computes to `<10>` during
staging. Then, we get `~<10>` in object code, which computes to simply `10`.

Now, it would be annoying if we had to write all of these quotes and splices
all the time. Fortunately, almost all of quotes and splices are unambiguously
inferable, with bidirectional elaboration and the disambiguated stages of
definitions (`=` vs `:=`). Something like the following works fine:

    id : {A : MetaTy} → A → A
	  = λ x. x

    n := id 10

How to elaborate `n`:

1. We try to infer an object type for `id 10`.
2. We look up the type of `id`.
3. Since `id` has implicit function type, we insert an implicit application, so
   we have `id {?0} 10` at this point, where `?0` is a metavariable.
4. We infer type for `10`, which is just `Int : ValTy`.
5. We try to cast `10 : Int : ValTy` to `?0 : MetaTy`. The stages don't match, so
   we lift the left side to `<10> : ↑Int : MetaTy`, and unify `↑Int` with `?0`.
6. Now we have `id {↑Int} <10> : ↑Int` as output.
7. Since we had `n :=`, an object expression is expected, so we cast the result
   down to object-level by inserting a splice.
8. `n := ~(id {↑Int} <10>)` is the final result.

I have implemented quote strong bidirectional stage inference in my 2LTT
prototype, so the inference promises I'm making here and later are based on
real-world experience.

**Example**. Coercive subtyping for functions:

    id : {A : MetaTy} → A → A
	  = λ x. x

    f : Int → Int
	  := id

This a bit weird; how can we turn the meta-level `id` to an object function? I
implemented this as a form of coercive subtyping, where a metafunction can be
cast down to the object-level by eta-expansion. Subtyping rules:

                         A' ≤ A      B ≤ B'            Aᵢ ≤ Bᵢ
    ──────    ──────     ──────────────────     ─────────────────────
    A ≤ ↑A    ↑A ≤ A     (A → B) ≤ (A' → B)     [xᵢ : Aᵢ] ≤ [xᵢ : Bᵢ]

In this example, `id` gets first elaborated to `id {?0} : ?0 → ?0 : MetaTy`, and
we want to coerce it to `Int → Int`. This works by eta-expansion, coercing the
argument type backwards, and the result forwards, getting as elaboration output

    f : Int → Int
	  := λ x. ~(id {Int↑} <x>)

and as staging output

    f : Int → Int
	  := λ x. x

When we coerce `x : Int` to have type `?0 : MetaTy`, we know that this is only
possible by quotation (since stages don't match), so we unify `↑Int` with `?0`
and insert a quote. So at this point we learn what `?0` is, and then we just use
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

Note that I left out the `↑`-s in the type signature. This is another
convenience feature that I've previously implemented. Since `ValTy` is in
`MetaTy`, we already know that `(A → B) → List A → List B` must be elaborated to
a `MetaTy` too. There's an ambiguity though in where we put lifts. For instance,
`↑(A → B) → ↑(List A → List B)` would be valid too. But in general, we want to
push lifts down, because `↑A → ↑B` supports more compile-time evaluation than
`↑(A → B)`. So it is a reasonable default to infer lifts in a way which yields
the most amount of compile-time computation.

**Example**. Let's stage some mapping. Input:

    foo (x : Int) := map (+x)

Staging output, also after lambda-lifting.

    go x xs := case xs of
	  Nil.       Nil
	  Cons y xs. Cons (y + x) (go x xs)

    foo x xs := go x xs

Note that the recursive function gained an extra argument via lambda-lifting.

**Example**. Computing code from meta-level data. We do exponentiation, where
the exponent is a statically known natural number.

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


### The Reader monad

Let's look at one of the simplest monads in this setting.

    Reader (R : ValTy) (A : Ty) : Ty = R → A

Let's define `pure` and binding, with staging annotations:

    pure : {R : ValTy}{A : Ty} → ↑A → ↑(Reader R A)
	  = λ a. <λ _. ~a>

    (>>=) : {R : ValTy}{A B : Ty} → ↑(Reader R A) → (↑A → ↑(Reader R B)) → ↑(Reader R B)
	  = λ ra f. <λ r. ~(f <~ra r>) r>

With full stage inference it's just the usual thing:

    pure : {R : ValTy}{A : Ty} → A → Reader R A
      = λ a _. a

    (>>=) : {R : ValTy}{A B : Ty} → Reader R A → (A → Reader R B) → Reader R B
	  = λ ra f r. f (ra r)

There's a problem with this definition.
