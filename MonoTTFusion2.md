
### Staged fusion without closures

The metatheory has `U1` as universe, closed under whatever type formers we want.
The object theory is simply typed. Object-theoretic types are specified as
follows:

    ValTy        : U1
    _~>_         : ValTy → ValTy → ValTy    -- closure-based functions
    <ADTs>       : ValTy                    -- ADTs may only have ValTy fields
    <primitives> : ValTy

    Ty   : U1
    ^_   : Ty → U1
    val  : ValTy → Ty
    _->_ : ValTy → ValTy → Ty  -- non-closure (static) functions

I will usually leave the `val` embedding implicit in notation.

We don't have currying in the object theory; multi-argument static functions
take tuples as argument. We still have currying in the meta-level, of course.

Importantly, we allow recursion on `ValTy` and `Ty` in the meta level
("typecase"). This is semantically totally fine since object types contain
no binders and have first-order definitional equality.

Object-level ADTs support *paramorphism with accumulation* as recursion
principles. First, I give a low-level specification for list-recursion, purely
in the object theory:

    A, B, C : ValTy
    Γ, a : val A, as : val (List A), rec : B → C, acc : val B ⊢ c : val C
    Γ, acc : val B ⊢ n : val C
    Γ ⊢ as : val (List A)
    Γ ⊢ acc : val B
    ────────────────────────────────────────────────────────────────────────────────
    Γ ⊢ ListRec A B C cons nil as acc : val C

    ListRec A B C c n nil         acc = n[acc ↦ acc]
    ListRec A B C c n (cons a as) acc = c[a ↦ a, as ↦ as, rec ↦ λ x. ListRec A B C c n as x, acc ↦ acc]

We define now an alternative lifting operation by typecase:

    ^* : Ty → U1
	^*(val A) = ^(val A)
	^*(A → B) = ^A → ^B

We can also define `to : ^A → ^*A` and `from : ^*A → ^A`.

Now, we can define a foldr for lists in the metatheory which supports
elimination to arbitrary types:

    foldr : {A : ValTy}{B : Ty} → (^A → ^*B → ^*B) → ^*B → ^(List A) → ^*B

	foldr {A}{val B} f b as =
	  <ListRec A ⊤ B (a as rec acc. ~(f <a> <rec acc>)) (acc. ~b) ~as tt>

	foldr {A}{B₀ → B₁} f b as =
	  λ b₀. <ListRec A B₀ B₁ (a as rec acc. ~(f <a> (λ x. <rec ~x>) <acc>)) (acc. ~(b <acc>)) ~as ~b₀>

We use this for push lists:

    Push : ValTy → U1
	Push A = {B : Ty} → (^A → ^*B → ^*B) → ^*B → ^*B

    up : ^(List A) → Push A
	up as = λ f b. foldr f b as

	down : Push A → ^(List A)
	down as = as {val (List A)} (λ a as. <cons ~a ~as>) <nil>

	map : (^A → ^B) → Push A → Push B
	map f as = λ c n. as (λ a bs. c (f a) bs) n

    foldl : {A B : ValTy} → (^B → ^A → ^B) → ^B → Push A → ^B
	foldl f b as = as {B → B} (λ a rec acc. rec (f acc a)) (λ acc. acc) b

	sum : Push Nat → ^Nat
	sum = foldl (λ x y. <x + y>) <0>

So we get fusion for foldl, without closures.

### Fixpoints

If we want non-termination in the object language, it's more convenient to
specify fixpoints (or letrec-s). Let's have case splits for ADTs and general
fixpoints. However, case splits can only target value types, and fixpoints are
specialized to function types.

    Γ, rec : val A → val B, a : val A ⊢ t : val B
	Γ ⊢ u : val A
	────────────────────────────────────────────────
	Γ ⊢ fix (rec a. t) u : val B

	fix (rec a. t) u = t[rec ↦ λ x. fix (rec a. t) x, a ↦ u]

The list foldr is now reproduced as:

    foldr : {A : ValTy}{B : Ty} → (^A → ^*B → ^*B) → ^*B → ^(List A) → ^*B

	foldr {A}{val B} f b as =
	  <fix (rec as. case as of nil       -> ~b
	                           cons a as -> ~(f <a> <rec as>))
           ~as>

    foldr {A}{B₀ → B₁} f b as =
	  λ acc. <fix (rec (as, acc). case as of nil       -> ~(b <acc>)
    	                                     cons a as -> ~(f <a> (λ acc. <rec (as, acc)>) <acc>))
                  (~as, ~acc)

For the functional case above we use a pair type for the recursion. The `Push`
definitions remain the same.

We can do non-termination now, so we can convert between lists and `Pull` streams.

First, let's write a helper function for list splitting:

    ListCase : ^(List A) → ^*B → (^A → ^(List A) → ^*B) → ^*B
	ListCase {B = val B} as n c =
	  <case ~as of nil -> ~n; cons a as -> ~(c <a> <as>)>
	ListCase {B = B₀ → B₁} as n c x =
	  <case ~as of nil -> ~(n x); cons a as -> ~(c <a> <as> x)>

`Pull`:

    Step : ValTy → ValTy → U1
	Step A S = {R : Ty} → ^*R → (^A → ^S → ^*R) → ^*R

    Pull : ValTy → U1
	Pull A = (S : ValTy) × (^S → Step A S) × ^S

	fromList : ^(List A) → Pull A
	fromList as = (List A, (λ as stop yield. ListCase as stop yield), as)

	toPush : Pull A → Push A
	toPush (S, step, s) {val R} c n =
	  <fix (rec s. ~(step <s> n (λ a s. c a <rec ~s>)) ~s>
	toPush (S, step, s) {R₀ → R₁} c n x =
	  <fix (rec (s, x). ~(step <s> (n x) (λ a s x. c a (λ x. <rec (~s, ~x)>) x)) (~s, ~x)>

    map : (^A → ^B) → Pull A → Pull B
	map f (S, step, s) = (S, (λ as stop yield. step stop (λ a s. yield (f a) s)), s)

In summary, when we convert between different representations, we usually have
to match on object types.
