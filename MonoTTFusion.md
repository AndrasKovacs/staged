
### Staged fusion with guaranteed closure-freedom

In this note I demonstrate staged fusion in a two-level type theory which
guarantees that the code output does not introduce any new closures.

#### Theory

The metatheory has `U1` as universe, closed under whatever type formers we want.
The object theory is monomorphic and simply typed. Object-theoretic types are
specified as follows:

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
take tuples as argument. We still have currying in the meta-level of 2LTT, so
this isn't a big deal.

Importantly, we allow recursion on `ValTy` and `Ty` in the meta level
("typecase"). This is semantically totally fine, since there are no binders
involved.

Object-level ADTs can be parameterized in the meta level. They support
*paramorphism with accumulation* as recursion principles. I give some
examples. First, I give a low-level specification for list-recursion, purely in
the object theory:

    A, B, C : ValTy
    Γ, a : val A, as : val (List A), rec : val B → val C, acc : val B ⊢ c : val C
    Γ, acc : val B ⊢ n : val C
    Γ ⊢ as : val (List A)
    Γ ⊢ acc : val B
    ────────────────────────────────────────────────────────────────────────────────
    Γ ⊢ ListRec A B C cons nil as acc : val C

    ListRec A B C c n nil         acc = n[acc ↦ acc]
    ListRec A B C c n (cons a as) acc = c[a ↦ a, as ↦ as, rec ↦ λ x. ListRec A B C c n as x, acc ↦ acc]


This can be faithfully specified in 2LTT in a more compact way (also omitting `val`-s):

    ListRec : {A B C : ValTy} → (^A → ^(List A) → (^B → ^C) → ^B → ^C)
                              → (^B → ^C)
                              → ^(List A)
                              → ^B
                              → ^C

This recursion principle allows us to derive left folds:

    foldl : {A B : ValTy} → (^B → ^A → ^B) → ^B → ^(List A) → ^B
    foldl f b as = ListRec (λ a _ rec b. rec (f b a)) (λ b. b) as b

Importantly, by using meta-level typecase, we also derive right folding which
eliminates into arbitrary object types:

    foldr : {A : ValTy}{B : Ty} → (^A → ^B → ^B) → ^B → ^(List A) → ^B
    foldr {A}{B} f b as = case B of
      val B      → ListRec {A}{⊤}{B} (λ a _ rec acc. f a (rec acc)) (λ _. b) as tt
      (B₀ → B₁)  → <λ acc. ~(ListRec {A}{B₀}{B₁}
                            (λ a _ (rec : ^B₀ → ^B₁) (acc : ^B₀). f a (rec acc))
                            (λ acc. <~b ~acc>) as <acc>)>

We want unrestricted meta-level elimination but restricted object-level elimination. In the object theory,
something like `λ b. case b of true → λ x. x + 1; false → λ x. x - 1` would be problematic, because we
want to compile functions without closures, and we would need to appeal to some case commutation law to
translate the above to `λ b x. case b of true → x + 1; false → x - 1`. Moreover, such commutation interacts
poorly with object-level side effects.

In the current system, we have the following Bool recursion:

    BoolRec : {A B : ValTy} → (^A → ^B) → (^A → ^B) → ^Bool → ^A → ^B

From which we derive unrestricted if-then-else:

    if_then_else_ : {A : Ty} → ^Bool → ^A → ^A → ^A
    if_then_else_ {A} b t f = case A of
      val A     → BoolRec {⊤}{A}(λ _. t) (λ _. f) b <tt>
      (A₀ → A₁) → <λ x. ~(BoolRec {A₀}{A₁} (λ x. <~t ~x>) (λ x. <~f ~x>) b <x>)>

Now, we can write

    if true then λ x. x + 1 else λ x. x - 1

With staging annotations:

    ~(if <true> then <λ x. x + 1> else <λ x. x - 1>) : Nat → Nat

After staging:

    λ x. BoolRec Nat Nat (x. (λ x. x + 1) x) (x. (λ x. x - 1) x) b x

The extra beta-redexes

This piece of code can be obviously compiled without closures because the inner
`x.` binder is not a function binder, it merely refers to the additional
accumulator that is being passed. Let's look at a nested if-then-else:

    ~(if b₁ then λ x. if b₂ then λ y. x + y else λ y. x * y
            else λ x y. x)

After staging, we get

    λ x. BoolRec







<!-- Second, by typecasing in the metatheory, we can define a custom lifting operation: -->

<!--     ^* : Ty → U1 -->
<!-- 	^* (val A) = ^(val A) -->
<!-- 	^* (A → B) = ^A → ^B -->

<!-- 	~*_ : {A} → ^*A → ^A -->
<!-- 	~*_ {val A} x = x -->
<!-- 	~*_ {A → B} f = <λ x. ~(f <x>)> -->

<!-- 	<_>* : {A} → ^A → ^*A -->
<!-- 	<_>* {val A} x = x -->
<!-- 	<_>* {A → B} f = λ x. <~f ~x> -->

^(A → B) ≃ (^A → ^B)
