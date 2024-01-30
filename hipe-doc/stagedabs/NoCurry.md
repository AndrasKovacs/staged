
### Letrec-s + case

Case only targets value types.

    FunTy : MetaTy
	FunTy = ValTy × ValTy

	↑* : FunTy → MetaTy
	↑* (A, B) = ↑A → ↑B

    Push : MetaTy → MetaTy
    Push A = (R : FunTy) → (A → ↑*R → ↑*R) → ↑*R → ↑*R

    push : {A : ValTy} → ↑(List A) → Push ↑A
	push {A} as (Acc, Res) cons nil acc = <
	  letrec go : (List A, Acc) → Res
	         go (as, acc) = case as of
			   Nil       -> ~(nil <acc>)
			   Cons a as -> ~(cons <a> (λ acc. <go (as, acc)>) <acc>);
      go (~as, ~acc)>

    foldrPush : {A B : ValTy} → (↑A → ↑B → ↑B) → ↑B → Push ↑A → ↑B
	foldrPush f b as = as () B (λ a rec acc. f a (rec acc)) (λ _. b) <()>

	toList : Push ↑A → ↑(List A)
	toList as = foldr (λ a as. <Cons ~a ~as>) <Nil> as

	foldlPush :: {A B : ValTy} → (↑B → ↑A → ↑B) → ↑B → Push ↑A → ↑B
	foldlPush f b as = as B B (λ a rec acc. rec (f acc a)) (λ acc. acc) b

	Gen : MetaTy → MetaTy
	Gen A = {R : Ty} → (A → ↑R) → ↑R

    instance Functor Gen where
	  f <$> Gen g = Gen λ k. g (λ a. k (f a))

    instance Applicative Gen where
	  pure a = Gen λ k. k a
	  Gen gf <*> Gen ga = Gen λ k. gf (λ f. ga (λ a. k (f a)))

	instance Monad Gen where
	  Gen ga >>= f = Gen λ k. ga (λ a. (unGen (f a) k))

	bcase : ↑Bool₀ → Gen A → Gen A → Gen A
	bcase b t f = do
	  b <- foo b
	  join $ if b then t else f

    looks OK!

    <!-- case : ↑Bool → Gen A → Gen A → Gen A -->
	<!-- case b t f = Gen λ k acc. <case ~b of -->
	<!--   True  → ~(unGen t λ k acc'.  ) -->
	<!--   False → ~(unGen t λ k  -->
	<!--   > -->

    <!-- gen : ↑A → Gen ↑A -->
	<!-- gen a = Gen (λ k. <let x := ~a; ~(k <x>)>) -->

    <!-- Pull : ValTy → MetaTy -->
	<!-- Pull A = (S : ValTy) × (^S → MaybeT Gen A) × ^S -->

	<!-- <\!-- pull : {A : ValTy} → ↑(List A) → Push ↑A -\-> -->
	<!-- <\!-- pull {A} as = (List A, (λ as. _) -\-> -->






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
