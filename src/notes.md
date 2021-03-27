
- Existing 2lvlTT demo:
  - Basic type theory (the same type theory (Π, Σ, U)) on every level
     (eval + elaboration + unification : uniform in levels)
	 (during elab, levels aren't very intrusive)

  - Separate step: staging: different value representation, different evaluator (carries levels around)
    - there are no unknown levels at this point (because we already finished elaboration, everything is known)

- First: type formers + definitional equality on  different levels

- data U = U0 CV | U1
- data CV = C | V       -- value/computation types

  U0 C : U1      -- type of computation types
  U0 V : U1      -- type of value types
  U1   : U1      -- type of metatypes

  Eff : U0 V → U0 C

  (implicit cumulative subtyping)    (cumulative means: lifting preserves everything strictly)
        U0 V ≤ U0 C

  U0 C ≤ U1        (explicit coercive subtyping, coercion is given by ^ _)
  U0 V ≤ U1        (explicit coercive subtyping,                      ^ _)

  _→_ : U0 V → U0 C → U0 C

  ADTs only at lvl 0 (single-sorted, create value types)

  for example:
     -- non-generic ADT
     data
	   Nat  : U0 V
	   zero : Nat
	   suc  : Nat → Nat

     -- generic ADT
     data
	   List : U0 V → U0 V
	   nil  : {A : U0 V} → ^(List A)
	   cons : {A : U0 V} → ^~A → ^(List A)

   case expression : eliminates from U0 V to U0 cv

   fixpoints

   - records: on both levels
      ( TODO : scoping magic for lvl 1 record types )
	     -- myrec : [foo : ^A, bar : ^B]
		 -- convert myrec to an iterated object-level let expression

   - on lvl 0, records are simply typed

--------------------------------------------------------------------------------

- definitional equality?

- what about def. equality of fixpoints?
  - Agda: every fixpoint is inseparably tied to its name
          a recursive application only computes if a telescope of args makes the underlying case tree compute

  - Coq: every fixpoint has a single distinguished argument which decreases structurally
         fix application only computes if there's an application to the decreasing argument, moreover the arg is canonical

    (fix go. λ x. case x of
	  Zero  → _
	  Suc x → _) (zero)

  - Mini-tt:
      - recursive definitions (both for values and types)
	  - case splitting functions (\case in Haskell)

	  - There's no unfolding of recursive names under a Sum type
	  - There's no unfolding of recursive names under a case split
	     (reasoning: any struct recursive function must eventually case split on decreasing arguments)

	  Nat = Sum(Zero: Nat, Suc : Nat → Nat)

	  add : Nat → Nat → Nat
	  add = split
	    zero  → λ b. b
		suc a → λ b. suc (add a b)

	  unify add add

  - PiSigma (a bit like mini-tt, but with fancy alpha-conversion checking)
      (knows about renaming of all recursive names)

  	  Nat  = Sum(Zero: Nat , Suc : Nat  → Nat )
	  Nat' = Sum(Zero: Nat', Suc : Nat' → Nat')


--------------------------------------------------------------------------------

- I don't want *any* beta rules in level 0 (intensional source code)
-  (I don't have to care about conversion checking fixpoints!)

- (During elaboration):
- In Lvl 0
  - Weakening, no substitution (we can still splice into lvl 0 code)
                (λ x y. f x y z )[z ↦ t  ]
				(λ x y. f x y ~z)[z ↦ <t>]
  - No beta rules, only one computation rule: ~<t> = t

  - *Every* syntactic construction is injective:
      e.g.     app t u = app t' u'  iff   t = t'  and  u = u'

  - Example for unifying lvl 0 things:

     -- refl f : Id ^Nat <f> <g>

--------------------------------------------------------------------------------

-- Lvl 0 has slightly different type formers
-- Lvl 0 has *very* different conversion rules  (eval, unify depends on level)
--   (I want to elab/eval/unify while having unknown levels)

let f = λ x. x in
let g : Nat → Nat = f in
...

What I tried:

   syntax :
      data Tm0
	  data Tm1
   values :
      data Val0
	  data Val1

syntax
  data Tm

values:
  data Val0
  data Val1

syntax
  data Tm
  data Val

-- I'm on the fourth version

syntax
  data Tm
values
  data Val
-- some ops parametrized by U

-- basic idea of Tm / Val
--------------------------------------------------------------------------------

-- Tm is immutable source code
-- Val is runtime object
--   Val has domain-specific features in support of unification

-- eval : Env -> Tm -> Val
-- check
-- infer

-- simple Val: exactly the same as Tm, closures instead of binders
--            (+ de Bruijn levels instead of indices)

data Closure = Closure Env Tm

data Val
  = Var Lvl
  | App Val Val
  | Pi Name Val Closure
  | Lam Name Val Closure
  | U


-- more sophisticated with metas
data Spine
  = Id
  | App Spine Val

data Val
  = Flex  MetaVar Spine     -- immediate access to the reason of blocking this computation
  | Rigid Lvl     Spine
  | UnfoldSolved MetaVar Spine ~Val   -- delayed unfoldings ("glued" evaluation)
  | UnfoldTopDef Lvl     Spine ~Val
  | Pi Name Val Closure
  | Lam Name Val Closure
  | U

force :: Val -> Val
force = \case
  Flex m sp -> try to unblock
  t         -> t


-- drop Rigid
--------------------------------------------------------------------------------

data Spine
  = Id
  | App Spine Val

-- drop Rigid
data Val
  = Flex MetaVar Spine     -- immediate access to the reason of blocking this computation
  | Var Lvl
  | App Val Val
  | Pi Name Val Closure
  | Lam Name Val Closure
  | U

--------------------------------------------------------------------------------

-- metavariables are *always* lvl 1
-- no type for contextual metas in lvl 0 (lvl 0 _→_ type cannot abstract over all things in scope)
-- Meta only in lvl1 : complication :

--   let f = λ x y. _ in
--   ...

-- _^: U0 cv → U1   (lifting from 0 to 1)

-- I have to add "weak" lifting: lifts from any level to U1
--    (has the identity action when lifting from U1 to U1)
--    (need "weak" term lifting as well)

--   WkLift    : {u : U} → u → U1                                     WkLift {U1} A = A
--   WkTmLift  : {u : U}{A : u} → A → WkLift A          -- quote      WkTmLift {U1} t = t
--   WkTmLower : {u : U}{A : u} → WkLift A → A          -- splice     WkTmLower {U1} t = t

--------------------------------------------------------------------------------

-- What if there's some F : V → C, only way to go this way, F is a monad.
--  the only way to call funtions is to bind results
--       let x = f t u v in e
--       e[x ↦ f t u v]
--       f t u v >>= \x -> e

-- CBV + side effects in any position
--   I have to first convert to monad IR
--

CBV + side effects complication:

f : Bool → Nat → Nat
f = λ b. if b t
      then λ x. x
      else λ y. suc y

-- arity raising
f : Bool → Nat → Nat
f = λ b x. if b t
      then x
      else suc x

f : Bool → Nat → Nat
f = λ b. print "foo"; if b t
      then λ x. x
      else λ y. suc y

f : Bool → Nat → Nat
f = λ b x. print "foo";
      if b
        then x
        else suc x

<!-- f : Bool → Nat → Eff Nat -->
<!-- f = λ b. print "foo"; if b t -->
<!--       then λ x. x -->
<!--       else λ y. suc y -->












































































































<!-- - Differences between lvl1 and lvl0. -->

<!-- - But: we want to represent constructions at unknown levels! -->

<!-- - data Tm0 -->
<!-- - data Tm1 -->

<!-- - data Tm : Level -> Type (Haskell types too weak) -->


<!-- Desired: -->

<!-- - Representing constructions (lam/app/record) with unknown stages in syntax. -->
<!-- - Evaluating/unifying different stages differently. -->
<!--   - Meta constructions behave like usual type theory -->
<!--   - Object level has -->
<!--     * No beta rules, only the splice/quote computation rule -->
<!-- 	* no substitution, only renaming -->
<!-- 	* No metas! Contextual metas aren't even typable on lvl0. We can splice in lvl1 metas. -->
<!-- 	* injectivity for every syntactic rule other than splice. -->
<!-- - Not having too much noise and redundancy in data. -->
<!-- - Reusing quotation as staging as first solution. -->

<!-- Current design: -->

<!-- - The core syntax is oblivious of stages, which makes it possible to represent -->
<!--   lambdas/apps/etc with unknown stages while doing elaboration. -->

<!-- - We have just a single Val type for values, but we make a *hard distinction* between differently -->
<!--   staged construtions. Evaluation blocks on unknown stages! Unification as well. -->

<!-- - Lvl0 has *no metas*! The only way to have metas in Lvl0 is to splice them. -->
<!--   * Contextual metas in Lvl0 are not eve typable! We can't abstract over stuff using Lvl0 arrow. -->
<!--     Even if we did, it doesn't compute! -->
<!--   * We need weak lifting for contextual metas and inference: -->
<!--     Lift : {u : U} → u → U1                  Lift {U1} A = A -->
<!-- 	Up   : {u : U}{A : u} → A → Lift A       Up {U1} t = t -->
<!-- 	Down : {u : U}{A : u} → Lift A → A       Down {U1} t = t -->

<!-- - Universe representation: -->
<!--   data CV = C | V         -- computation and value types -->
<!--   data U = U0 CV | U1     -- runtime and meta univs -->

<!-- - We have -->
<!--   * U0 C ≤ U0 V -->
<!--   * U0 x ∈ U1 -->
<!--   * Later: Eff : U0 V → U0 C -->
<!--   * U0 V and U1 closed under records -->
<!--   * _→_ : U0 V → U0 C → U0 C -->
<!--   * U0 V closed under ADTs -->
<!--     - non-mutual ADT -->
<!--     - single case splits -->
<!-- 	- single fixpoints -->
<!--   * Arithmetic primops -->

<!-- - We have *three* metacontexts -->
<!--   - The usual one with possibly unknown U -->
<!--   - U metas -->
<!--   - CV metas -->

<!-- - Subtyping: -->
<!--   * implicit (strictly preserves all constructions) -->
<!--     - U0 V ≤ U0 C -->

<!--   * coercive (doesn't preserve anything strictly) -->
<!--     - A ≤ ^A -->
<!--     - ^A ≤ A -->
<!--     - _→_ contra/covariant -->
<!--     - Rec covariant -->




<!-- SIMPLE PLAN for compilation: -->

<!-- Pipeline -->
<!--   - Parse -->
<!--   - Elaborate -->
<!--   - Stage to monomorphic core, reusing quotation from elab phase -->
<!--   - Normalize typereps -->
<!--   - Arity expand + lambda lift -->
<!--   - Convert to ANF -->
<!--   - Allocate stack frames, annotate code with frame init/store/read ops -->
<!--     - Should handle tail calling here as well -->
<!--   - Codegen -->

<!-- Infrastructure: -->

<!--   - Type checker and interpreter for *every* IR except the actual backend -->
<!--     - Closed evaluator for pre-stage core -->
<!-- 	- Closed evaluator for mono-core -->
<!-- 	- Closed evaluator for ANF core -->
<!-- 	- Try to emulate relocating GC for the lowest IR for debugging purposes. -->
<!-- 	  (Have explicit stack + heap) -->

<!--   - Use static analyzer / valgrind / address sanitizer for RTS and a bunch of -->
<!--     backend output -->

<!-- gcc backend TODO : -->

<!--   - option for word-aligning code pointers -->
<!--   - find out how to ensure tail calls (visibility options, opt flag) -->
