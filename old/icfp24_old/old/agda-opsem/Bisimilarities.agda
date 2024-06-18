
module Bisimilarities where

open import Lib
open import Syntax
open import Interpreter
open import Renaming
open import Bisimilarity

-- Some β-η

-- function
--------------------------------------------------------------------------------

fun-η : (t : Tm Γ (ℂ (a ⇒ B))) → ∀sim t (lam (app (t [ suc ]) (varV zero)))
fun-η t n k lt γ γ' γ~ (app as v) =
     ren-eval (wk idr) t k (wkV-idr γ v) (app as v) ⁻¹
   ◼ sim-refl (t [ suc ]) n k lt (defV γ v) (defV γ' v) (γ~ , refl) (app as v)

fun-var-β : (t : Tm (Γ ▶ V a) B)(x : Var Γ (V a)) → ∀sim (app (lam t) (varV x)) (t [ renameTop idr x ])
fun-var-β t x n k lt γ γ' γ~ as =
    ren-eval _ t k (renameTopV x identity) as ⁻¹
  ◼ sim-refl (t [ renameTop idr x ]) _ _ lt _ _ γ~ as
-- β holds in general for terms which don't contain fun calls ("ground values")

-- product
--------------------------------------------------------------------------------

pair-η : (t : Tm Γ (V (a * b))) → ∀sim t (pair (fst t) (snd t))
pair-η t n k lt γ γ' γ~ [] rewrite sim-refl t _ _ lt _ _ γ~ [] with eval t k γ' []
... | nothing         = refl
... | just (pair _ _) = refl

-- fst-β and snd-β only work with ground values in the other projection

-- sum
--------------------------------------------------------------------------------

-- works with ground values in general
inl-var-β : (x : Var Γ (V a))(l : Tm (Γ ▶ V a) B)(r : Tm (Γ ▶ V b) B)
            → ∀sim (split (inl (varV x)) l r) (l [ renameTop idr x ])
inl-var-β x l r n k lt γ γ' γ~ as =
    ren-eval _ l k (renameTopV x identity) as ⁻¹
  ◼ sim-refl (l [ renameTop idr x ]) _ _ lt _ _ γ~ as

inr-var-β : (x : Var Γ (V b))(l : Tm (Γ ▶ V a) B)(r : Tm (Γ ▶ V b) B)
            → ∀sim (split (inr (varV x)) l r) (r [ renameTop idr x ])
inr-var-β x l r n k lt γ γ' γ~ as =
    ren-eval _ r k (renameTopV x identity) as ⁻¹
  ◼ sim-refl (r [ renameTop idr x ]) _ _ lt _ _ γ~ as

-- sum-η works in general

-- one has η for ground values
-- letV unfolding as well
-- letC unfolding is not valid up to strong bisimulation, but would be up to weak bisim,
--  if the defined function is not actually recursive


-- μ
--------------------------------------------------------------------------------
μ-beta : (t : Tm Γ (V (⟦ F ⟧ (μ F)))) → ∀sim (unwrap {F = F} (wrap t)) t
μ-beta t n k lt γ γ' γ~ [] rewrite sim-refl t _ _ lt _ _ γ~ [] with eval t k γ' []
... | nothing = refl
... | just v  = refl

μ-eta : (t : Tm Γ (V (μ F))) → ∀sim (wrap {F = F} (unwrap t)) t
μ-eta t n k lt γ γ' γ~ [] rewrite sim-refl t _ _ lt _ _ γ~ [] with eval t k γ' []
... | nothing = refl
... | just (wrap _)  = refl

-- Commutations
--------------------------------------------------------------------------------

-- (letV x = t; u) y  ~   (letV x = t; u y)

letV-app-var : (t : Tm Γ (V a))(u : Tm (Γ ▶ V a) (ℂ (a ⇒ B)))(x : Var Γ (V a))
             → ∀sim (app (letV t u) (varV x)) (letV t (app u (varV (suc x))))
letV-app-var t u x n k lt γ γ' γ~ as rewrite sim-refl t _ _ lt _ _ γ~ [] with eval t k γ' []
... | nothing = refl
... | just v  rewrite lookupV-sim-refl x γ~ =
  sim-refl u _ _ lt (defV γ v) (defV γ' v) (γ~ , refl) _


-- (letC x = t; u) y  ~  (letC x = t; u y)

letC-app-var : (t : Tm (Γ ▶ ℂ α) (ℂ α))(u : Tm (Γ ▶ ℂ α) (ℂ (a ⇒ B)))(x : Var Γ (V a))
             → ∀sim (app (letC t u) (varV x)) (letC t (app u (varV (suc x))))
letC-app-var t u x n k lt γ γ' γ~ as rewrite lookupV-sim-refl x γ~ =
  sim-refl u _ _ lt (defC γ t) (defC γ' t) (γ~ , (next-sim (sim-refl t k))) _


-- (case t of inl x. l; inr x. r) y   ~   case t of inl x. l y; inr x. r y

split-app-var : (t : Tm Γ (V (a + b)))(l : Tm (Γ ▶ V a) (ℂ (c ⇒ D)))(r : Tm (Γ ▶ V b) (ℂ (c ⇒ D)))(x : Var Γ (V c))
              → ∀sim (app (split t l r) (varV x)) (split t (app l (varV (suc x))) (app r (varV (suc x))))
split-app-var t l r x n k lt γ γ' γ~ as rewrite sim-refl t _ _ lt _ _ γ~ [] with eval t k γ' []
... | nothing      = refl
... | just (inl v) rewrite lookupV-sim-refl x γ~ = sim-refl l _ _ lt (defV γ v) (defV γ' v) (γ~ , refl) _
... | just (inr v) rewrite lookupV-sim-refl x γ~ = sim-refl r _ _ lt (defV γ v) (defV γ' v) (γ~ , refl) _


-- t u  ~  let x = u in t x

letV-bind-arg : (t : Tm Γ (ℂ (a ⇒ B)))(u : Tm Γ (V a)) → ∀sim (app t u) (letV u (app (t [ suc ]) (varV zero)))
letV-bind-arg t u n k lt γ γ' γ~ as rewrite sim-refl u _ _ lt _ _ γ~ [] with eval u k γ' []
... | nothing = refl
... | just v  = ren-eval (wk idr) t k (wkV-idr γ v) (app as v) ⁻¹ ◼ sim-refl (t [ suc ]) _ _ lt (defV γ v) (defV γ' v) (γ~ , refl) _
