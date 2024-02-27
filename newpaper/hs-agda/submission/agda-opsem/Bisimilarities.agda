
module Bisimilarities where

{-
We validate some bisimilarities in this module, in particular those that come
handy in call saturation.
-}

open import Lib
open import Syntax
open import Interpreter
open import Renaming
open import Bisimilarity


-- functions
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


-- computational products
--------------------------------------------------------------------------------

Pair-η : (t : Tm Γ (ℂ (α * β))) → ∀sim t (Pair (Fst t) (Snd t))
Pair-η t n k lt γ γ' γ~ (fst as) = sim-refl (Fst t) _ _ lt _ _ γ~ as
Pair-η t n k lt γ γ' γ~ (snd as) = sim-refl (Snd t) _ _ lt _ _ γ~ as

Fst-β : ∀ (t : Tm Γ (ℂ α))(u : Tm Γ (ℂ β)) → ∀sim (Fst (Pair t u)) t
Fst-β t u n k lt γ γ' γ~ as = sim-refl t _ _ lt _ _ γ~ as

Snd-β : ∀ (t : Tm Γ (ℂ α))(u : Tm Γ (ℂ β)) → ∀sim (Snd (Pair t u)) u
Snd-β t u n k lt γ γ' γ~ as = sim-refl u _ _ lt _ _ γ~ as


-- value products
--------------------------------------------------------------------------------

pair-η : (t : Tm Γ (V (a * b))) → ∀sim t (pair (fst t) (snd t))
pair-η t n k lt γ γ' γ~ [] rewrite sim-refl t _ _ lt _ _ γ~ [] with eval t k γ' []
... | nothing         = refl
... | just (pair _ _) = refl

-- fst-β and snd-β only work with ground values in the other projection


-- sums
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


-- Fst (let x = t; u)   ~   (let x = t; Fst u)

letV-Fst : (t : Tm Γ (V a))(u : Tm (Γ ▶ V a) (ℂ (α * β))) → ∀sim (Fst (letV t u)) (letV t (Fst u))
letV-Fst t u n k lt γ γ' γ~ as rewrite sim-refl t _ _ lt _ _ γ~ [] with eval t k γ' []
... | nothing = refl
... | just v  = sim-refl u _ _ lt (defV γ v) (defV γ' v) (γ~ , refl) (fst as)

-- Snd (let x = t; u)   ~   (let x = t; Snd u)

letV-Snd : (t : Tm Γ (V a))(u : Tm (Γ ▶ V a) (ℂ (α * β))) → ∀sim (Snd (letV t u)) (letV t (Snd u))
letV-Snd t u n k lt γ γ' γ~ as rewrite sim-refl t _ _ lt _ _ γ~ [] with eval t k γ' []
... | nothing = refl
... | just v  = sim-refl u _ _ lt (defV γ v) (defV γ' v) (γ~ , refl) (snd as)


-- Fst (let x = t; u)   ~   (let x = t; Fst u)

letC-Fst : (t : Tm (Γ ▶ ℂ α) (ℂ α))(u : Tm (Γ ▶ ℂ α) (ℂ (β * γ))) → ∀sim (Fst (letC t u)) (letC t (Fst u))
letC-Fst t u n k lt γ γ' γ~ as = sim-refl u _ _ lt (defC γ t) (defC γ' t) (γ~ , next-sim (sim-refl t k)) (fst as)

-- Snd (let x = t; u)   ~   (let x = t; Snd u)

letC-Snd : (t : Tm (Γ ▶ ℂ α) (ℂ α))(u : Tm (Γ ▶ ℂ α) (ℂ (β * γ))) → ∀sim (Snd (letC t u)) (letC t (Snd u))
letC-Snd t u n k lt γ γ' γ~ as = sim-refl u _ _ lt (defC γ t) (defC γ' t) (γ~ , next-sim (sim-refl t k)) (snd as)


-- Fst (case t of inl x. l; inr x. r)  ~  (case t of inl x. Fst l; inr x. Fst r)

Fst-split : (t : Tm Γ (V (a + b)))(l : Tm (Γ ▶ V a) (ℂ (α * β)))(r : Tm (Γ ▶ V b) (ℂ (α * β)))
              → ∀sim (Fst (split t l r)) (split t (Fst l) (Fst r))
Fst-split t l r n k lt γ γ' γ~ as rewrite sim-refl t _ _ lt _ _ γ~ [] with eval t k γ' []
... | nothing       = refl
... | just (inl v)  = sim-refl l _ _ lt (defV γ v) (defV γ' v) (γ~ , refl) (fst as)
... | just (inr v)  = sim-refl r _ _ lt (defV γ v) (defV γ' v) (γ~ , refl) (fst as)

-- Snd (case t of inl x. l; inr x. r)  ~  (case t of inl x. Snd l; inr x. Snd r)

Snd-split : (t : Tm Γ (V (a + b)))(l : Tm (Γ ▶ V a) (ℂ (α * β)))(r : Tm (Γ ▶ V b) (ℂ (α * β)))
              → ∀sim (Snd (split t l r)) (split t (Snd l) (Snd r))
Snd-split t l r n k lt γ γ' γ~ as rewrite sim-refl t _ _ lt _ _ γ~ [] with eval t k γ' []
... | nothing       = refl
... | just (inl v)  = sim-refl l _ _ lt (defV γ v) (defV γ' v) (γ~ , refl) (snd as)
... | just (inr v)  = sim-refl r _ _ lt (defV γ v) (defV γ' v) (γ~ , refl) (snd as)
