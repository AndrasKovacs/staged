
module Renaming where

{-
Formalization of variable renaming for the object theory. This is required to
prove some basic facts about bisimilarity, and also to validate the β rule for
applying functions to variables.
-}

open import Lib
open import Syntax
open import Interpreter


Ren : Con → Con → Set -- renaming
Ren Γ Δ = ∀ {A} → Var Δ A → Var Γ A

wk : Ren Γ Δ → Ren (Γ ▶ A) Δ
wk σ = suc ∘ σ

idr : Ren Γ Γ -- identity renaming
idr = id

renameTop : Ren Γ Δ → Var Γ A → Ren Γ (Δ ▶ A)
renameTop σ x zero    = x
renameTop σ x (suc y) = σ y

keep : Ren Γ Δ → Ren (Γ ▶ A) (Δ ▶ A)
keep σ = renameTop (wk σ) zero

_[_] : Tm Γ A → Ren Δ Γ → Tm Δ A
varC x      [ σ ] = varC (σ x)
varV x      [ σ ] = varV (σ x)
letV t u    [ σ ] = letV (t [ σ ]) (u [ keep σ ])
letC t u    [ σ ] = letC (t [ keep σ ]) (u [ keep σ ])
lam t       [ σ ] = lam (t [ keep σ ])
app t u     [ σ ] = app (t [ σ ]) (u [ σ ])
pair t u    [ σ ] = pair (t [ σ ]) (u [ σ ])
fst t       [ σ ] = fst (t [ σ ])
snd t       [ σ ] = snd (t [ σ ])
tt          [ σ ] = tt
inl t       [ σ ] = inl (t [ σ ])
inr t       [ σ ] = inr (t [ σ ])
split t l r [ σ ] = split (t [ σ ]) (l [ keep σ ]) (r [ keep σ ])
wrap t      [ σ ] = wrap (t [ σ ])
unwrap t    [ σ ] = unwrap (t [ σ ])
Pair t u    [ σ ] = Pair (t [ σ ]) (u [ σ ])
Fst t       [ σ ] = Fst (t [ σ ])
Snd t       [ σ ] = Snd (t [ σ ])
Tt          [ σ ] = Tt

-- Action of evaluation on renamed terms
--------------------------------------------------------------------------------

-- Relation that tracks the effect of renaming on runtime environments.
-- The intuitive meaning of RenEnv is essentially given by the type of ren-eval.
data RenEnv : ∀ {Γ Δ} → (σ : Ren Γ Δ) → Env Γ → Env Δ → Set where
  identity   : ∀ {γ} → RenEnv (idr {Γ}) γ γ
  renameTopV : ∀ {σ : Ren Γ Δ}{γ γ'}(x : Var Γ (V a)) → RenEnv σ γ γ' → RenEnv (renameTop σ x) γ (defV γ' (lookupV x γ))
  dropV      : ∀ {σ : Ren Γ Δ}{γ γ'}{v : Val a} → RenEnv σ γ γ' → RenEnv (wk σ) (defV γ v) γ'
  keepC      : ∀ {σ : Ren Γ Δ}{γ γ'}{t : Tm (Δ ▶ ℂ α) (ℂ α)} → RenEnv σ γ γ' → RenEnv (keep σ) (defC γ (t [ keep σ ])) (defC γ' t)

keepV : ∀ {σ : Ren Γ Δ}{γ γ'}{v : Val a} → RenEnv σ γ γ' → RenEnv (keep σ) (defV γ v) (defV γ' v)
keepV p = renameTopV zero (dropV p)

wkV-idr : ∀ (γ : Env Γ) (v : Val a) → RenEnv (wk idr) (defV γ v) γ
wkV-idr γ v = dropV identity

ren-lookupV : ∀ {σ : Ren Δ Γ}(x : Var Γ (V a)) {γ γ'} (p : RenEnv σ γ γ') → lookupV (σ x) γ ≡ lookupV x γ'
ren-lookupV x       identity         = refl
ren-lookupV zero    (renameTopV y p) = refl
ren-lookupV (suc x) (renameTopV y p) = ren-lookupV x p
ren-lookupV x       (dropV p)        = ren-lookupV x p
ren-lookupV (suc x) (keepC p)        = ren-lookupV x p

mutual
  ren-call : ∀ {σ : Ren Δ Γ}(x : Var Γ (ℂ α)) k {γ γ'} (p : RenEnv σ γ γ'){b} as → call {b = b}(σ x) k γ as ≡ call x k γ' as
  ren-call x       k       identity                  as = refl
  ren-call (suc x) k       (renameTopV y p)          as = ren-call x k  p as
  ren-call x       k       (dropV p)                 as = ren-call x k  p as
  ren-call zero    zero    (keepC p)                 as = refl
  ren-call zero    (suc k) (keepC {σ = σ} {t = t} p) as = ren-eval (keep σ) t k (keepC p) as
  ren-call (suc x) k       (keepC p)                 as = ren-call x k p as

  ren-eval : ∀ (σ : Ren Δ Γ)(t : Tm Γ A) k {γ γ'} (p : RenEnv σ γ γ'){b} as → eval {b = b}(t [ σ ]) k γ as ≡ eval t k γ' as
  ren-eval σ (varC x) k p as = ren-call x k p as

  ren-eval σ (varV x) k p [] = ap just (ren-lookupV x p)

  ren-eval σ (letV t u) k {γ' = γ'} p as rewrite ren-eval σ t k p [] with eval t k γ' []
  ... | nothing = refl
  ... | just v  = ren-eval (keep σ) u k (keepV p) as

  ren-eval σ (letC t u) k p as = ren-eval (keep σ) u k (keepC p) as
  ren-eval σ (lam t) k p (app as v) = ren-eval (keep σ) t k (keepV p) as

  ren-eval σ (app t u) k {γ' = γ'} p as rewrite ren-eval σ u k p [] with eval u k γ' []
  ... | nothing = refl
  ... | just v  = ren-eval σ t k p (app as v)

  ren-eval σ (pair t u) k p [] =
    ap (λ x y → pair <$> x <*> y) (ren-eval σ t k p []) ⊗ ren-eval σ u k p []

  ren-eval σ (fst t) k p [] rewrite ren-eval σ t k p [] = refl
  ren-eval σ (snd t) k p [] rewrite ren-eval σ t k p [] = refl

  ren-eval σ tt k p [] = refl
  ren-eval σ (inl t) k p [] = ap (inl <$>_) (ren-eval σ t k p [])
  ren-eval σ (inr t) k p [] = ap (inr <$>_) (ren-eval σ t k p [])
  ren-eval σ (split t l r) k {γ' = γ'} p as rewrite ren-eval σ t k p [] with eval t k γ' []
  ... | nothing      = refl
  ... | just (inl v) = ren-eval (keep σ) l k (keepV p) as
  ... | just (inr v) = ren-eval (keep σ) r k (keepV p) as

  ren-eval σ (wrap t) k p [] = ap (wrap <$>_) (ren-eval σ t k p [])
  ren-eval σ (unwrap t) k p [] rewrite ren-eval σ t k p [] = refl

  ren-eval σ (Pair t u) k p (fst as) = ren-eval σ t k p as
  ren-eval σ (Pair t u) k p (snd as) = ren-eval σ u k p as
  ren-eval σ (Fst t) k p as = ren-eval σ t k p (fst as)
  ren-eval σ (Snd t) k p as = ren-eval σ t k p (snd as)
  ren-eval σ Tt k p ()
