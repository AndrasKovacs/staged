
module Bisimilarity where

-- Strong bisimilarity of terms.

open import Lib
open import Syntax
open import Interpreter
open import Renaming

--------------------------------------------------------------------------------

mutual
  -- The definition is obviously terminating, but requires well-founded recursion here.
  -- I skip the well-founded recursion, because many things depend on sim, so it would be
  -- noisy and probably expensive in type checking.
  {-# TERMINATING #-}
  sim : Tm Γ A → Tm Γ A → ℕ → Set
  sim t t' n  = ∀ k → k ≤′ n → ∀ γ γ' (γ~ : simₑ γ γ' k) {b} as → eval {b = b} t k γ as ≡ eval t' k γ' as

  sim-later : Tm Γ A → Tm Γ A → ℕ → Set
  sim-later t t' zero    = ⊤
  sim-later t t' (suc n) = sim t t' n

  simₑ : Env Γ → Env Γ → ℕ → Set
  simₑ nil        nil          n = ⊤
  simₑ (defV γ v) (defV γ' v') n = simₑ γ γ' n × v ≡ v'
  simₑ (defC γ t) (defC γ' t') n = simₑ γ γ' n × sim-later t t' n

∀sim : Tm Γ A → Tm Γ A → Set
∀sim t t' = ∀ n → sim t t' n

∀simₑ : Env Γ → Env Γ → Set
∀simₑ γ γ' = ∀ n → simₑ γ γ' n


-- restriction (weakening)
--------------------------------------------------------------------------------

wk-sim : ∀ (t t' : Tm Γ A){n} → sim t t' (suc n) → sim t t' n
wk-sim t t' {n} t~ k lt γ γ' γ~ as = t~ k (≤′-step lt) _ _ γ~ as

wk≤-sim : ∀ (t t' : Tm Γ A){n m} → n ≤′ m → sim t t' m → sim t t' n
wk≤-sim _ _  ≤′-refl      t~ = t~
wk≤-sim t t' (≤′-step lt) t~ = wk≤-sim t t' lt (wk-sim t t' t~)

wk-sim-later : ∀ (t t' : Tm Γ A){n} → sim-later t t' (suc n) → sim-later t t' n
wk-sim-later t t' {zero} t~  = tt
wk-sim-later t t' {suc n} t~ = wk-sim t t' t~

wk-simₑ : ∀ (γ γ' : Env Γ){n} → simₑ γ γ' (suc n) → simₑ γ γ' n
wk-simₑ nil        nil          γ~        = tt
wk-simₑ (defV γ v) (defV γ' v') (γ~ , p)  = wk-simₑ γ γ' γ~ , p
wk-simₑ (defC γ t) (defC γ' t') (γ~ , t~) = wk-simₑ γ γ' γ~ , wk-sim-later t t' t~

wk≤-simₑ : ∀ (γ γ' : Env Γ){n m} → n ≤′ m →  simₑ γ γ' m → simₑ γ γ' n
wk≤-simₑ γ γ' ≤′-refl     γ~ = γ~
wk≤-simₑ γ γ' (≤′-step p) γ~ = wk≤-simₑ _ _ p (wk-simₑ γ γ' γ~)

next-sim : ∀ {t t' : Tm Γ A}{k} → sim t t' k → sim-later t t' k
next-sim {t = t}{t'}{zero}   p = tt
next-sim {t = t}{t'} {suc k} p = wk-sim t t' p


-- reflexive
--------------------------------------------------------------------------------

lookupV-sim-refl : ∀ (x : Var Γ (V a)) {γ γ' k} (γ~ : simₑ γ γ' k) → lookupV x γ ≡ lookupV x γ'
lookupV-sim-refl zero    {defV γ v} {defV γ' v'} γ~ = ₂ γ~
lookupV-sim-refl (suc x) {defV γ v} {defV γ' v'} γ~ = lookupV-sim-refl x (₁ γ~)
lookupV-sim-refl (suc x) {defC γ t} {defC γ' t'} γ~ = lookupV-sim-refl x (₁ γ~)

sim-call-refl : ∀ (x : Var Γ (ℂ α)) {γ γ' k} (γ~ : simₑ γ γ' k){b}(as : Args _ b) → call x k γ as ≡ call x k γ' as
sim-call-refl zero {defC γ t} {defC γ' t'} {zero}  (γ~ , t~) as = refl
sim-call-refl zero {defC γ t} {defC γ' t'} {suc k} (γ~ , t~) as =
  t~ k ≤′-refl (defC γ t) (defC γ' t') ((wk-simₑ _ _ γ~) , next-sim t~) as
sim-call-refl (suc x) {defV γ v} {defV γ' v'} (γ~ , _) as = sim-call-refl x γ~ as
sim-call-refl (suc x) {defC γ t} {defC γ' t'} (γ~ , _) as = sim-call-refl x γ~ as

sim-refl : ∀ (t : Tm Γ A) → ∀sim t t
sim-refl (varC x)      n k lt γ γ' γ~ as = sim-call-refl x γ~ as
sim-refl (varV x)      n k lt γ γ' γ~ [] = ap just (lookupV-sim-refl x γ~)

sim-refl (letV t u)    n k lt γ γ' γ~ as rewrite sim-refl t n k lt γ γ' γ~ [] with eval t k γ' []
... | nothing = refl
... | just v  = sim-refl u n k lt _ _ (γ~ , refl) as

sim-refl (letC t u)    n k lt γ γ' γ~ as = sim-refl u n k lt _ _ (γ~ , next-sim (sim-refl t k)) as

sim-refl (lam t) n k lt γ γ' γ~ (app as v) = sim-refl t n k lt _ _ (γ~ , refl) as

sim-refl (app t u)     n k lt γ γ' γ~ as rewrite sim-refl u n k lt _ _ γ~ [] with eval u k γ' []
... | nothing = refl
... | just v  = sim-refl t n k lt _ _ γ~ (app as v)

sim-refl (pair t u)    n k lt γ γ' γ~ [] =
  ap (λ x y → pair <$> x <*> y) (sim-refl t n k lt _ _ γ~ []) ⊗ sim-refl u n k lt _ _ γ~ []

sim-refl (fst t)       n k lt γ γ' γ~ [] rewrite sim-refl t n k lt _ _ γ~ [] = refl
sim-refl (snd t)       n k lt γ γ' γ~ [] rewrite sim-refl t n k lt _ _ γ~ [] = refl

sim-refl tt            n k lt γ γ' γ~ [] = refl
sim-refl (inl t)       n k lt γ γ' γ~ [] = ap (inl <$>_) (sim-refl t n k lt _ _ γ~ [])
sim-refl (inr t)       n k lt γ γ' γ~ [] = ap (inr <$>_) (sim-refl t n k lt _ _ γ~ [])
sim-refl (split t l r) n k lt γ γ' γ~ as rewrite sim-refl t n k lt _ _ γ~ [] with eval t k γ' []
... | nothing      = refl
... | just (inl v) = sim-refl l n k lt _ _ (γ~ , refl) as
... | just (inr v) = sim-refl r n k lt _ _ (γ~ , refl) as
sim-refl (wrap t)      n k lt γ γ' γ~ [] = ap (wrap <$>_) (sim-refl t n k lt _ _ γ~ [])
sim-refl (unwrap t)    n k lt γ γ' γ~ [] rewrite sim-refl t n k lt _ _ γ~ [] = refl

simₑ-refl : ∀ {Γ}{γ : Env Γ} → ∀simₑ γ γ
simₑ-refl {γ = nil}      n = tt
simₑ-refl {γ = defV γ v} n = (simₑ-refl n) , refl
simₑ-refl {γ = defC γ t} n = (simₑ-refl n) , (next-sim (sim-refl t n))

-- symmetric
--------------------------------------------------------------------------------

simₑ-sym-lem :
   ∀ n
   (hyp : ∀ k → k <′ n → ∀ {Γ A}{t t' : Tm Γ A} → sim t t' k → sim t' t k) →
   ∀ {Γ}(γ γ' : Env Γ) → simₑ γ γ' n → simₑ γ' γ n
simₑ-sym-lem n hyp nil nil γ~ = tt
simₑ-sym-lem n hyp (defV γ v) (defV γ' v') (γ~ , v~) =
  simₑ-sym-lem n hyp γ γ' γ~ , v~ ⁻¹
simₑ-sym-lem zero hyp (defC γ t) (defC γ' t') (γ~ , t~) =
  (simₑ-sym-lem zero hyp γ γ' γ~) , tt
simₑ-sym-lem (suc n) hyp (defC γ t) (defC γ' t') (γ~ , t~) =
  (simₑ-sym-lem (suc n) hyp γ γ' γ~) , hyp n ≤′-refl {t = t}{t'} t~

-- Here I do use well-founded recursion though
sim-sym : ∀ {t t' : Tm Γ A} → ∀sim t t' → ∀sim t' t
sim-sym {Γ} {A} {t} {t'} t~ n = <′-rec (λ n → ∀ {Γ A}{t t' : Tm Γ A} → sim t t' n → sim t' t n)
  (λ n hyp {Γ}{A} {t}{t'} t~ k lt γ γ' γ~ as →
    t~ k lt γ' γ (simₑ-sym-lem k (λ k' lt' {Γ}{A}{l}{r} lr → hyp k' (≤′-trans lt' lt) {t = l}{r} lr) γ γ' γ~) as ⁻¹
    )
  n {Γ}{A}{t}{t'} (t~ n)


-- transitive
--------------------------------------------------------------------------------

sim-trans : ∀ {t t' t'' : Tm Γ A} → ∀sim t t' → ∀sim t' t'' → ∀sim t t''
sim-trans p q n k lt γ γ' γ~ as = p n k lt γ γ' γ~ as ◼ q n k lt γ' γ' (simₑ-refl k) as


-- congruent
--------------------------------------------------------------------------------

sim-varC : ∀ {x x' : Var Γ (ℂ α)} → x ≡ x' → ∀sim (varC x) (varC x')
sim-varC {x = x} refl = sim-refl (varC x)

sim-varV : ∀ {x x' : Var Γ (V a)} → x ≡ x' → ∀sim (varV x) (varV x')
sim-varV {x = x} refl = sim-refl (varV x)

sim-letV : ∀ {t t' : Tm Γ (V a)}{u u' : Tm (Γ ▶ V a) B} → ∀sim t t' → ∀sim u u' → ∀sim (letV t u) (letV t' u')
sim-letV {t = t} {t'} {u} {u'} t~ u~ n k lt γ γ' γ~ as rewrite t~ _ _ lt _ _ γ~ [] with eval t' k γ' []
... | nothing = refl
... | just v  = u~ _ _ lt (defV γ v) (defV γ' v) (γ~ , refl) as

sim-letC : ∀ {t t' : Tm (Γ ▶ ℂ α) (ℂ α)}{u u' : Tm (Γ ▶ ℂ α) B} → ∀sim t t' → ∀sim u u' → ∀sim (letC t u) (letC t' u')
sim-letC {t = t} {t'} {u} {u'} t~ u~ n k lt γ γ' γ~ as = u~ _ _ lt (defC γ t) (defC γ' t') (γ~ , (next-sim (t~ k))) as

sim-lam : ∀ {t t' : Tm (Γ ▶ V a) B} → ∀sim t t' → ∀sim (lam t) (lam t')
sim-lam {t = t} {t'} t~ n k lt γ γ' γ~ (app as v) = t~ _ _ lt (defV γ v) (defV γ' v) (γ~ , refl) as

sim-app : ∀ {t t' : Tm Γ (ℂ (a ⇒ B))}{u u' : Tm Γ (V a)} → ∀sim t t' → ∀sim u u' → ∀sim (app t u) (app t' u')
sim-app {t = t} {t'} {u} {u'} t~ u~ n k lt γ γ' γ~ as rewrite u~ _ _ lt _ _ γ~ [] with eval u' k γ' []
... | nothing = refl
... | just v  = t~ _ _ lt _ _ γ~ (app as v)

sim-pair : ∀ {t t' : Tm Γ (V a)}{u u' : Tm Γ (V b)} → ∀sim t t' → ∀sim u u' → ∀sim (pair t u) (pair t' u')
sim-pair {t = t} {t'} {u} {u'} t~ u~ n k lt γ γ' γ~ [] =
 ap (λ x y → pair <$> x <*> y) (t~ _ _ lt _ _ γ~ []) ⊗ u~ _ _ lt _ _ γ~ []

sim-fst : ∀ {t t' : Tm Γ (V (a * b))} → ∀sim t t' → ∀sim (fst t) (fst t')
sim-fst {t = t} {t'} t~ n k lt γ γ' γ~ [] rewrite t~ _ _ lt _ _ γ~ [] = refl

sim-snd : ∀ {t t' : Tm Γ (V (a * b))} → ∀sim t t' → ∀sim (snd t) (snd t')
sim-snd {t = t} {t'} t~ n k lt γ γ' γ~ [] rewrite t~ _ _ lt _ _ γ~ [] = refl

sim-tt : ∀sim {Γ} tt tt
sim-tt _ _ _ _ _ _ [] = refl

sim-inl : ∀ {t t' : Tm Γ (V a)} → ∀sim t t' → ∀sim (inl {b = b} t) (inl t')
sim-inl {t = t} {t'} t~ n k lt γ γ' γ~ [] rewrite t~ _ _ lt _ _ γ~ [] = refl

sim-inr : ∀ {t t' : Tm Γ (V b)} → ∀sim t t' → ∀sim (inr {a = a} t) (inr t')
sim-inr {t = t} {t'} t~ n k lt γ γ' γ~ [] rewrite t~ _ _ lt _ _ γ~ [] = refl

sim-split :
  ∀ {t t' : Tm Γ (V (a + b))}{l l' : Tm (Γ ▶ V a) B}{r r' : Tm (Γ ▶ V b) B}
  → ∀sim t t'
  → ∀sim l l'
  → ∀sim r r'
  → ∀sim (split t l r) (split t' l' r')
sim-split {t = t} {t'} {l} {l'}{r}{r'} t~ l~ r~ n k lt γ γ' γ~ as rewrite t~ _ _ lt _ _ γ~ [] with eval t' k γ' []
... | nothing      = refl
... | just (inl v) = l~ _ _ lt (defV γ v) (defV γ' v) (γ~ , refl) as
... | just (inr v) = r~ _ _ lt (defV γ v) (defV γ' v) (γ~ , refl) as

sim-wrap : ∀ {t t' : Tm Γ (V (⟦ F ⟧ (μ F)))} → ∀sim t t' → ∀sim (wrap {F = F} t) (wrap t')
sim-wrap {t = t} {t'} t~ n k lt γ γ' γ~ [] rewrite t~ _ _ lt _ _ γ~ [] = refl

sim-unwrap : ∀ {t t' : Tm Γ (V (μ F))} → ∀sim t t' → ∀sim (unwrap {F = F} t) (unwrap t')
sim-unwrap {t = t} {t'} t~ n k lt γ γ' γ~ [] rewrite t~ _ _ lt _ _ γ~ [] = refl

--------------------------------------------------------------------------------
