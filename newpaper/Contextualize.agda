
open import Data.Nat
open import Data.Fin

-- higher-order model of pure LC, without equations
record HOM : Set₁ where
  field
    Tm  : Set
    lam : (Tm → Tm) → Tm
    app : Tm → Tm → Tm

-- Contextual first-order models. "Contextual" means that contexts and variables
-- are fixed as inductively generated. A more general definition would allow
-- contexts and vars to vary across models.
record CxtFOM : Set₁ where
  field
    Tm  : ℕ → Set
    var : ∀ {n} → Fin n → Tm n
    app : ∀ {n} → Tm n → Tm n → Tm n
    lam : ∀ {n} → Tm (suc n) → Tm n

ext : ∀ {A : Set}{n} → (Fin n → A) → A → (Fin (suc n) → A)
ext γ a zero    = a
ext γ a (suc x) = γ x

contextualize : HOM → CxtFOM
contextualize M = record {
   Tm  = λ n → (Fin n → M.Tm) → M.Tm;
   var = λ x γ → γ x;
   app = λ t u γ → M.app (t γ) (u γ);
   lam = λ t γ → M.lam (λ u → t (ext γ u))
   }
   where module M = HOM M

------------------------------------------------------------

data Tm (n : ℕ) : Set where
  var : Fin n → Tm n
  app : Tm n → Tm n → Tm n
  lam : Tm (suc n) → Tm n

module Eval (M : CxtFOM) where
  module M = CxtFOM M
  eval : ∀ {n} → Tm n → M.Tm n
  eval (var x)   = M.var x
  eval (app t u) = M.app (eval t) (eval u)
  eval (lam t)   = M.lam (eval t)

module Eval' (M : HOM) where
  eval' : ∀ {n} → Tm n → (Fin n → HOM.Tm M) → HOM.Tm M
  eval' = Eval.eval (contextualize M)
