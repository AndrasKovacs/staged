
module ObjectInterpreter where

open import Lib
open import ObjectSyntax

mutual
  data Val : ValTy → Set where
    tt     : Val one
    pair   : Val a → Val b → Val (a * b)
    inl    : Val a → Val (a + b)
    inr    : Val b → Val (a + b)
    wrap   : Val (⟦ F ⟧ (μ F)) → Val (μ F)

  data Env : Con → Set where
    nil    : Env ∙
    defV : Env Γ → Val a → Env (Γ ▶ V a)
    defC : Env Γ → Tm (Γ ▶ ℂ α) (ℂ α) → Env (Γ ▶ ℂ α)

data Args : Ty → ValTy → Set where
  []   : Args (V a) a
  app  : Args B c → Val a → Args (ℂ (a ⇒ B)) c

lookupV : Var Γ (V a) → Env Γ → Val a
lookupV zero    (defV γ v) = v
lookupV (suc x) (defV γ _) = lookupV x γ
lookupV (suc x) (defC γ _) = lookupV x γ

mutual
  call : Var Γ (ℂ α) → ℕ → Env Γ → Args (ℂ α) b → Maybe (Val b)
  call zero    zero    (defC _ _) _  = nothing
  call zero    (suc k) (defC γ t) as = eval t k (defC γ t) as
  call (suc x) k       (defV γ _) as = call x k γ as
  call (suc x) k       (defC γ _) as = call x k γ as

  eval : Tm Γ A → ℕ → Env Γ → Args A b → Maybe (Val b)
  eval (varV x)      k γ []         = return (lookupV x γ)
  eval (varC x)      k γ as         = call x k γ as
  eval (letV t u)    k γ as         = do v ← eval t k γ []; eval u k (defV γ v) as
  eval (letC t u)    k γ as         = eval u k (defC γ t) as
  eval (lam t)       k γ (app as v) = eval t k (defV γ v) as
  eval (app t u)     k γ as         = do v ← eval u k γ []; eval t k γ (app as v)
  eval (pair t u)    k γ []         = pair <$> eval t k γ [] <*> eval u k γ []
  eval (fst t)       k γ []         = do pair v _ ← eval t k γ []; return v
  eval (snd t)       k γ []         = do pair _ v ← eval t k γ []; return v
  eval tt            k γ []         = return tt
  eval (inl t)       k γ []         = inl <$> eval t k γ []
  eval (inr t)       k γ []         = inr <$> eval t k γ []
  eval (split t l r) k γ as         = do v ← eval t k γ []
                                         case v of λ where
                                           (inl v) → eval l k (defV γ v) as
                                           (inr v) → eval r k (defV γ v) as
  eval (wrap t)      k γ []         = wrap <$> eval t k γ []
  eval (unwrap t)    k γ []         = do wrap v ← eval t k γ []; return v
