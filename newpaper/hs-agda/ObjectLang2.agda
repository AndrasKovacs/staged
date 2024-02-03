

module ObjectLang2 where

{- Large product, large unit, letrec -}

open import Relation.Binary.PropositionalEquality
  renaming (subst to tr; sym to infix 5 _⁻¹; trans to infixr 4 _■_;
            cong to ap)
  hiding ([_])

open import Data.Product
  renaming (proj₁ to ₁; proj₂ to ₂)
  hiding (_<*>_)

open import Data.Maybe
open import Data.Nat hiding (_+_; _*_)
open import Function

--------------------------------------------------------------------------------

return : ∀ {i}{A : Set i} → A → Maybe A
return = just

infixl 1 _>>_
_>>_ : ∀ {i j}{A : Set i}{B : Set j} → Maybe A → Maybe B → Maybe B
ma >> mb = ma >>= λ _ → mb

infixl 4 _<$>_
_<$>_ : ∀ {i j}{A : Set i}{B : Set j} → (A → B) → Maybe A → Maybe B
f <$> ma = do
  a ← ma
  just (f a)

infixl 4 _<*>_
_<*>_ : ∀ {i j}{A : Set i}{B : Set j} → Maybe (A → B) → Maybe A → Maybe B
mf <*> ma = do
  f ← mf
  a ← ma
  just (f a)

-- Syntax
--------------------------------------------------------------------------------

mutual
  data ValTy : Set where
    one : ValTy
    _+_ : ValTy → ValTy → ValTy
    _*_ : ValTy → ValTy → ValTy
    μ   : Functor → ValTy

  data Functor : Set where
    K   : ValTy → Functor
    Id  : Functor
    _+_ : Functor → Functor → Functor
    _*_ : Functor → Functor → Functor

infixr 5 _+_
infixr 6 _*_

⟦_⟧ : Functor → ValTy → ValTy
⟦ K A   ⟧ _ = A
⟦ Id    ⟧ A = A
⟦ F + G ⟧ A = ⟦ F ⟧ A + ⟦ G ⟧ A
⟦ F * G ⟧ A = ⟦ F ⟧ A * ⟦ G ⟧ A

mutual
  data CompTy : Set where
    one : CompTy
    _*_ : CompTy → CompTy → CompTy
    _⇒_ : ValTy → Ty → CompTy
  infixr 4 _⇒_

  data Ty : Set where
    V : ValTy → Ty
    ℂ : CompTy → Ty

data Con : Set where
  ∙   : Con
  _▶_ : Con → Ty → Con
infixl 3 _▶_

variable
  Γ Δ : Con
  A B C : Ty
  a b c : ValTy
  α β γ : CompTy
  F G : Functor

data Var : Con → Ty → Set where
  zero : Var (Γ ▶ A) A
  suc  : Var Γ A → Var (Γ ▶ B) A

data Tm (Γ : Con) : Ty → Set where
  var    : Var Γ A → Tm Γ A
  let'   : Tm Γ A → Tm (Γ ▶ A) B → Tm Γ B
  letrec : Tm (Γ ▶ ℂ α) (ℂ α) → Tm (Γ ▶ ℂ α) B → Tm Γ B
  lam    : Tm (Γ ▶ V a) B → Tm Γ (ℂ (a ⇒ B))
  app    : Tm Γ (ℂ (a ⇒ B)) → Tm Γ (V a) → Tm Γ B
  Pair   : Tm Γ (ℂ α) → Tm Γ (ℂ β) → Tm Γ (ℂ (α * β))
  Fst    : Tm Γ (ℂ (α * β)) → Tm Γ (ℂ α)
  Snd    : Tm Γ (ℂ (α * β)) → Tm Γ (ℂ β)
  Tt     : Tm Γ (ℂ one)
  pair   : Tm Γ (V a) → Tm Γ (V b) → Tm Γ (V (a * b))
  fst    : Tm Γ (V (a * b)) → Tm Γ (V a)
  snd    : Tm Γ (V (a * b)) → Tm Γ (V b)
  tt     : Tm Γ (V one)
  inl    : Tm Γ (V a) → Tm Γ (V (a + b))
  inr    : Tm Γ (V b) → Tm Γ (V (a + b))
  split  : Tm Γ (V (a + b)) → Tm (Γ ▶ V a) C → Tm (Γ ▶ V b) C → Tm Γ C
  wrap   : Tm Γ (V (⟦ F ⟧ (μ F))) → Tm Γ (V (μ F))
  unwrap : Tm Γ (V (μ F)) → Tm Γ (V (⟦ F ⟧ (μ F)))


-- Fuel-based operational semantics
--------------------------------------------------------------------------------

data Val : ValTy → Set where
  tt     : Val one
  pair   : Val a → Val b → Val (a * b)
  inl    : Val a → Val (a + b)
  inr    : Val b → Val (a + b)
  wrap   : Val (⟦ F ⟧ (μ F)) → Val (μ F)

data Env : Con → Set where
  nil       : Env ∙
  defVal    : Env Γ → Val a → Env (Γ ▶ V a)
  defComp   : Env Γ → Tm Γ (ℂ α) → Env (Γ ▶ ℂ α)
  defRec    : Env Γ → Tm (Γ ▶ ℂ α) (ℂ α) → Env (Γ ▶ ℂ α)

data Args : Ty → ValTy → Set where
  []   : Args (V a) a
  app  : Args B c → Val a → Args (ℂ (a ⇒ B)) c
  fst  : Args (ℂ α) c → Args (ℂ (α * β)) c
  snd  : Args (ℂ β) c → Args (ℂ (α * β)) c

mutual
  evalVar : Var Γ A → ℕ → Env Γ → ∀ {B} → Args A B → Maybe (Val B)
  evalVar zero    k (defVal env v)  [] = return v
  evalVar zero    k (defComp env t) sp = eval t k env sp
  evalVar zero    k (defRec env t)  sp = eval t k (defRec env t) sp
  evalVar (suc x) k (defVal env _)  sp = evalVar x k env sp
  evalVar (suc x) k (defComp env _) sp = evalVar x k env sp
  evalVar (suc x) k (defRec env _)  sp = evalVar x k env sp

  bindLet : Tm Γ A → ℕ → Env Γ → Maybe (Env (Γ ▶ A))
  bindLet {A = V _} t k env = defVal env <$> eval t k env []
  bindLet {A = ℂ _} t k env = return (defComp env t)

  eval : Tm Γ A → (ℕ → Env Γ → ∀ {B} → Args A B → Maybe (Val B))
  eval t             zero    env args       = nothing
  eval (var x)       (suc k) env args       = evalVar x k env args
  eval (let' t u)    (suc k) env args       = do env ← bindLet t k env
                                                 eval u k env args
  eval (letrec t u)  (suc k) env args       = eval u k (defRec env t) args
  eval (lam t)       (suc k) env (app as v) = eval t k (defVal env v) as
  eval (app t u)     (suc k) env args       = do v ← eval u k env []
                                                 eval t k env (app args v)
  eval (Pair t u)    (suc k) env (fst args) = eval t k env args
  eval (Pair t u)    (suc k) env (snd args) = eval u k env args
  eval (Fst t)       (suc k) env args       = eval t k env (fst args)
  eval (Snd t)       (suc k) env args       = eval t k env (snd args)
  eval (pair t u)    (suc k) env []         = pair <$> eval t k env [] <*> eval u k env []
  eval (fst t)       (suc k) env []         = do pair v _ ← eval t k env []
                                                 return v
  eval (snd t)       (suc k) env []         = do pair _ v ← eval t k env []
                                                 return v
  eval tt            (suc k) env []         = return tt
  eval (inl t)       (suc k) env []         = inl <$> eval t k env []
  eval (inr t)       (suc k) env []         = inr <$> eval t k env []
  eval (split t l r) (suc k) env args       = do v ← eval t k env []
                                                 case v of λ where
                                                   (inl v) → eval l k (defVal env v) args
                                                   (inr v) → eval r k (defVal env v) args
  eval (wrap t)      (suc k) env []         = wrap <$> eval t k env []
  eval (unwrap t)    (suc k) env []         = do wrap v ← eval t k env []
                                                 return v

--------------------------------------------------------------------------------

{-
Is our language (+closures) just CBPV with the possibility to let-bind anything?


In the paper, we don't present computational products in the beginning, only
when we get to mutual letrec in pull streams.


- Lemma: ∀ t k env args v. (eval t k env args = just v) → (eval t (suc k) env args = just v)
  By induction on terms.

-- - Definition:
--   t is *total* if ∀ env args. ∃ k v. eval t k env args = just v

-- Definition of totality is also circular in our setting!
-- Let's just not mention it.

-- Beta rule for variables is enough for saturation translation.

- Definition
  t ≈ u   if   ∀ env args. (∃ k k' v. (eval t k env args = just v) × (eval u k' env args = just v))
                         ∨ (∀ k. eval t k env agrs = nothing × eval u k env args = nothing)




-}
