

{-# OPTIONS --guarded --cubical #-}

module ObjectInterpreter4 where

{- Extending the language with large product and unit types -}

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

data Ty : Set where
  val : ValTy → Ty
  _⇒_ : ValTy → Ty → Ty
infixr 4 _⇒_

data Con : Set where
  ∙   : Con
  _▶_ : Con → Ty → Con
infixl 3 _▶_

variable
  Γ Δ : Con
  A B C : Ty
  a b c : ValTy
  F G : Functor

data Var : Con → Ty → Set where
  zero : Var (Γ ▶ A) A
  suc  : Var Γ A → Var (Γ ▶ B) A

data Tm (Γ : Con) : Ty → Set where
  var    : Var Γ A → Tm Γ A
  let'   : Tm Γ A → Tm (Γ ▶ A) B → Tm Γ B
  fix    : Tm (Γ ▶ (a ⇒ B)) (a ⇒ B) → Tm Γ (a ⇒ B)
  lam    : Tm (Γ ▶ val a) B → Tm Γ (a ⇒ B)
  app    : Tm Γ (a ⇒ B) → Tm Γ (val a) → Tm Γ B
  pair   : Tm Γ (val a) → Tm Γ (val b) → Tm Γ (val (a * b))
  fst    : Tm Γ (val (a * b)) → Tm Γ (val a)
  snd    : Tm Γ (val (a * b)) → Tm Γ (val b)
  tt     : Tm Γ (val one)
  inl    : Tm Γ (val a) → Tm Γ (val (a + b))
  inr    : Tm Γ (val b) → Tm Γ (val (a + b))
  split  : Tm Γ (val (a + b)) → Tm (Γ ▶ val a) C → Tm (Γ ▶ val b) C → Tm Γ C
  wrap   : Tm Γ (val (⟦ F ⟧ (μ F))) → Tm Γ (val (μ F))
  unwrap : Tm Γ (val (μ F)) → Tm Γ (val (⟦ F ⟧ (μ F)))


-- Fuel-based operational semantics
--------------------------------------------------------------------------------

data Val : ValTy → Set where
  tt     : Val one
  pair   : Val a → Val b → Val (a * b)
  inl    : Val a → Val (a + b)
  inr    : Val b → Val (a + b)
  wrap   : Val (⟦ F ⟧ (μ F)) → Val (μ F)

data Env : Con → Set where
  nil    : Env ∙
  defVal : Env Γ → Val a → Env (Γ ▶ val a)
  defFun : Env Γ → Tm Γ (a ⇒ B) → Env (Γ ▶ (a ⇒ B))

data Spine : Ty → ValTy → Set where
  []   : Spine (val a) a
  _∷_  : Val a → Spine B c → Spine (a ⇒ B) c
infixr 4 _∷_

-- mutual
--   evalVar : Var Γ A → ℕ → Env Γ → ∀ {B} → Spine A B → Maybe (Val B)
--   evalVar zero    k (defVal env v) [] = return v
--   evalVar zero    k (defFun env t) sp = eval t k env sp
--   evalVar (suc x) k (defVal env _) sp = evalVar x k env sp
--   evalVar (suc x) k (defFun env _) sp = evalVar x k env sp

--   bindLet : Tm Γ A → ℕ → Env Γ → Maybe (Env (Γ ▶ A))
--   bindLet {A = val _} t k env = defVal env <$> eval t k env []
--   bindLet {A = _ ⇒ _} t k env = return (defFun env t)

--   eval : Tm Γ A → (ℕ → Env Γ → ∀ {B} → Spine A B → Maybe (Val B))
--   eval t             zero    env sp       = nothing
--   eval (var x)       (suc k) env sp       = evalVar x k env sp
--   eval (let' t u)    (suc k) env sp       = do env ← bindLet t k env
--                                                eval u k env sp
--   eval (fix t)       (suc k) env sp       = eval t k (defFun env (fix t)) sp
--   eval (lam t)       (suc k) env (v ∷ sp) = eval t k (defVal env v) sp
--   eval (app t u)     (suc k) env sp       = do v ← eval u k env []
--                                                eval t k env (v ∷ sp)
--   eval (pair t u)    (suc k) env []       = pair <$> eval t k env [] <*> eval u k env []
--   eval (fst t)       (suc k) env []       = do pair v _ ← eval t k env []
--                                                return v
--   eval (snd t)       (suc k) env []       = do pair _ v ← eval t k env []
--                                                return v
--   eval tt            (suc k) env []       = return tt
--   eval (inl t)       (suc k) env []       = inl <$> eval t k env []
--   eval (inr t)       (suc k) env []       = inr <$> eval t k env []
--   eval (split t l r) (suc k) env sp       = do v ← eval t k env []
--                                                case v of λ where
--                                                  (inl v) → eval l k (defVal env v) sp
--                                                  (inr v) → eval r k (defVal env v) sp
--   eval (wrap t)      (suc k) env []       = wrap <$> eval t k env []
--   eval (unwrap t)    (suc k) env []       = do wrap v ← eval t k env []
--                                                return v

--------------------------------------------------------------------------------

{-

TODO: semantics should be
- guarded recursive
- use (Spine A B → ▶ Val) for semantics of functions
  this should be nice, compositional and sound w.r.t. all substitutions
  define program equivalence as equality in this model

- define the closure-free evaluator as an alternative version.
  show that on closed terms it coincides with the "more semantic" evaluator.

- from the closure-free eval we can derive a closure-free abstract machine.
-}
