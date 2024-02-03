
module ReferenceInterpreter where

open import Relation.Binary.PropositionalEquality
  renaming (subst to tr; sym to infix 5 _⁻¹; trans to infixr 4 _■_;
            cong to ap)
  hiding ([_])

open import Data.Product
  renaming (proj₁ to ₁; proj₂ to ₂)

open import Function

mutual
  data ValTy : Set where
    one : ValTy                  -- unit
    _+_ : ValTy → ValTy → ValTy  -- sum
    _*_ : ValTy → ValTy → ValTy  -- product
    μ   : Functor → ValTy        -- fixpoint of functor

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

data Var : Con → Ty → Set where
  zero : ∀ {Γ A} → Var (Γ ▶ A) A
  suc  : ∀ {Γ A B} → Var Γ A → Var (Γ ▶ B) A

data Tm (Γ : Con) : Ty → Set where
  var    : ∀ {A} → Var Γ A → Tm Γ A
  let'   : ∀ {A B} → Tm Γ A → Tm (Γ ▶ A) B → Tm Γ B
  fix    : ∀ {A B} → Tm (Γ ▶ (A ⇒ B)) (A ⇒ B) → Tm Γ (A ⇒ B)
  lam    : ∀ {A B} → Tm (Γ ▶ val A) B → Tm Γ (A ⇒ B)
  app    : ∀ {A B} → Tm Γ (A ⇒ B) → Tm Γ (val A) → Tm Γ B
  pair   : ∀ {A B} → Tm Γ (val A) → Tm Γ (val B) → Tm Γ (val (A * B))
  fst    : ∀ {A B} → Tm Γ (val (A * B)) → Tm Γ (val A)
  snd    : ∀ {A B} → Tm Γ (val (A * B)) → Tm Γ (val B)
  tt     : Tm Γ (val one)
  inl    : ∀ {A B} → Tm Γ (val A) → Tm Γ (val (A + B))
  inr    : ∀ {A B} → Tm Γ (val B) → Tm Γ (val (A + B))
  split  : ∀ {A B C} → Tm Γ (val (A + B)) → Tm (Γ ▶ val A) C → Tm (Γ ▶ val B) C → Tm Γ C
  wrap   : ∀ {F} → Tm Γ (val (⟦ F ⟧ (μ F))) → Tm Γ (val (μ F))
  unwrap : ∀ {F} → Tm Γ (val (μ F)) → Tm Γ (val (⟦ F ⟧ (μ F)))

--------------------------------------------------------------------------------

data Val : ValTy → Set where
  tt     : Val one
  pair   : ∀ {A B} → Val A → Val B → Val (A * B)
  inl    : ∀ {A B} → Val A → Val (A + B)
  inr    : ∀ {A B} → Val B → Val (A + B)
  wrap   : ∀ {F} → Val (⟦ F ⟧ (μ F)) → Val (μ F)

data Env : Con → Set where
  nil    : Env ∙
  defVal : ∀ {Γ A} → Env Γ → Val A → Env (Γ ▶ val A)
  defFun : ∀ {Γ A B} → Env Γ → Tm Γ (A ⇒ B) → Env (Γ ▶ (A ⇒ B))

data Spine : Ty → ValTy → Set where
  []   : ∀ {A} → Spine (val A) A
  _∷_  : ∀ {A B C} → Val A → Spine B C → Spine (A ⇒ B) C
infixr 4 _∷_

mutual
  evalVar : ∀ {Γ A} → Var Γ A → (∀ {B} → Env Γ → Spine A B → Val B)
  evalVar zero    (defVal env v) [] = v
  evalVar zero    (defFun env t) sp = eval t env sp
  evalVar (suc x) (defVal env _) sp = evalVar x env sp
  evalVar (suc x) (defFun env _) sp = evalVar x env sp

  extEnv : ∀ {Γ A} → Env Γ → Tm Γ A → Env (Γ ▶ A)
  extEnv {A = val _} env t = defVal env (eval t env [])
  extEnv {A = _ ⇒ _} env t = defFun env t

  {-# NON_TERMINATING #-}
  eval : ∀ {Γ A} → Tm Γ A → (Env Γ → ∀ {B} → Spine A B → Val B)
  eval (var x)       env sp       = evalVar x env sp
  eval (let' t u)    env sp       = eval u (extEnv env t) sp
  eval (fix t)       env sp       = eval t (defFun env (fix t)) sp
  eval (lam t)       env (v ∷ sp) = eval t (defVal env v) sp
  eval (app t u)     env sp       = eval t env (eval u env [] ∷ sp)
  eval (pair t u)    env []       = pair (eval t env []) (eval u env [])
  eval (fst t)       env []       = case (eval t env []) of λ {(pair v u) → v}
  eval (snd t)       env []       = case (eval t env []) of λ {(pair v u) → u}
  eval tt            env []       = tt
  eval (inl t)       env []       = inl (eval t env [])
  eval (inr t)       env []       = inr (eval t env [])
  eval (split t l r) env sp       = case (eval t env []) of λ {
                                      (inl v) → eval l (defVal env v) sp;
                                      (inr v) → eval r (defVal env v) sp}
  eval (wrap t)      env []       = wrap (eval t env [])
  eval (unwrap t)    env []       = case (eval t env []) of λ {(wrap v) → v}
