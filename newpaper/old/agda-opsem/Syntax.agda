
module Syntax where

mutual
  data ValTy : Set where
    one   : ValTy
    _+_   : ValTy → ValTy → ValTy
    _*_   : ValTy → ValTy → ValTy
    μ     : Functor → ValTy

  data Functor : Set where
    K   : ValTy → Functor
    Id  : Functor
    _+_ : Functor → Functor → Functor
    _*_ : Functor → Functor → Functor

  data CompTy : Set where
    _⇒_ : ValTy → Ty → CompTy
  infixr 4 _⇒_

  data Ty : Set where
    V : ValTy → Ty
    ℂ : CompTy → Ty

infixr 5 _+_
infixr 6 _*_

⟦_⟧ : Functor → ValTy → ValTy
⟦ K A   ⟧ _ = A
⟦ Id    ⟧ A = A
⟦ F + G ⟧ A = ⟦ F ⟧ A + ⟦ G ⟧ A
⟦ F * G ⟧ A = ⟦ F ⟧ A * ⟦ G ⟧ A

data Con : Set where
  ∙   : Con
  _▶_ : Con → Ty → Con
infixl 3 _▶_

variable
  Γ Δ : Con
  A B C D : Ty
  a b c d : ValTy
  α β γ : CompTy
  F G : Functor

data Var : Con → Ty → Set where
  zero : Var (Γ ▶ A) A
  suc  : Var Γ A → Var (Γ ▶ B) A

data Tm (Γ : Con) : Ty → Set where
  varC   : Var Γ (ℂ α) → Tm Γ (ℂ α)
  varV   : Var Γ (V a) → Tm Γ (V a)
  letV   : Tm Γ (V a) → Tm (Γ ▶ V a) B → Tm Γ B
  letC   : Tm (Γ ▶ ℂ α) (ℂ α) → Tm (Γ ▶ ℂ α) B → Tm Γ B
  lam    : Tm (Γ ▶ V a) B → Tm Γ (ℂ (a ⇒ B))
  app    : Tm Γ (ℂ (a ⇒ B)) → Tm Γ (V a) → Tm Γ B
  pair   : Tm Γ (V a) → Tm Γ (V b) → Tm Γ (V (a * b))
  fst    : Tm Γ (V (a * b)) → Tm Γ (V a)
  snd    : Tm Γ (V (a * b)) → Tm Γ (V b)
  tt     : Tm Γ (V one)
  inl    : Tm Γ (V a) → Tm Γ (V (a + b))
  inr    : Tm Γ (V b) → Tm Γ (V (a + b))
  split  : Tm Γ (V (a + b)) → Tm (Γ ▶ V a) C → Tm (Γ ▶ V b) C → Tm Γ C
  wrap   : Tm Γ (V (⟦ F ⟧ (μ F))) → Tm Γ (V (μ F))
  unwrap : Tm Γ (V (μ F)) → Tm Γ (V (⟦ F ⟧ (μ F)))
