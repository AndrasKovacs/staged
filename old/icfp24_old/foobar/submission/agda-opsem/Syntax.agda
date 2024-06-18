
module Syntax where

{-
Syntax & typing for the object language.

Instead of having "data" declarations, we support datatypes as fixpoints of
functors, with wrapping/unwrapping rules.
-}

mutual
  data ValTy : Set where
    one   : ValTy
    _+_   : ValTy → ValTy → ValTy
    _*_   : ValTy → ValTy → ValTy
    μ     : Functor → ValTy         -- fixpoints

  data Functor : Set where
    K   : ValTy → Functor                -- Constant functor
    Id  : Functor                        -- Identity (marks recursive type occurrences)
    _+_ : Functor → Functor → Functor
    _*_ : Functor → Functor → Functor

  data CompTy : Set where
    _⇒_ : ValTy → Ty → CompTy
    one : CompTy
    _*_ : CompTy → CompTy → CompTy
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

  -- We split let and variable rules just for convenience in pattern matching
  -- proofs in Agda.
  varC   : Var Γ (ℂ α) → Tm Γ (ℂ α)
  varV   : Var Γ (V a) → Tm Γ (V a)
  letV   : Tm Γ (V a) → Tm (Γ ▶ V a) B → Tm Γ B
  letC   : Tm (Γ ▶ ℂ α) (ℂ α) → Tm (Γ ▶ ℂ α) B → Tm Γ B

  -- value types
  pair   : Tm Γ (V a) → Tm Γ (V b) → Tm Γ (V (a * b))
  fst    : Tm Γ (V (a * b)) → Tm Γ (V a)
  snd    : Tm Γ (V (a * b)) → Tm Γ (V b)
  tt     : Tm Γ (V one)
  inl    : Tm Γ (V a) → Tm Γ (V (a + b))
  inr    : Tm Γ (V b) → Tm Γ (V (a + b))
  split  : Tm Γ (V (a + b)) → Tm (Γ ▶ V a) C → Tm (Γ ▶ V b) C → Tm Γ C
  wrap   : Tm Γ (V (⟦ F ⟧ (μ F))) → Tm Γ (V (μ F))
  unwrap : Tm Γ (V (μ F)) → Tm Γ (V (⟦ F ⟧ (μ F)))

  -- computation types
  lam    : Tm (Γ ▶ V a) B → Tm Γ (ℂ (a ⇒ B))
  app    : Tm Γ (ℂ (a ⇒ B)) → Tm Γ (V a) → Tm Γ B
  Tt     : Tm Γ (ℂ one)
  Pair   : Tm Γ (ℂ α) → Tm Γ (ℂ β) → Tm Γ (ℂ (α * β))
  Fst    : Tm Γ (ℂ (α * β)) → Tm Γ (ℂ α)
  Snd    : Tm Γ (ℂ (α * β)) → Tm Γ (ℂ β)
