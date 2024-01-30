{-# OPTIONS --without-K #-}

module Syntax where
open import Lib

infixr 4 _⇒_
infix 5 ↑_

data Ty : Set where
  ι   : Ty
  _⇒_ : Ty → Ty → Ty
  ↑_  : Ty → Ty

data Con : Set where
  ∙   : Con
  _,_ : Con → Ty → Con

data _∈_ (A : Ty) : Con → Set where
  vz : ∀ {Γ} → A ∈ (Γ , A)
  vs : ∀ {B Γ} → (v : A ∈ Γ) → A ∈ (Γ , B)

infix 5 ~_

data Tm Γ : Ty → Set where
  var : ∀ {A} → (v : A ∈ Γ) → Tm Γ A
  lam : ∀ {A B} → Tm (Γ , A) B → Tm Γ (A ⇒ B)
  app : ∀ {A B} → (f : Tm Γ (A ⇒ B)) → (a : Tm Γ A) → Tm Γ B
  <_> : ∀ {A} → Tm Γ A → Tm Γ (↑ A)
  ~_  : ∀ {A} → Tm Γ (↑ A) → Tm Γ A
