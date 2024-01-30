
{-# OPTIONS --without-K #-}

module Lib where

open import Level

_∘_ : ∀ {a b c}
        {A : Set a} {B : A → Set b} {C : {x : A} → B x → Set c} →
        (∀ {x} (y : B x) → C y) → (g : (x : A) → B x) →
        ((x : A) → C (g x))
f ∘ g = λ x → f (g x)

data _≡_ {i}{A : Set i} (x : A) : A → Set i where
  refl : x ≡ x
infix 4 _≡_
{-# BUILTIN EQUALITY _≡_ #-}

_◾_ : ∀{i}{A : Set i}{x y z : A} → x ≡ y → y ≡ z → x ≡ z
refl ◾ refl = refl
infixr 4 _◾_

_⁻¹ : ∀{i}{A : Set i}{x y : A} → x ≡ y → y ≡ x
refl ⁻¹ = refl
infix 6 _⁻¹

coe : ∀{i}{A B : Set i} → A ≡ B → A → B
coe refl a = a

_&_ :
  ∀{i j}{A : Set i}{B : Set j}(f : A → B){a₀ a₁ : A}(a₂ : a₀ ≡ a₁)
  → f a₀ ≡ f a₁
f & refl = refl
infixl 9 _&_

_⊗_ :
  ∀ {α β}{A : Set α}{B : Set β}
    {f g : A → B}(p : f ≡ g){a a' : A}(q : a ≡ a')
  → f a ≡ g a'
refl ⊗ refl = refl
infixl 8 _⊗_

record Σ {i j} (A : Set i) (B : A → Set j) : Set (i ⊔ j) where
  constructor _,_
  field
    proj₁ : A
    proj₂ : B proj₁
infixr 5 _,_
open Σ public

data _⊎_ {α β}(A : Set α)(B : Set β) : Set (α ⊔ β) where
  inj₁ : A → A ⊎ B
  inj₂ : B → A ⊎ B

_×_ : ∀{i j} → Set i → Set j → Set (i ⊔ j)
A × B = Σ A λ _ → B
infixr 4 _×_

record ⊤ : Set where
  constructor tt

data ⊥ : Set where

⊥-elim : ∀{i}{A : Set i} → ⊥ → A
⊥-elim ()

Dec : ∀ {α} → Set α → Set α
Dec A = A ⊎ (A → ⊥)

postulate
  fext  : ∀{i j}{A : Set i}{B : A → Set j}{f g : (x : A) → B x}
          → ((x : A) → f x ≡ g x) → f ≡ g

  fexti : ∀{i j}{A : Set i}{B : A → Set j}{f g : {x : A} → B x}
          → ((x : A) → f {x} ≡ g {x}) → (λ {x} → f {x}) ≡ (λ {x} → g {x})
