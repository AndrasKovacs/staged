
{-# OPTIONS --type-in-type #-}

open import Function using (_$_)
open import Data.Nat

module Object where

data Ty : Set where
  Nat  : Ty
  _⇒_  : Ty → Ty → Ty
  List : Ty → Ty
infixr 2 _⇒_

{-# NO_POSITIVITY_CHECK #-}
data Tm : Ty → Set where
  zero  : Tm Nat
  suc   : Tm Nat → Tm Nat
  lam   : ∀ {A B} → (Tm A → Tm B) → Tm (A ⇒ B)
  _∙_   : ∀ {A B} → Tm (A ⇒ B) → (Tm A → Tm B)
  iter  : ∀ {A} → Tm Nat → Tm (A ⇒ A) → Tm A → Tm A
  nil   : ∀ {A} → Tm (List A)
  cons  : ∀ {A} → Tm A → Tm (List A) → Tm (List A)
  foldr : ∀ {A B} → Tm (A ⇒ B ⇒ B) → Tm B → Tm (List A) → Tm B
  Let   : ∀ {A B} → Tm A → (Tm A → Tm B) → Tm B

infixl 8 _∙_

lit : ℕ → Tm Nat
lit zero    = zero
lit (suc n) = suc (lit n)

map : ∀ {A B} → (Tm A → Tm B) → Tm (List A) → Tm (List B)
map f as = foldr (lam λ a → lam λ bs → cons (f a) bs) nil as

foo : Tm Nat
foo =
  Let (lit 2) λ two →
  Let (lit 10) λ ten →

  -- let add : Nat → Nat → Nat := λ n m. iter n suc m
  Let {Nat ⇒ Nat ⇒ Nat} (lam (λ n → lam (λ m → iter n (lam suc) m))) λ add →

  -- let mapSuc : List Nat → List Nat := map suc
  Let {List Nat ⇒ List Nat} (lam λ ns → map suc ns) λ mapSuc →

  add ∙ two ∙ (add ∙ ten ∙ ten)

-- map : ∀ {A B : ValTy} → (A → B) → List A → List B
-- map f as = foldr (λ a bs → cons (f a) bs) nil as

open import Data.String
open import Data.List renaming (List to List')

render : ∀ {A} → Tm A → List' String → String
render zero            usednames = {!!}
render (suc t)         usednames = {!!}
render (lam x)         usednames = {!!}
render (t ∙ t₁)        usednames = {!!}
render (iter t t₁ t₂)  usednames = {!!}
render nil             usednames = {!!}
render (cons t t₁)     usednames = {!!}
render (foldr t t₁ t₂) usednames = {!!}
render (Let t x)       usednames = {!!}









------------------------------------------------------------
