{-# OPTIONS --type-in-type #-}

open import Relation.Binary.PropositionalEquality
open import Data.Nat
open import Data.Unit

record Pair (A B : Set) : Set where
  constructor _,_
  field
    fst : A
    snd : B
open Pair

ℕElim : {P : ℕ → Set} → (∀ {n} → P n → P (suc n)) → P 0 → ∀ n → P n
ℕElim     s z zero    = z
ℕElim {P} s z (suc n) = s {n} (ℕElim {P} s z n)

Vec : ℕ → Set → Set
Vec n A = ℕElim (λ B → Pair A B) ⊤ n

vnil : ∀ {A} → Vec 0 A
vnil = tt

vcons : ∀ {n A} → A → Vec n A → Vec (suc n) A
vcons a as = a , as

caseℕ : ∀ {A : Set} → ℕ → (ℕ → A) → A → A
caseℕ zero    s z = z
caseℕ (suc n) s z = s n

test : ℕ → ℕ
test x =
  let m : ℕ
      m = _
      p : (caseℕ x (λ _ → 0) 0) ≡ (caseℕ m (λ _ → 0) 0)
      p = refl
  in x
