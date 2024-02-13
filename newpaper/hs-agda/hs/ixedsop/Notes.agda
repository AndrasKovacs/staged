{-# OPTIONS --guardedness #-}

open import Function
open import Data.Product
open import Data.Maybe

data Step (S A : Set) : Set where
  stop  : Step S A
  skip  : S → Step S A
  yield : A → S → Step S A

record Pull (A : Set) : Set₁ where
  coinductive
  field
    S    : Set
    seed : S
    step : S → Step S A
open Pull

bind : ∀ {A B} → Pull A → (A → Pull B) → Pull B
S    (bind {A}{B} as f) = S as × Maybe (Σ A (S ∘ f))
seed (bind {A}{B} as f) = seed as , nothing
step (bind {A} {B} as f) (s , just (a , s')) with step (f a) s'
... | stop       = skip (s , nothing)
... | skip s'    = skip (s , (just (a , s')))
... | yield b s' = yield b (s , (just (a , s')))
step (bind {A} {B} as f) (s , nothing) with step as s
... | stop      = stop
... | skip s    = skip (s , nothing)
... | yield a s = skip (s , just (a , seed (f a)))

-- compute bss s.t.
-- State = (s, Maybe (ZipS ass bss))
-- seed' : S ass -> ZipS ass bss      (returns (S ass) unchanged as first proj)
-- step' : ZipS ass bss -> Gen (Step (ZipS ass bss) B)
