
module Fusion3 where

open import Data.Product
open import Data.Maybe
open import Function

data Step (S A : Set) : Set where
  done  : Step S A
  skip  : S → Step S A
  yield : A → S → Step S A

record Stream (A : Set) : Set₁ where
  field
    S    : Set
    step : S → Step S A
    seed : S
open Stream public

bind : ∀ {A B} → Stream A → (A → Stream B) → Stream B
S    (bind as f) = S as × Maybe (∃ (S ∘ f))
seed (bind as f) = seed as , nothing
step (bind as f) (s , nothing) = skip (s , nothing)
step (bind as f) (s , just (a , s')) with step (f a) s'
... | done       = skip (s , nothing)
... | skip s'    = skip (s , just (a , s'))
... | yield b s' = yield b (s , (just (a , s')))
