
module Fusion2 where

open import Data.Product
open import Data.Maybe
open import Function

data Step (S A : Set) : Set where
  done  : Step S A
  yield : A → S → Step S A

record Stream (A : Set) : Set₁ where
  field
    S    : Set
    step : S → Step S A
    seed : S
open Stream public

{-# NON_TERMINATING #-}
bindNothing : ∀ {A B : Set} (as : Stream A) (f : A → Stream B)
        → S as → Step (S as × Maybe (∃ (S ∘ f))) B
bindJust : ∀ {A B : Set} (as : Stream A) (f : A → Stream B)
        → S as × ∃ (S ∘ f) → Step (S as × Maybe (∃ (S ∘ f))) B
bindNothing as f sa with step as sa
... | done        = done
... | yield a sa' = bindJust as f (sa , a , seed (f a))
bindJust as f (sa , a , sb) with step (f a) sb
... | done       = bindNothing as f sa
... | yield b sb = yield b (sa , just (a , sb))

bind : ∀ {A B} → Stream A → (A → Stream B) → Stream B
S    (bind as f)                     = S as × Maybe (∃ (S ∘ f))
step (bind as f) (s , nothing)       = bindNothing as f s
step (bind as f) (s , just (a , s')) = bindJust as f (s , a , s')
seed (bind as f)                     = seed as , nothing
