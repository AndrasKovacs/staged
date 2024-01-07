
{-# OPTIONS --type-in-type #-}

open import Data.Product
open import Function

module _ (S A : Set) where
  data PT : Set
  data Res : Set

  data PT where
    con : (S → Res × S) → PT

  data Res where
    nothing : Res
    just    : A → PT → Res

  append : PT → PT → PT
  append (con xs) (con ys) =
    con λ s → case xs s of λ { (nothing   , s) → ys s
                             ; (just x xs , s) → (just x (append xs (con ys))) , s}
