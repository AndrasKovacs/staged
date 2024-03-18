
module Test where

open import Obj

foo : El Bool' → Bool
foo x = {!BoolElim!}

-- Univ : (A : U) → isContr (Σ (B : U) (B ≃ A))
--    B, B'
--    p : (B, B~) = (B', B'~)
--    transport fst p : B → B'
--    ap fst p : B = B'
