
module ObjInternals where

open import Data.Product renaming (proj₁ to ₁; proj₂ to ₂) public
open import Data.Bool public

record U : Set₁ where
  constructor mkU
  field
    unU : Set
open U public

record El (A : U) : Set where
  constructor mkEl
  field
    unEl : unU A
open El public

Pi' : (A : U) → (El A → U) → U
Pi' A B = mkU ((a : El A) → El (B a))

lam : ∀{A B} → ((a : El A) → El (B a)) → El (Pi' A B)
lam = mkEl

app : ∀{A B} → El (Pi' A B) → ((a : El A) → El (B a))
app = unEl

Bool' : U
Bool' = mkU Bool

true' : El Bool'
true' = mkEl true

false' : El Bool'
false' = mkEl false

BoolElim : (P : El Bool' → U) → (b : El Bool') → El (P true') → El (P false') → El (P b)
BoolElim P (mkEl true)  t f = t
BoolElim P (mkEl false) t f = f

{-# DISPLAY mkU Bool = Bool' #-}

{-
U, El + type formers with βη
elimination restricted into U
-}
