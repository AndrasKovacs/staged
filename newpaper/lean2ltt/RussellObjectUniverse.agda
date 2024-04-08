

module RussellObjectUniverse where

open import Data.Product renaming (proj₁ to ₁; proj₂ to ₂) public
open import Data.Bool public
import Level as L
open import Function
open import Relation.Binary.PropositionalEquality

--------------------------------------------------------------------------------

record Ty* i : Set (L.suc i) where
  constructor mkTy
  field
    unTy : Set i
open Ty* public

record Tm* {i} (A : Ty* i) : Set i where
  constructor mkTm
  field
    unTm : unTy A
open Tm* public

Ty : ∀ i → Tm* (mkTy (Ty* (L.suc i)))   -- ∀ i → Tm (Ty (suc i))
Ty i = mkTm (mkTy (Ty* i))

Tm : ∀ {i} → Tm* (unTm (Ty i)) → Set i  -- ∀ {i} → Tm (Ty i) → Set i
Tm a = Tm* (unTm a)

--------------------------------------------------------------------------------
