{-# OPTIONS --type-in-type #-}

module Improve where

open import Lib
open import Object
open import Gen
open import Split

record Improve (F : VTy → Ty) (M : Set → Set) : Set where
  field
    ⦃ MGM ⦄ : MonadGen M
    up      : ∀ {A} → ↑ (F A) → M (↑V A)
    down    : ∀ {A} → M (↑V A) → ↑ (F A)
open Improve ⦃...⦄ public

instance
  ImpIdentity : Improve Identity∘ Gen
  Improve.up   ImpIdentity x = gen λ k → k (runIdentity∘ x)
  Improve.down ImpIdentity x = runGen (identity∘ <$> x)

  ImpMaybeT : ∀ {F M} → ⦃ _ : Improve F M ⦄ → Improve (MaybeT∘ F) (MaybeT M)
  Improve.up ImpMaybeT x = maybeT do x ← up (runMaybeT∘ x); split x
  Improve.down ImpMaybeT (maybeT x) = maybeT∘ $ down do
    x >>= λ where
      nothing  → pure nothing∘
      (just a) → pure (just∘ a)

  ImpStateT : ∀ {F M S} → ⦃ _ : Improve F M ⦄ → Improve (StateT∘ S F) (StateT (↑V S) M)
  Improve.up   ImpStateT x          = stateT λ s → do as ← up (runStateT∘ x s); split as
  Improve.down ImpStateT (stateT x) = stateT∘ λ s → down (do (a , s) ← x s; pure (a ,∘ s))

  ImpReaderT : ∀ {F M S} → ⦃ _ : Improve F M ⦄ → Improve (ReaderT∘ S F) (ReaderT (↑V S) M)
  Improve.up ImpReaderT   x           = readerT (up ∘ runReaderT∘ x)
  Improve.down ImpReaderT (readerT x) = readerT∘ (down ∘ x)
