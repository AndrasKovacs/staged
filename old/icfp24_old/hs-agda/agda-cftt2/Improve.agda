
module Improve where

open import Lib
open import Object
open import Gen
open import Split
open import SOP

record Improve (F : VTy → Ty) (M : Set → Set) : Set₁ where
  field
    ⦃ MGM ⦄ : MonadGen M
    up    : ∀ {A} → ↑ (F A) → M (↑V A)
    down  : ∀ {A} → M (↑V A) → ↑ (F A)

open Improve ⦃...⦄ public

instance
  ImpIdentity : ∀ {E} → Improve Identity∘ (Gen E)
  Improve.up   ImpIdentity m = pure (runIdentity∘ m)
  Improve.down ImpIdentity m = unGen m {!!} {!!}

--   ImpMaybeT : ∀ {F M} → ⦃ _ : Improve F M ⦄ → Improve (MaybeT∘ F) (MaybeT M)
--   Improve.up ImpMaybeT x = maybeT do x ← up (runMaybeT∘ x); split x
--   Improve.downTC ImpMaybeT x = maybeT∘ $ downTC $ runMaybeT x >>= λ where
--     (just (call' x)) → pure $ call' (runMaybeT∘ x)
--     (just (ret' x))  → pure $ ret' $ just∘ x
--     nothing          → pure $ ret' nothing∘

--   ImpStateT : ∀ {F M S} → ⦃ _ : Improve F M ⦄ → Improve (StateT∘ S F) (StateT (↑V S) M)
--   Improve.up   ImpStateT x   = stateT λ s → do as ← up (runStateT∘ x s); split as
--   Improve.downTC ImpStateT m = stateT∘ λ s → downTC $ flip runStateT s m >>= λ where
--     (call' a , s) → pure $ call' (runStateT∘ a s)
--     (ret' a  , s) → pure $ ret' (a ,∘ s)

--   ImpReaderT : ∀ {F M S} → ⦃ _ : Improve F M ⦄ → Improve (ReaderT∘ S F) (ReaderT (↑V S) M)
--   Improve.up ImpReaderT   x = readerT (up ∘ runReaderT∘ x)
--   Improve.downTC ImpReaderT m = readerT∘ λ r →
--      downTC $ flip runReaderT r $ m >>= λ where
--          (call' a) → readerT λ r → pure $ call' (runReaderT∘ a r)
--          (ret' a)  → pure $ ret' a
