
module Improve where

open import Lib
open import Object
open import Gen
open import Split
open import SOP

data TailCall (F : VTy → Ty) (A : VTy) : Set where
  call' : ↑ (F A) → TailCall F A
  ret'  : ↑V A → TailCall F A

record Improve (F : VTy → Ty) (M : Set → Set) : Set₁ where
  field
    ⦃ MGM ⦄ : MonadGen M
    up      : ∀ {A} → ↑ (F A) → M (↑V A)
    downTC  : ∀ {A} → M (TailCall F A) → ↑ (F A)

  ret : ∀ {A} → ↑V A → M (TailCall F A)
  ret a = pure $ ret' a

  call : ∀ {A} → ↑ (F A) → M (TailCall F A)
  call a = pure $ call' a

  down : ∀ {A} → M (↑V A) → ↑ (F A)
  down ma = downTC (ret' <$> ma)

  upTC : ∀ {A} → TailCall F A → M (↑V A)
  upTC (call' x) = up x
  upTC (ret' x)  = pure x

open Improve ⦃...⦄ public

instance
  ImpIdentity : Improve Identity∘ Gen
  Improve.up   ImpIdentity x = pure (runIdentity∘ x)
  Improve.downTC ImpIdentity m = identity∘ $ unGen m λ where
    (call' a) → runIdentity∘ a
    (ret' a)  → a

  ImpMaybeT : ∀ {F M} → ⦃ _ : Improve F M ⦄ → Improve (MaybeT∘ F) (MaybeT M)
  Improve.up ImpMaybeT x = maybeT do x ← up (runMaybeT∘ x); split x
  Improve.downTC ImpMaybeT x = maybeT∘ $ downTC $ runMaybeT x >>= λ where
    (just (call' x)) → pure $ call' (runMaybeT∘ x)
    (just (ret' x))  → pure $ ret' $ just∘ x
    nothing          → pure $ ret' nothing∘

  ImpStateT : ∀ {F M S} → ⦃ _ : Improve F M ⦄ → Improve (StateT∘ S F) (StateT (↑V S) M)
  Improve.up   ImpStateT x   = stateT λ s → do as ← up (runStateT∘ x s); split as
  Improve.downTC ImpStateT m = stateT∘ λ s → downTC $ flip runStateT s m >>= λ where
    (call' a , s) → pure $ call' (runStateT∘ a s)
    (ret' a  , s) → pure $ ret' (a ,∘ s)

  ImpReaderT : ∀ {F M S} → ⦃ _ : Improve F M ⦄ → Improve (ReaderT∘ S F) (ReaderT (↑V S) M)
  Improve.up ImpReaderT   x = readerT (up ∘ runReaderT∘ x)
  Improve.downTC ImpReaderT m = readerT∘ λ r →
     downTC $ flip runReaderT r $ m >>= λ where
         (call' a) → readerT λ r → pure $ call' (runReaderT∘ a r)
         (ret' a)  → pure $ ret' a
