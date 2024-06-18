
{-# OPTIONS --large-indices #-}

module Improve2 where

open import Lib
open import Object
open import Gen
open import Split
open import SOP

record TCT (F : VTy → Ty) (E : VTy) (M : Set → Set)(A : Set) : Set where
  constructor tct
  field
    runTCT : M (Either (↑ (F E)) A)
open TCT public

instance
  ATCT : ∀ {F E M}⦃ _ : Monad M ⦄ → Applicative (TCT F E M)
  Applicative.pure ATCT a = tct $ pure $ right a
  Applicative._<*>_ ATCT (tct mf) (tct ma) = tct $ _<*>_ <$> mf <*> ma

  MTCT : ∀ {F E M}⦃ _ : Monad M ⦄ → Monad (TCT F E M)
  Monad._>>=_ MTCT (tct ma) f = tct $ ma >>= λ where
    (left e)  → pure $ left e
    (right a) → runTCT (f a)

  MTTCT : ∀ {F E} → MonadTrans (TCT F E)
  MonadTrans.lift MTTCT ma = tct (right <$> ma)

record MonadTCT (F : VTy → Ty)(E : VTy) (M : Set → Set) : Set₁ where
  field
    call    : ∀ {A} → ↑(F E) → M A
    downTCT : ∀ {A} → M (↑V A) → ↑ (F A)
    upTCT   : ∀ {A} → ↑ (F A) → M (↑V A)
open MonadTCT ⦃...⦄ public

record Improve (F : VTy → Ty) (M : Set → Set) : Set₁ where
  field
    ⦃ MGM ⦄ : MonadGen M
    up      : ∀ {A} → ↑ (F A) → M (↑V A)
    down    : ∀ {A} → M (↑V A) → ↑ (F A)
open Improve ⦃...⦄ public

instance
  ImpIdentity : Improve Identity∘ Gen
  Improve.up   ImpIdentity x = pure (runIdentity∘ x)
  Improve.down ImpIdentity x = runGen (identity∘ <$> x)

  ImpMaybeT : ∀ {F M} → ⦃ _ : Improve F M ⦄ → Improve (MaybeT∘ F) (MaybeT M)
  Improve.up ImpMaybeT x = maybeT do x ← up (runMaybeT∘ x); split x
  Improve.down ImpMaybeT x = maybeT∘ $ down $ runMaybeT x >>= λ where
    (just x) → pure $ just∘ x
    nothing  → pure nothing∘

  ImpStateT : ∀ {F M S} → ⦃ _ : Improve F M ⦄ → Improve (StateT∘ S F) (StateT (↑V S) M)
  Improve.up   ImpStateT x          = stateT λ s → do as ← up (runStateT∘ x s); split as
  Improve.down ImpStateT (stateT x) = stateT∘ λ s → down (do (a , s) ← x s; pure (a ,∘ s))

  ImpReaderT : ∀ {F M S} → ⦃ _ : Improve F M ⦄ → Improve (ReaderT∘ S F) (ReaderT (↑V S) M)
  Improve.up ImpReaderT   x           = readerT (up ∘ runReaderT∘ x)
  Improve.down ImpReaderT (readerT x) = readerT∘ (down ∘ x)

  -- ImpTCT : ∀ {F E M} ⦃ _ : Improve F M ⦄ → Improve F (TCT F E M)
  -- ImpTCT = {!!}

-- upTCT : ∀ {F E M A}⦃ _ : Improve F M ⦄ → ↑ (F A) → TCT F E M (↑V A)
-- upTCT fa = lift $ up fa

-- downTCT : ∀ {F M A}⦃ _ : Improve F M ⦄ → TCT F A M (↑V A) → ↑ (F A)
-- downTCT ma = down (runTCT ma >>= λ where
--   (left x)  → {!!}
--   (right y) → pure y)

instance
  MTCTTCT : MonadTCT



--------------------------------------------------------------------------------
