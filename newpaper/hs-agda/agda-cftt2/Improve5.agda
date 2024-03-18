
module Improve5 where

open import Lib
open import Object
open import Gen
open import Split
open import SOP

record Improve (F : VTy → Ty) (A : VTy) (M : Set → Set) : Set₁ where
  field
    {{MGM}}  : MonadGen M
    up       : ↑ (F A) → M (↑V A)
    down     : M (↑V A) → ↑ (F A)
    call'    : ↑ (F A) → M (TailTy {M})
    ret'     : ↑ (V A) → M (TailTy {M})
open Improve ⦃...⦄ public

tailcall : ∀ {F A M} → ⦃ _ : Improve F A M ⦄ → ↑ (F A) → M (↑V A)
tailcall fa = do e ← call' fa; liftGen (gen λ l _ → l e)

tailret : ∀ {F A M} → ⦃ _ : Improve F A M ⦄ → ↑ (V A) → M (↑V A)
tailret a = do e ← ret' a; liftGen (gen λ l _ → l e)

instance
  IIdentity : ∀ {A} → Improve Identity∘ A (Gen (↑V A))
  Improve.MGM   IIdentity   = MGGen
  Improve.up    IIdentity m = pure (runIdentity∘ m)
  Improve.down  IIdentity m = unGen m identity∘ identity∘
  Improve.call' IIdentity m = pure (runIdentity∘ m)
  Improve.ret'  IIdentity m = pure m

  IMaybeT : ∀ {F M A} ⦃ _ : Improve F (Maybe∘ A) M ⦄ → Improve (MaybeT∘ F) A (MaybeT M)
  Improve.MGM   IMaybeT = MGMaybeT
  Improve.up    IMaybeT m = maybeT (up (runMaybeT∘ m) >>= split)
  Improve.down  IMaybeT m = maybeT∘ $ down $ runMaybeT m >>= λ where
    (just x) → pure $ just∘ x
    nothing  → pure nothing∘
  Improve.call' IMaybeT m = lift $ call' (runMaybeT∘ m)
  Improve.ret'  IMaybeT m = lift $ ret' (just∘ m)

  IStateT : ∀ {F M S A}⦃ _ : Improve F (A ×∘ S) M ⦄ → Improve (StateT∘ S F) A (StateT (↑V S) M)
  Improve.MGM   IStateT = MGStateT
  Improve.up    IStateT m = stateT λ s → up (runStateT∘ m s) >>= split
  Improve.down  IStateT m = stateT∘ λ s → down (do (a , s) ← runStateT m s; pure (a ,∘ s))
  Improve.call' IStateT m = do s ← get; lift $ call' (runStateT∘ m s)
  Improve.ret'  IStateT m = do s ← get; lift $ ret' (m ,∘ s)

  IReaderT : ∀ {F M S A}⦃ _ : Improve F A M ⦄ → Improve (ReaderT∘ S F) A (ReaderT (↑V S) M)
  Improve.MGM   IReaderT = MGReaderT
  Improve.up    IReaderT m = readerT (up ∘ runReaderT∘ m)
  Improve.down  IReaderT m = readerT∘ (down ∘ runReaderT m)
  Improve.call' IReaderT m = do r ← ask; lift $ call' (runReaderT∘ m r)
  Improve.ret'  IReaderT m = lift $ ret' m
