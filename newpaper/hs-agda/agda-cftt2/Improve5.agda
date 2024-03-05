
module Improve5 where

open import Lib
open import Object
open import Gen
open import Split
open import SOP

-- instance
--   IIdentity : ∀ {A} → Improve (Identity∘ A) (Gen {!!} (↑V A)) (↑V A)
--   Improve.up IIdentity m = gen λ l r → r (runIdentity∘ m)
--   Improve.tc IIdentity m = {!gen!}
--   Improve.down IIdentity m = unGen m identity∘ identity∘

record Improve (F : VTy → Ty) (A : VTy) (M : Set → Set) : Set₁ where
  field
    {{MGM}}  : MonadGen M
    up       : ↑ (F A) → M (↑V A)
    down     : M (↑V A) → ↑ (F A)
    call'    : ↑ (F A) → M (TailTy {M})
open Improve ⦃...⦄ public

instance
  IIdentity : ∀ {A} → Improve Identity∘ A (Gen (↑V A))
  Improve.MGM   IIdentity   = MGGen
  Improve.up    IIdentity m = {!!}
  Improve.down  IIdentity m = {!!}
  Improve.call' IIdentity m = pure (runIdentity∘ m)
  -- Imrpove.MGM      IIdentity = ?
  -- Improve.up       IIdentity m = pure (runIdentity∘ m)
  -- Improve.down     IIdentity m = unGen m identity∘ identity∘
  -- Improve.prepCall IIdentity m = gen λ _ r → r {!!}


  -- IMaybeT : ∀ {F M A} ⦃ _ : Improve (F (Maybe∘ A)) (M (↑V (Maybe∘ A))) {!!} ⦄ ⦃ _ : MonadGen M ⦄ → Improve (MaybeT∘ F A) (MaybeT M (↑V A)) {!!}
  -- Improve.up IMaybeT m = maybeT (do m ← up (runMaybeT∘ m); split m)
  -- Improve.tc IMaybeT m = maybeT {!tc (runMaybeT∘ m)!}
  -- Improve.down IMaybeT m = maybeT∘ $ down $ runMaybeT m >>= λ where
  --   (just x) → pure $ just∘ x
  --   nothing  → pure nothing∘

-- record TC (

--    tc : ↑ (F A) → M E


--   Improve.up       IIdentity m = gen λ l r → r (runIdentity∘ m)
--   Improve.tailcall IIdentity m = gen λ l r → l (runIdentity∘ m)
--   Improve.down     IIdentity m = unGen m identity∘ identity∘

--   -- foo : ∀ {A} → Improve (MaybeT∘ Identity∘) A (MaybeT (Gen (↑V (Maybe∘ A))))
--   -- Improve.up foo = {!!}
--   -- Improve.tailcall foo m = maybeT $ gen λ l r → l (runIdentity∘ (runMaybeT∘ m))
--   -- Improve.down foo = {!!}

--   IMaybeT : ∀ {F M A} → ⦃ _ : Improve F (Maybe∘ A) M ⦄ → Improve (MaybeT∘ F) A (MaybeT M)
--   Improve.up       IMaybeT m = maybeT (do m ← up (runMaybeT∘ m); {!!})
--   Improve.tailcall (IMaybeT {F} {M} {A}) m =
--     maybeT (do x ← tailcall {F}{Maybe∘ A}{M} (runMaybeT∘ m); {!!})
--   Improve.down IMaybeT m = maybeT∘ $ down $ runMaybeT m >>= λ where
--     (just x) → pure $ just∘ x
--     nothing  → pure nothing∘

--   ImpStateT : ∀ {F M S A} → ⦃ _ : Improve F (A ×∘ S) M ⦄ → Improve (StateT∘ S F) A (StateT (↑V S) M)
--   Improve.up   ImpStateT x          = stateT λ s → do as ← up (runStateT∘ x s); split as
--   Improve.down ImpStateT (stateT x) = stateT∘ λ s → down (do (a , s) ← x s; pure (a ,∘ s))

--   ImpReaderT : ∀ {F M S A} → ⦃ _ : Improve F A M ⦄ → Improve (ReaderT∘ S F) A (ReaderT (↑V S) M)
--   Improve.up ImpReaderT   x           = readerT (up ∘ runReaderT∘ x)
--   Improve.down ImpReaderT (readerT x) = readerT∘ (down ∘ x)
