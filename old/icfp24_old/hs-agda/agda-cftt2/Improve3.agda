
module Improve3 where

open import Lib
open import Object
open import Gen
open import Split
open import SOP
open import Join

record TCT (F : VTy → Ty) (E : VTy) (M : Set → Set)(A : Set) : Set where
  constructor tct
  field
    runTCT : M (Either (↑(F E)) A)
open TCT public

tailcall : ∀ {F A M}⦃ _ : Applicative M ⦄ → ↑(F A) → TCT F A M (↑V A)
tailcall a = tct (pure (left a))

instance
  ATCT : ∀ {F E M}⦃ _ : Applicative M ⦄ → Applicative (TCT F E M)
  Applicative.pure ATCT a = tct (pure (right a))
  Applicative._<*>_ ATCT (tct mf) (tct ma) = tct $ _<*>_ <$> mf <*> ma

  MTCT : ∀ {F E M}⦃ _ : Monad M ⦄ → Monad (TCT F E M)
  Monad._>>=_ MTCT (tct ma) f = tct $ ma >>= λ where
    (left x) → pure (left x)
    (right y) → runTCT (f y)

  MTTCT : ∀ {F E} → MonadTrans (TCT F E)
  MonadTrans.lift MTTCT ma = tct (right <$> ma)

  MGTCT : ∀ {F E M}⦃ _ : MonadGen M ⦄ → MonadGen (TCT F E M)
  MonadGen.liftGen MGTCT ga = lift (liftGen ga)

  StateTCT : ∀ {F E M}⦃ _ : MonadState M ⦄ ⦃ _ : Monad M ⦄ → MonadState (TCT F E M)
  MonadState.StateTy (StateTCT {M = M}) = StateTy {M}
  MonadState.get StateTCT = lift get
  MonadState.put StateTCT s = lift $ put s

  MaybeTCT : ∀ {F E M}⦃ _ : MonadFail M ⦄ ⦃ _ : Monad M ⦄ → MonadFail (TCT F E M)
  MonadFail.fail MaybeTCT = tct fail
  MonadFail.catch MaybeTCT (tct x) (tct y) = tct (catch x y)

  ReaderTCT : ∀ {F E M}⦃ _ : MonadReader M ⦄ ⦃ _ : Monad M ⦄ → MonadReader (TCT F E M)
  MonadReader.ReaderTy (ReaderTCT {M = M}) = ReaderTy {M}
  MonadReader.ask ReaderTCT = lift ask
  MonadReader.local ReaderTCT f (tct ma) = tct (local f ma)

  JoinTCT : ∀ {F E M}⦃ _ : MonadJoin M ⦄ → MonadJoin (TCT F E M)
  MonadJoin.join JoinTCT (tct ma) = tct {!!}

record Improve (F : VTy → Ty) (M : Set → Set) : Set₁ where
  field
    ⦃ MGM ⦄ : MonadGen M
    up'  : ∀ {A} → ↑ (F A) → M (↑V A)
    down : ∀ {A} → TCT F A M (↑V A) → ↑ (F A)

  up : ∀ {A E} → ↑ (F A) → TCT F E M (↑V A)
  up = lift ∘ up'

open Improve ⦃...⦄ public

instance
  ImpIdentity : Improve Identity∘ Gen
  Improve.up'   ImpIdentity m = pure (runIdentity∘ m)
  Improve.down ImpIdentity m = unGen (runTCT m) λ where
    (left x)  → x
    (right y) → identity∘ y

  ImpMaybeT : ∀ {F M} → ⦃ _ : Improve F M ⦄ → Improve (MaybeT∘ F) (MaybeT M)
  Improve.up'  ImpMaybeT m = maybeT (up' (runMaybeT∘ m) >>= split)
  Improve.down ImpMaybeT (tct m) = maybeT∘ $ down $ tct $ runMaybeT m <&> λ where
    (just (left x))  → left (runMaybeT∘ x)
    (just (right y)) → right (just∘ y)
    nothing          → right nothing∘

  ImpStateT : ∀ {F M S} → ⦃ _ : Improve F M ⦄ → Improve (StateT∘ S F) (StateT (↑V S) M)
  Improve.up'   ImpStateT x   = stateT λ s → do as ← up' (runStateT∘ x s); split as
  Improve.down ImpStateT (tct m) = stateT∘ λ s → down $ tct $ flip runStateT s m <&> λ where
    (left a  , s) → left (runStateT∘ a s)
    (right a , s) → right (a ,∘ s)

  ImpReaderT : ∀ {F M S} → ⦃ _ : Improve F M ⦄ → Improve (ReaderT∘ S F) (ReaderT (↑V S) M)
  Improve.up' ImpReaderT   x = readerT (up' ∘ runReaderT∘ x)
  Improve.down ImpReaderT (tct m) = readerT∘ λ r → down $ tct $ flip runReaderT r $ m >>= λ where
     (left x)  → readerT λ r → pure (left (runReaderT∘ x r))
     (right y) → pure $ right y
