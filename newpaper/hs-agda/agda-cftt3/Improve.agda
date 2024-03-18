
module Improve where

open import Lib
open import Object
open import Gen
open import Split
open import SOP

-- Tail call transformer
--------------------------------------------------------------------------------

data TC (Call Ret : Set)(A : Set) : Set where
  tailcall' : Call → TC Call Ret A
  tailret'  : Ret  → TC Call Ret A
  ret'      : A    → TC Call Ret A

record TCT (Call Ret : Set)(M : Set → Set)(A : Set) : Set where
  constructor tct
  field
    runTCT : M (TC Call Ret A)
open TCT public

tailcall : ∀ {C R M A}⦃ _ : Applicative M ⦄ → C → TCT C R M A
tailcall c = tct (pure (tailcall' c))

tailret : ∀ {C R M A}⦃ _ : Applicative M ⦄ → R → TCT C R M A
tailret r = tct (pure (tailret' r))

instance
  ATCT : ∀ {F E M}⦃ _ : Monad M ⦄ → Applicative (TCT F E M)
  Applicative.pure  ATCT a                 = tct (pure (ret' a))
  Applicative._<*>_ ATCT (tct mf) (tct ma) = tct do mf >>= λ where
    (tailcall' x) → pure (tailcall' x)
    (tailret' x)  → pure (tailret' x)
    (ret' f)      → ma >>= λ where
      (tailcall' x) → pure (tailcall' x)
      (tailret' x)  → pure (tailret' x)
      (ret' a)      → pure (ret' (f a))

  MTCT : ∀ {F E M}⦃ _ : Monad M ⦄ → Monad (TCT F E M)
  Monad._>>=_ MTCT (tct ma) f = tct do ma >>= λ where
    (tailcall' x) → pure (tailcall' x)
    (tailret' x)  → pure (tailret' x)
    (ret' x)      → runTCT (f x)

  MTTCT : ∀ {F E} → MonadTrans (TCT F E)
  MonadTrans.lift MTTCT ma = tct (ret' <$> ma)

  ReaderTCT : ∀ {F E M}⦃ _ : MonadReader M ⦄ ⦃ _ : Monad M ⦄ → MonadReader (TCT F E M)
  MonadReader.ReaderTy (ReaderTCT {M = M}) = ReaderTy {M}
  MonadReader.ask   ReaderTCT = lift ask
  MonadReader.local ReaderTCT f (tct ma) = tct (local f ma)

  StateTCT : ∀ {F E M}⦃ _ : MonadState M ⦄ ⦃ _ : Monad M ⦄ → MonadState (TCT F E M)
  MonadState.StateTy (StateTCT {M = M}) = StateTy {M}
  MonadState.get StateTCT = lift get
  MonadState.put StateTCT s = lift (put s)

  FailTCT : ∀ {F E M}⦃ _ : MonadFail M ⦄ ⦃ _ : Monad M ⦄ → MonadFail (TCT F E M)
  MonadFail.fail FailTCT = lift fail
  MonadFail.catch FailTCT (tct x) (tct y) = tct (catch x y)

  MGTCT : ∀ {F E M}⦃ _ : MonadGen M ⦄ → MonadGen (TCT F E M)
  MonadGen.liftGen MGTCT ga = lift (liftGen ga)

--------------------------------------------------------------------------------

record Improve (F : VTy → Ty) (M : Set → Set) : Set₁ where
  field
    ⦃ MGM ⦄ : MonadGen M
    down : ∀ {A} → TCT (↑(F A)) (↑V A) M (↑V A) → ↑(F A)
    up'  : ∀ {A} → ↑ (F A) → M (↑V A)

  down' : ∀ {A} → M (↑V A) → ↑ (F A)
  down' ma = down (lift ma)

  up : ∀ {C R A} → ↑ (F A) → TCT C R M (↑V A)
  up fa = lift (up' fa)
open Improve ⦃...⦄ public

instance
  IIdentity : Improve Identity∘ Gen
  Improve.down IIdentity (tct ma) = identity∘ $ unGen ma λ where
    (tailcall' x) → runIdentity∘ x
    (tailret' x)  → x
    (ret' x)      → x
  Improve.up' IIdentity ma = pure (runIdentity∘ ma)

  IMaybeT : ∀ {F M}⦃ _ : Improve F M ⦄ → Improve (MaybeT∘ F) (MaybeT M)
  Improve.down IMaybeT ma = maybeT∘ $ down $ tct $ runMaybeT (runTCT ma) >>= λ where
      (just (tailcall' x)) → pure (tailcall' (runMaybeT∘ x))
      (just (tailret' x))  → pure (tailret' (just∘ x))
      (just (ret' x))      → pure (ret' (just∘ x))
      nothing              → pure (ret' nothing∘)
  Improve.up' IMaybeT x = maybeT (do x ← up' (runMaybeT∘ x); split x)

  IStateT : ∀ {F M S}⦃ _ : Improve F M ⦄ → Improve (StateT∘ S F) (StateT (↑V S) M)
  Improve.down IStateT (tct ma) = stateT∘ λ s → down $ tct $ flip runStateT s ma >>= λ where
    (tailcall' x , s) → pure (tailcall' (runStateT∘ x s))
    (tailret' x  , s) → pure (tailret' (x ,∘ s))
    (ret' x      , s) → pure (ret' (x ,∘ s))
  Improve.up' IStateT x = stateT λ s → do as ← up' (runStateT∘ x s); split as

  IReaderT : ∀ {F M S}⦃ _ : Improve F M ⦄ → Improve (ReaderT∘ S F) (ReaderT (↑V S) M)
  Improve.down IReaderT (tct ma) = readerT∘ λ r → down $ tct $ flip runReaderT r $ ma >>= λ where
    (tailcall' x) → do r ← ask; pure (tailcall' (runReaderT∘ x r))
    (tailret' x)  → pure (tailret' x)
    (ret' x)      → pure (ret' x)
  Improve.up' IReaderT x = readerT λ r → up' (runReaderT∘ x r)
