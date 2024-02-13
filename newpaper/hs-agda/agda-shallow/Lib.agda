
module Lib where

open import Agda.Primitive public
open import Relation.Binary.PropositionalEquality
  renaming (cong to ap; sym to infix 6 _⁻¹; trans to infixr 4 _◼_; subst to tr)
  hiding ([_])
  public
open import Data.Product hiding (_<*>_) renaming (proj₁ to ₁; proj₂ to ₂) public
open import Function public

variable
  i : Level

record ⊤ {i} : Set i where
  constructor tt

data Maybe (A : Set i) : Set i where
  just : A → Maybe A
  nothing : Maybe A

data Either (A : Set i) (B : Set i) : Set i where
  left : (x : A)  → Either A B
  right : (y : B) → Either A B

data ℕ : Set where

record Monad (M : Set i → Set i) : Set (lsuc i) where
  field
    return : {A : Set i} → A → M A
    _>>=_  : ∀ {A B} → M A → (A → M B) → M B
  infixl 1 _>>=_ _>>_

  _>>_ : ∀ {A B} → M A → M B → M B
  _>>_ ma mb = ma >>= λ _ → mb

  infixl 4 _<$>_ _<*>_
  _<$>_ : ∀ {A B} → (A → B) → M A → M B
  f <$> ma = ma >>= λ a → return (f a)

  _<*>_ : ∀ {A B} → M (A → B) → M A → M B
  mf <*> ma = mf >>= λ f → ma >>= λ a → return (f a)

open Monad ⦃...⦄ public

instance
  MonadMaybe : Monad (Maybe {i})
  Monad.return MonadMaybe = just
  (MonadMaybe Monad.>>= nothing) f = nothing
  (MonadMaybe Monad.>>= just x)  f = f x

instance
  MonadEither : ∀ {A : Set i} → Monad (Either A)
  Monad.return MonadEither = right
  (MonadEither Monad.>>= left x) f = left x
  (MonadEither Monad.>>= right y) f = f y

record MaybeT (M : Set i → Set i)(A : Set i) : Set i where
  constructor maybeT
  field
    runMaybeT : M (Maybe A)
open MaybeT public

record MonadTrans (T : (Set i → Set i) → Set i → Set i) : Set (lsuc i) where
  field
    ⦃ monadM ⦄ : ∀ {M}⦃ _ : Monad M ⦄ → Monad (T M)
    lift : ∀ {M A}⦃ _ : Monad M ⦄ → M A → T M A
open MonadTrans ⦃...⦄ public

instance
  MMaybeT : ∀ {M : Set i → Set i} ⦃ _ : Monad M ⦄ → Monad (MaybeT M)
  Monad.return MMaybeT a   = maybeT (return (just a))
  Monad._>>=_ MMaybeT ma f = maybeT do x ← runMaybeT ma; case x of λ where
    nothing  → return nothing
    (just x) → runMaybeT (f x)

instance
  MTMaybeT : MonadTrans {i} MaybeT
  MonadTrans.lift MTMaybeT ma = maybeT (just <$> ma)

record Identity (A : Set i) : Set i where
  constructor identity
  field
    runIdentity : A
open Identity public

instance
  MonadIdentity : Monad {i} Identity
  Monad.return MonadIdentity = identity
  Monad._>>=_ MonadIdentity (identity a) f = f a

record ReaderT (R : Set i)(M : Set i → Set i)(A : Set i) : Set i where
  constructor readerT
  field
    runReaderT : R → M A
open ReaderT public

instance
  MonadReaderT : ∀{R M}⦃ _ : Monad {i} M ⦄ → Monad (ReaderT R M)
  Monad.return MonadReaderT a             = readerT λ _ → return a
  Monad._>>=_ MonadReaderT (readerT ma) f = readerT λ r → ma r >>= λ a → runReaderT (f a) r

instance
  MTReaderT : ∀ {R} → MonadTrans (ReaderT {i} R)
  MonadTrans.lift MTReaderT ma = readerT λ _ → ma

record StateT (S : Set i)(M : Set i → Set i)(A : Set i) : Set i where
  constructor stateT
  field
    runStateT : S → M (A × S)
open StateT

instance
  MonadStateT : ∀ {S M}⦃ _ : Monad {i} M ⦄ → Monad (StateT S M)
  Monad.return MonadStateT a = stateT λ s → return (a , s)
  Monad._>>=_ MonadStateT (stateT ma) f = stateT λ s → do a , s ← ma s; runStateT (f a) s

instance
  MTStateT : ∀ {i S} → MonadTrans {i} (StateT S)
  MonadTrans.lift MTStateT ma = stateT λ s → do a ← ma; return (a , s)

record MonadReader (R : Set i)(M : Set i → Set i) : Set (lsuc i) where
  field
    ⦃ monadM ⦄ : Monad M
    ask    : M R
    local  : ∀ {A} → (R → R) → M A → M A
open MonadReader ⦃...⦄ public

record MonadState (S : Set i)(M : Set i → Set i) : Set (lsuc i) where
  field
    ⦃ monadM ⦄ : Monad M
    get : M S
    put : S → M ⊤
open MonadState ⦃...⦄ public

State : Set i → Set i → Set i
State S A = StateT S Identity A

Reader : Set i → Set i → Set i
Reader R A = ReaderT R Identity A

instance
  ReaderReaderT : ∀ {M R}⦃ _ : Monad {i} M ⦄ → MonadReader R (ReaderT R M)
  MonadReader.ask ReaderReaderT = readerT λ r → return r
  MonadReader.local ReaderReaderT f (readerT ma) = readerT λ r → ma (f r)

  ReaderStateT : ∀ {S M R}⦃ _ : MonadReader {i} R M ⦄ → MonadReader R (StateT S M)
  MonadReader.ask ReaderStateT   = lift ask
  MonadReader.local ReaderStateT f (stateT ma) = stateT λ s → local f (ma s)
