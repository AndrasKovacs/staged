{-# OPTIONS --type-in-type #-}

module Lib where

open import Agda.Primitive public
open import Relation.Binary.PropositionalEquality
  renaming (cong to ap; sym to infix 6 _⁻¹; trans to infixr 4 _◼_; subst to tr)
  renaming ([_] to hide)
  public
open import Data.Product hiding (_<*>_; map; zip) renaming (proj₁ to ₁; proj₂ to ₂) public
open import Function public
open import Data.List hiding (tabulate) public
open import Data.Empty public

record ⊤ : Set where
  constructor tt

data Bool : Set where true false : Bool

data Maybe (A : Set) : Set where
  just : A → Maybe A
  nothing : Maybe A

data Either (A B : Set) : Set where
  left : (x : A)  → Either A B
  right : (y : B) → Either A B

either : ∀ {A B}{C : Set} → Either A B → (A → C) → (B → C) → C
either (left x) l r = l x
either (right y) l r = r y

case× : ∀ {A : Set}{B : A → Set}{C : ∀ a → B a → Set} → (ab : Σ A B) → (∀ a b → C a b) → C (ab .₁) (ab .₂)
case× (fst , snd) f = f fst snd

coe : ∀ {A B : Set} → A ≡ B → A → B
coe refl x = x

UIP : ∀ {A : Set}{x y : A}(p q : x ≡ y) → p ≡ q
UIP refl refl = refl

data ℕ : Set where

record Monad (M : Set → Set) : Set where
  field
    return : ∀ {A} → A → M A
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
  MonadMaybe : Monad Maybe
  Monad.return MonadMaybe = just
  (MonadMaybe Monad.>>= nothing) f = nothing
  (MonadMaybe Monad.>>= just x)  f = f x

instance
  MonadEither : ∀ {A} → Monad (Either A)
  Monad.return MonadEither = right
  (MonadEither Monad.>>= left x) f = left x
  (MonadEither Monad.>>= right y) f = f y

record MaybeT (M : Set → Set)(A : Set) : Set where
  constructor maybeT
  field
    runMaybeT : M (Maybe A)
open MaybeT public

record MonadTrans (T : (Set → Set) → Set → Set) : Set where
  field
    ⦃ monadM ⦄ : ∀ {M}⦃ _ : Monad M ⦄ → Monad (T M)
    lift : ∀ {M A}⦃ _ : Monad M ⦄ → M A → T M A
open MonadTrans ⦃...⦄ public

instance
  MMaybeT : ∀ {M : Set → Set} ⦃ _ : Monad M ⦄ → Monad (MaybeT M)
  Monad.return MMaybeT a   = maybeT (return (just a))
  Monad._>>=_ MMaybeT ma f = maybeT do x ← runMaybeT ma; case x of λ where
    nothing  → return nothing
    (just x) → runMaybeT (f x)

instance
  MTMaybeT : MonadTrans MaybeT
  MonadTrans.lift MTMaybeT ma = maybeT (just <$> ma)

record Identity (A : Set) : Set where
  constructor identity
  field
    runIdentity : A
open Identity public

instance
  MonadIdentity : Monad Identity
  Monad.return MonadIdentity = identity
  Monad._>>=_ MonadIdentity (identity a) f = f a

record ReaderT (R : Set)(M : Set → Set)(A : Set) : Set where
  constructor readerT
  field
    runReaderT : R → M A
open ReaderT public

instance
  MonadReaderT : ∀{R M}⦃ _ : Monad  M ⦄ → Monad (ReaderT R M)
  Monad.return MonadReaderT a             = readerT λ _ → return a
  Monad._>>=_ MonadReaderT (readerT ma) f = readerT λ r → ma r >>= λ a → runReaderT (f a) r

instance
  MTReaderT : ∀ {R} → MonadTrans (ReaderT R)
  MonadTrans.lift MTReaderT ma = readerT λ _ → ma

record StateT (S : Set)(M : Set → Set)(A : Set) : Set where
  constructor stateT
  field
    runStateT : S → M (A × S)
open StateT

instance
  MonadStateT : ∀ {S M}⦃ _ : Monad M ⦄ → Monad (StateT S M)
  Monad.return MonadStateT a = stateT λ s → return (a , s)
  Monad._>>=_ MonadStateT (stateT ma) f = stateT λ s → do a , s ← ma s; runStateT (f a) s

instance
  MTStateT : ∀ {S} → MonadTrans (StateT S)
  MonadTrans.lift MTStateT ma = stateT λ s → do a ← ma; return (a , s)

record MonadReader (R : Set)(M : Set → Set) : Set where
  field
    ⦃ monadM ⦄ : Monad M
    ask    : M R
    local  : ∀ {A} → (R → R) → M A → M A
open MonadReader ⦃...⦄ public

record MonadState (S : Set)(M : Set → Set) : Set where
  field
    ⦃ monadM ⦄ : Monad M
    get : M S
    put : S → M ⊤
open MonadState ⦃...⦄ public

State : Set → Set → Set
State S A = StateT S Identity A

Reader : Set → Set → Set
Reader R A = ReaderT R Identity A

instance
  ReaderReaderT : ∀ {M R}⦃ _ : Monad M ⦄ → MonadReader R (ReaderT R M)
  MonadReader.ask ReaderReaderT = readerT λ r → return r
  MonadReader.local ReaderReaderT f (readerT ma) = readerT λ r → ma (f r)

  ReaderStateT : ∀ {S M R}⦃ _ : MonadReader R M ⦄ → MonadReader R (StateT S M)
  MonadReader.ask ReaderStateT   = lift ask
  MonadReader.local ReaderStateT f (stateT ma) = stateT λ s → local f (ma s)
