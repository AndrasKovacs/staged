{-# OPTIONS --type-in-type #-}

module Lib where

open import Agda.Primitive public
open import Relation.Binary.PropositionalEquality
  renaming (cong to ap; sym to infix 6 _⁻¹; trans to infixr 4 _◼_; subst to tr)
  renaming ([_] to hide)
  public
open import Data.Product hiding (_<*>_; map; zip) renaming (proj₁ to ₁; proj₂ to ₂) public
open import Function public
open import Data.List hiding (tabulate; take; drop; foldr; foldl; zip; concatMap; filter) public
open import Data.Empty public
open import Data.Nat using (ℕ; zero; suc) public

record Number {a} (A : Set a) : Set a where
  field fromNat : ℕ → A
open Number ⦃...⦄ public
{-# BUILTIN FROMNAT fromNat #-}

instance
  Numℕ : Number ℕ
  Number.fromNat Numℕ x = x

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

record Semigroup (A : Set) : Set where
  field
    _<>_ : A → A → A
  infixr 6 _<>_
open Semigroup ⦃...⦄ public

record Monoid (A : Set) : Set where
  field
    mempty : A
    {{ semigroupA }} : Semigroup A
open Monoid ⦃...⦄ public

record Applicative (F : Set → Set) : Set where
  field
    pure  : ∀ {A} → A → F A
    _<*>_ : ∀ {A B} → F (A → B) → F A → F B

  infixl 4 _<$>_ _<*>_

  _<$>_ : ∀ {A B} → (A → B) → F A → F B
  f <$> fa = pure f <*> fa

  infixl 1 _<&>_
  _<&>_ : ∀ {A B} → F A → (A → B) → F B
  fa <&> f = f <$> fa

open Applicative ⦃...⦄ public

record Monad (M : Set → Set) : Set where
  field
    _>>=_  : ∀ {A B} → M A → (A → M B) → M B
    {{ applicativeM }} : Applicative M
  infixl 1 _>>=_ _>>_

  _>>_ : ∀ {A B} → M A → M B → M B
  _>>_ ma mb = ma >>= λ _ → mb

open Monad ⦃...⦄ public

instance
  ApplicativeMaybe : Applicative Maybe
  Applicative.pure ApplicativeMaybe = just
  (ApplicativeMaybe Applicative.<*> just f) (just a) = just (f a)
  (ApplicativeMaybe Applicative.<*> _) _ = nothing

  MonadMaybe : Monad Maybe
  (MonadMaybe Monad.>>= nothing) f = nothing
  (MonadMaybe Monad.>>= just x)  f = f x

instance
  ApplicativeEither : ∀ {A} → Applicative (Either A)
  Applicative.pure ApplicativeEither = right
  Applicative._<*>_ ApplicativeEither (right f) (right a) = right (f a)
  Applicative._<*>_ ApplicativeEither (left e) _ = left e
  Applicative._<*>_ ApplicativeEither _ (left e) = left e

  MonadEither : ∀ {A} → Monad (Either A)
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
  AMaybeT : ∀ {M : Set → Set} ⦃ _ : Applicative M ⦄ → Applicative (MaybeT M)
  Applicative.pure AMaybeT a = maybeT (pure (just a))
  Applicative._<*>_ AMaybeT mf ma = maybeT (_<*>_ <$> runMaybeT mf <*> runMaybeT ma)

  MMaybeT : ∀ {M : Set → Set} ⦃ _ : Monad M ⦄ → Monad (MaybeT M)
  Monad._>>=_ MMaybeT ma f = maybeT do x ← runMaybeT ma; case x of λ where
    nothing  → pure nothing
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
  AIdentity : Applicative Identity
  Applicative.pure AIdentity = identity
  Applicative._<*>_ AIdentity (identity f) (identity a) = identity (f a)

  MonadIdentity : Monad Identity
  Monad._>>=_ MonadIdentity (identity a) f = f a

record ReaderT (R : Set)(M : Set → Set)(A : Set) : Set where
  constructor readerT
  field
    runReaderT : R → M A
open ReaderT public

instance
  AReaderT : ∀{R M}⦃ _ : Applicative M ⦄ → Applicative (ReaderT R M)
  Applicative.pure AReaderT a = readerT λ _ → pure a
  Applicative._<*>_ AReaderT mf ma = readerT λ r → runReaderT mf r <*> runReaderT ma r

  MonadReaderT : ∀{R M}⦃ _ : Monad  M ⦄ → Monad (ReaderT R M)
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
  AStateT : ∀ {S M}⦃ _ : Monad M ⦄ → Applicative (StateT S M)
  Applicative.pure AStateT a = stateT λ s → pure (a , s)
  Applicative._<*>_ AStateT mf ma = stateT λ s → do
    f , s ← runStateT mf s; a , s ← runStateT ma s; pure (f a , s)

  MonadStateT : ∀ {S M}⦃ _ : Monad M ⦄ → Monad (StateT S M)
  Monad._>>=_ MonadStateT (stateT ma) f = stateT λ s → do a , s ← ma s; runStateT (f a) s

instance
  MTStateT : ∀ {S} → MonadTrans (StateT S)
  MonadTrans.lift MTStateT ma = stateT λ s → do a ← ma; pure (a , s)

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
  MonadReader.ask ReaderReaderT = readerT λ r → pure r
  MonadReader.local ReaderReaderT f (readerT ma) = readerT λ r → ma (f r)

  ReaderStateT : ∀ {S M R}⦃ _ : MonadReader R M ⦄ → MonadReader R (StateT S M)
  MonadReader.ask ReaderStateT   = lift ask
  MonadReader.local ReaderStateT f (stateT ma) = stateT λ s → local f (ma s)
