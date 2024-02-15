{-# OPTIONS --type-in-type #-}

module Gen where

open import Lib
open import Object

record Gen (A : Set) : Set where
  constructor gen
  field
    unGen : {R : Ty} → (A → ↑ R) → ↑ R
open Gen public

instance
  AGen : Applicative Gen
  Applicative.pure AGen a = gen λ k → k a
  Applicative._<*>_ AGen gf ga = gen λ k → unGen gf λ f → unGen ga λ a → k (f a)

  MGen : Monad Gen
  Monad._>>=_ MGen (gen ga) f = gen λ k → ga (λ a → unGen (f a) k)

runGen : ∀{A} → Gen (↑ A) → ↑ A
runGen ga = unGen ga id

record MonadGen (M : Set → Set) : Set  where
  field
    ⦃ monadM ⦄ : Monad M
    liftGen : ∀ {A} → Gen A → M A

  genLet : ∀ {A} → ↑ A → M (↑ A)
  genLet a = liftGen $ gen (Let a)

  genLetRec : ∀ {A} → (↑C A → ↑C A) → M (↑C A)
  genLetRec f = liftGen $ gen (LetRec f)

open MonadGen ⦃...⦄ public

instance
  MGenGen : MonadGen Gen
  MonadGen.liftGen MGenGen x = x

instance
  MGenStateT : ∀ {S M} ⦃ _ : MonadGen M ⦄ → MonadGen (StateT S M)
  MonadGen.liftGen MGenStateT ga = lift (liftGen ga)

  MGenReaderT : ∀ {R M} ⦃ _ : MonadGen M ⦄ → MonadGen (ReaderT R M)
  MonadGen.liftGen MGenReaderT ga = lift (liftGen ga)

  MGenMaybeT : ∀ {M} ⦃ _ : MonadGen M ⦄ → MonadGen (MaybeT M)
  MonadGen.liftGen MGenMaybeT ga = lift (liftGen ga)
