
module Gen where

open import Lib
open import Object

record Gen (A : Set i) : Set i where
  constructor gen
  field
    unGen : {R : Ty} → (A → ↑ R) → ↑ R
open Gen public

instance
  MGen : Monad {i} Gen
  Monad.return MGen a         = gen λ k → k a
  Monad._>>=_ MGen (gen ga) f = gen λ k → ga (λ a → unGen (f a) k)

runGen : ∀{A} → Gen (↑ A) → ↑ A
runGen ga = unGen ga id

record MonadGen (M : Set i → Set i) : Set (lsuc i) where
  field
    ⦃ monadM ⦄ : Monad M
    liftGen : ∀ {A} → Gen A → M A
open MonadGen ⦃...⦄

instance
  MGenGen : MonadGen {i} Gen
  MonadGen.liftGen MGenGen x = x

instance
  MGenStateT : ∀ {S M} ⦃ _ : MonadGen {i} M ⦄ → MonadGen (StateT S M)
  MonadGen.liftGen MGenStateT ga = lift (liftGen ga)

  MGenReaderT : ∀ {R M} ⦃ _ : MonadGen {i} M ⦄ → MonadGen (ReaderT R M)
  MonadGen.liftGen MGenReaderT ga = lift (liftGen ga)
