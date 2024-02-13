
{-# OPTIONS --type-in-type #-}

module Join where

open import Lib
open import Object
open import Gen
open import SumV
open import Split

record MonadJoin (M : Set → Set) : Set where
  field
    ⦃ monadM ⦄ : Monad M
    join : ∀ {A} ⦃ _ : IsSumV A ⦄ → M A → M A

  case'' : ∀ {A B}⦃ _ : Split A ⦄ ⦃ _ : IsSumV B ⦄ ⦃ _ : MonadGen M ⦄ → ↑V A → (SplitTo {A} → M B) → M B
  case'' a f = join (case' a f)
open MonadJoin ⦃...⦄ public

instance
  MonadJoinGen : MonadJoin Gen
  MonadJoin.join MonadJoinGen ma = gen λ k → runGen do
    conts ← genLetPC (tabulate (k ∘ decode))
    a ← ma
    return (index conts (encode a))

  MonadJoinMaybeT : ∀ {M}⦃ _ : MonadJoin M ⦄ → MonadJoin (MaybeT M)
  MonadJoin.join MonadJoinMaybeT (maybeT ma) = maybeT (join ma)

  MonadJoinReaderT : ∀ {R M}⦃ _ : MonadJoin M ⦄ → MonadJoin (ReaderT R M)
  MonadJoin.join MonadJoinReaderT (readerT ma) = readerT (join ∘ ma)

  MonadJoinStateT : ∀ {S M}⦃ _ : MonadJoin M ⦄ ⦃ _ : IsSumV S ⦄ → MonadJoin (StateT S M)
  MonadJoin.join MonadJoinStateT (stateT ma) = stateT (join ∘ ma)
