
module TailRec where

open import Lib
open import Object
open import Gen
open import Improve

record TailRecT (Arg : VTy) (M : Set → Set) (A : Set) : Set where
  constructor tailRecT
  field
    runTailRecT : M (Either (↑V Arg) A)
open TailRecT public

instance
  ATRT : ∀ {Arg M}⦃ _ : Applicative M ⦄ → Applicative (TailRecT Arg M)
  Applicative.pure ATRT a = tailRecT (pure (right a))
  Applicative._<*>_ ATRT (tailRecT mf) (tailRecT ma) = tailRecT (_<*>_ <$> mf <*> ma)

  MTRT : ∀ {Arg M}⦃ _ : Monad M ⦄ → Monad (TailRecT Arg M)
  Monad._>>=_ MTRT (tailRecT ma) f = tailRecT $ ma >>= λ where
    (left v)  → pure (left v)
    (right a) → runTailRecT (f a)

  MTTRT : ∀ {Arg} → MonadTrans (TailRecT Arg)
  MonadTrans.lift MTTRT ma = tailRecT (right <$> ma)

makeTailRec : ∀ {F M Arg A}⦃ _ : Improve (C ∘ F) M ⦄ → TailRecT Arg M (↑V A) → ↑C (F Arg)
makeTailRec {F} {M} {Arg} {A} (tailRecT ma) =
  LetRec {!!}
         {!!}
         {!!}
