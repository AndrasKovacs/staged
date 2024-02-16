
module MonadTailRec where

open import Lib
open import Object
open import Gen
open import Improve
open import SOP

data TailCall A B : Set where
  call : A → TailCall A B
  exit : B → TailCall A B

mapCall : ∀ {M A A' B} ⦃ _ : Applicative M ⦄ → (A → A') → M (TailCall A B) → M (TailCall A' B)
mapCall f mab = mab <&> λ where
  (call a) → call (f a)
  (exit b) → exit b

bindCall : ∀ {M A A' B} ⦃ _ : Monad M ⦄ → M (TailCall A B) → (A → M A') → M (TailCall A' B)
bindCall mab f = mab >>= λ where
  (call a) → call <$> f a
  (exit b) → pure $ exit b

record MonadTailRec (F : VTy → Ty)(M : Set → Set) : Set where
  field
     goTailRec : ∀ {A B} → (Elₚ A → M (TailCall (Elₚ A) (↑V B))) → (Elₚ A → ↑ (F B)) → Elₚ A → ↑ (F B)

  downRecM : ∀ {A B}⦃ _ : Applicative M ⦄ → (↑V A → M (TailCall (↑V A) (↑V B))) → ↑C (A ⇒ F B)
  downRecM f = LetRec _ (λ rec → Λ λ x →
                             goTailRec (λ x → mapCall (_∷ []) (f (headₚ x)))
                                       (λ x → rec ∙ headₚ x)
                                       (x ∷ []))
                        id

open MonadTailRec ⦃...⦄ public

instance
o  MTRIdentity : MonadTailRec Identity∘ Gen
  MonadTailRec.goTailRec MTRIdentity f rec arg = identity∘ $ unGen (f arg) λ where
     (call a) → runIdentity∘ (rec a)
     (exit b) → b

  MRTReaderT : ∀ {R F M}⦃ _ : MonadTailRec F M ⦄ ⦃ _ : Monad M ⦄
              → MonadTailRec (ReaderT∘ R F) (ReaderT (↑V R) M)
  MonadTailRec.goTailRec (MRTReaderT {R} {F} {M}) {A} {B} f rec x = readerT∘ λ r →
    goTailRec {A = R ∷ A} (λ {(r ∷ x) → runReaderT (bindCall (f x) (λ a → (_∷ a) <$> ask)) r})
              (λ {(r ∷ x) → runReaderT∘ (rec x) r})
              (r ∷ x)

  MRTMaybeT : ∀ {F M}⦃ _ : MonadTailRec F M ⦄ ⦃ _ : Monad M ⦄
               → MonadTailRec (MaybeT∘ F) (MaybeT M)
  MonadTailRec.goTailRec MRTMaybeT f rec x = maybeT∘ $
    goTailRec (λ x → runMaybeT (f x) <&> λ where
       nothing         → exit nothing∘
       (just (call x)) → call x
       (just (exit x)) → exit (just∘ x))
       (λ x → runMaybeT∘ (rec x))
       x

  MRTStateT : ∀ {S F M}⦃ _ : MonadTailRec F M ⦄ ⦃ _ : Monad M ⦄
              → MonadTailRec (StateT∘ S F) (StateT (↑V S) M)
  MonadTailRec.goTailRec MRTStateT f rec x = stateT∘ λ s →
    goTailRec
      (λ {(s ∷ x) → runStateT (f x) s <&> λ where
         (call a , s) → call (s ∷ a)
         (exit b , s) → exit (b ,∘ s)})
      (λ {(s ∷ x) → runStateT∘ (rec x) s})
      (s ∷ x)
