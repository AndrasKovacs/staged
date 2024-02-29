
module MonadTailRec where

open import Lib
open import Object
open import Gen
open import Improve
open import SOP

-- Note: relies on either injective ↑, C, V, or TailCall being
-- large, or equalities of Set being small. I use injective ↑V here.
data TailCall (F : VTy → Ty) : Set → Set where
  call' : ∀ {A B}(f : Elₚ B → ↑ (F A))(arg : Elₚ B) → TailCall F (↑V A)
  ret'  : ∀ {A} → A → TailCall F A

record MonadTC (F : VTy → Ty)(M : Set → Set) : Set where
  field
    downTC : ∀ {A} → M (TailCall F (↑V A)) → ↑ (F A)

  ret : ∀ {A}⦃ _ : Applicative M ⦄ → A → M (TailCall F A)
  ret a = pure $ ret' a

  tailcall1 : ∀ {A B}⦃ _ : Applicative M ⦄(f : ↑C (B ⇒ F A)) → ↑V B → M (TailCall F (↑V A))
  tailcall1 {A}{B} f x = pure (call' {B = B ∷ []}(λ x → f ∙ (headₚ x)) (x ∷ []))

  tailcall2 : ∀ {A B₀ B₁}⦃ _ : Applicative M ⦄(f : ↑C (B₀ ⇒ C (B₁ ⇒ F A))) → ↑V B₀ → ↑V B₁
          → M (TailCall F (↑V A))
  tailcall2 f x y = pure $ call' (appₚₜ f) (x ∷ y ∷ [])

  tailcall3 : ∀ {A B₀ B₁ B₂}⦃ _ : Applicative M ⦄(f : ↑C (B₀ ⇒ C (B₁ ⇒ C (B₂ ⇒ F A))))
          → ↑V B₀ → ↑V B₁ → ↑V B₂ → M (TailCall F (↑V A))
  tailcall3 f x y z = pure $ call' (appₚₜ f) (x ∷ y ∷ z ∷ [])

open MonadTC ⦃...⦄ public

instance
  MTCIdentity : MonadTC Identity∘ Gen
  MonadTC.downTC MTCIdentity m = identity∘ $ unGen m λ where
    (call' f as) → runIdentity∘ (f as)
    (ret' a)     → a

  MTCReaderT : ∀ {R F M}⦃ _ : MonadTC F M ⦄ ⦃ _ : Monad M ⦄
               → MonadTC (ReaderT∘ R F) (ReaderT (↑V R) M)
  MonadTC.downTC MTCReaderT m = readerT∘ λ r →
     downTC $ flip runReaderT r $ m >>= λ where
         (call' f as) → do r ← ask; pure $ call' (λ {(r ∷ x) → runReaderT∘ (f x) r}) (r ∷ as)
         (ret' a)     → pure $ ret' a

  MTCMaybeT : ∀ {F M}⦃ _ : MonadTC F M ⦄ ⦃ _ : Monad M ⦄
               → MonadTC (MaybeT∘ F) (MaybeT M)
  MonadTC.downTC MTCMaybeT m = maybeT∘ $ downTC $ runMaybeT m >>= λ where
    (just (call' f as)) → pure $ call' (runMaybeT∘ ∘ f) as
    (just (ret' x))     → pure $ ret' $ just∘ x
    nothing            → pure $ ret' nothing∘

  MRTStateT : ∀ {S F M}⦃ _ : MonadTC F M ⦄ ⦃ _ : Monad M ⦄
              → MonadTC (StateT∘ S F) (StateT (↑V S) M)
  MonadTC.downTC MRTStateT m = stateT∘ λ s → downTC $ flip runStateT s m >>= λ where
    (call' f as , s) → pure (call' (λ {(s ∷ as) → runStateT∘ (f as) s}) (s ∷ as))
    (ret' a     , s) → pure (ret' (a ,∘ s))
