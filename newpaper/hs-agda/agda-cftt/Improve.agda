{-# OPTIONS --large-indices #-}

module Improve where

open import Lib
open import Object
open import Gen
open import Split
open import SOP

data TailCall (F : VTy → Ty) (B : VTy) : Set where
  call : ∀ {A}(f : Elₚ A → ↑ (F B))(arg : Elₚ A) → TailCall F B

data Ret (F : VTy → Ty) : Set → Set where
  call : ∀ {A} → TailCall F A → Ret F (↑V A)
  ret' : ∀ {A} → A → Ret F A

-- -- Note: relies on either injective ↑, C, V, or TailCall being
-- -- large, or equalities of Set being small. I use injective ↑V here.
-- data TailCall (F : VTy → Ty) : Set → Set where
--   call' : ∀ {A B}(f : Elₚ B → ↑ (F A))(arg : Elₚ B) → TailCall F (↑V A)
--   ret'  : ∀ {A} → A → TailCall F A

-- record Improve (F : VTy → Ty) (M : Set → Set) : Set₁ where
--   field
--     ⦃ MGM ⦄ : MonadGen M
--     up      : ∀ {A} → ↑ (F A) → M (↑V A)
--     downTC  : ∀ {A} → M (TailCall F (↑V A)) → ↑ (F A)

--   down : ∀ {A} → M (↑V A) → ↑ (F A)
--   down ma = downTC (ret' <$> ma)

--   upTC : ∀ {A} → TailCall F A → M A
--   upTC (call' f arg) = up (f arg)
--   upTC (ret' x)      = pure x

--   ret : ∀ {A} → A → M (TailCall F A)
--   ret a = pure $ ret' a

--   -- shorthands for tailcalls with concrete arity
--   tailcall1 : ∀ {A B}(f : ↑C (B ⇒ F A)) → ↑V B → M (TailCall F (↑V A))
--   tailcall1 {A}{B} f x = pure (call' {B = B ∷ []}(λ x → f ∙ (headₚ x)) (x ∷ []))

--   tailcall2 : ∀ {A B₀ B₁}(f : ↑C (B₀ ⇒ C (B₁ ⇒ F A))) → ↑V B₀ → ↑V B₁
--           → M (TailCall F (↑V A))
--   tailcall2 f x y = pure $ call' (appₚₜ f) (x ∷ y ∷ [])

--   tailcall3 : ∀ {A B₀ B₁ B₂}(f : ↑C (B₀ ⇒ C (B₁ ⇒ C (B₂ ⇒ F A))))
--           → ↑V B₀ → ↑V B₁ → ↑V B₂ → M (TailCall F (↑V A))
--   tailcall3 f x y z = pure $ call' (appₚₜ f) (x ∷ y ∷ z ∷ [])
-- open Improve ⦃...⦄ public

-- instance
--   ImpIdentity : Improve Identity∘ Gen
--   Improve.up   ImpIdentity x = pure (runIdentity∘ x)
--   Improve.downTC ImpIdentity m = identity∘ $ unGen m λ where
--     (call' f as) → runIdentity∘ (f as)
--     (ret' a)     → a

--   ImpMaybeT : ∀ {F M} → ⦃ _ : Improve F M ⦄ → Improve (MaybeT∘ F) (MaybeT M)
--   Improve.up ImpMaybeT x = maybeT do x ← up (runMaybeT∘ x); split x
--   Improve.downTC ImpMaybeT x = maybeT∘ $ downTC $ runMaybeT x >>= λ where
--     (just (call' f as)) → pure $ call' (runMaybeT∘ ∘ f) as
--     (just (ret' x))     → pure $ ret' $ just∘ x
--     nothing             → pure $ ret' nothing∘

--   ImpStateT : ∀ {F M S} → ⦃ _ : Improve F M ⦄ → Improve (StateT∘ S F) (StateT (↑V S) M)
--   Improve.up   ImpStateT x   = stateT λ s → do as ← up (runStateT∘ x s); split as
--   Improve.downTC ImpStateT m = stateT∘ λ s → downTC $ flip runStateT s m >>= λ where
--     (call' f as , s) → pure (call' (λ {(s ∷ as) → runStateT∘ (f as) s}) (s ∷ as))
--     (ret' a     , s) → pure (ret' (a ,∘ s))

--   ImpReaderT : ∀ {F M S} → ⦃ _ : Improve F M ⦄ → Improve (ReaderT∘ S F) (ReaderT (↑V S) M)
--   Improve.up ImpReaderT   x = readerT (up ∘ runReaderT∘ x)
--   Improve.downTC ImpReaderT m = readerT∘ λ r →
--      downTC $ flip runReaderT r $ m >>= λ where
--          (call' f as) → do r ← ask; pure $ call' (λ {(r ∷ x) → runReaderT∘ (f x) r}) (r ∷ as)
--          (ret' a)     → pure $ ret' a
