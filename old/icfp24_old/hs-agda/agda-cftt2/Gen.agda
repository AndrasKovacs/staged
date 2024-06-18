
module Gen where

open import Lib
open import Object

record Gen (E A : Set) : Set where
  constructor gen
  field
    unGen : {R : Ty} → (E → ↑ R) → (A → ↑ R) → ↑ R
open Gen public

instance
  AGen : ∀ {E} → Applicative (Gen E)
  Applicative.pure AGen a = gen λ l r → r a
  Applicative._<*>_ AGen gf ga = gen λ l r → unGen gf l λ f → unGen ga l λ a → r (f a)

  MGen : ∀ {E} → Monad (Gen E)
  Monad._>>=_ MGen (gen ga) f = gen λ l r → ga l (λ a → unGen (f a) l r)

record MonadGen (M : Set → Set) : Set₁  where
  field
    ⦃ monadM ⦄ : Monad M
    TailTy  : Set
    liftGen : ∀ {A} → Gen TailTy A → M A

  genLet : ∀ {A} → ↑ A → M (↑ A)
  genLet a = liftGen $ gen λ _ → Let a

  genLetRec : ∀ {A} → (↑C A → ↑C A) → M (↑C A)
  genLetRec f = liftGen $ gen λ _ → LetRec _ f
open MonadGen ⦃...⦄ public

record IsTy (A : Set) : Set₁ where
  field
    A'   : Ty
    isTy : A ≡ ↑ A'

  genLetIsTy : ∀ {M} ⦃ _ : MonadGen M ⦄ → A → M A
  genLetIsTy {M} a = tr M (isTy ⁻¹) (genLet (coe isTy a))
open IsTy ⦃...⦄

instance
  IsTyTy : ∀ {A} → IsTy (↑ A)
  IsTy.A' (IsTyTy {A}) = A
  IsTy.isTy IsTyTy = refl

instance
  MGGen : ∀ {E} → MonadGen (Gen E)
  MonadGen.TailTy (MGGen {E}) = E
  MonadGen.liftGen MGGen x = x

  MGStateT : ∀ {S M} ⦃ _ : MonadGen M ⦄ → MonadGen (StateT S M)
  MonadGen.TailTy (MGStateT {M = M}) = TailTy {M}
  MonadGen.liftGen MGStateT = lift ∘ liftGen

  MGReaderT : ∀ {S M} ⦃ _ : MonadGen M ⦄ → MonadGen (ReaderT S M)
  MonadGen.TailTy (MGReaderT {M = M}) = TailTy {M}
  MonadGen.liftGen MGReaderT = lift ∘ liftGen

  MGMaybeT : ∀ {M} ⦃ _ : MonadGen M ⦄ → MonadGen (MaybeT M)
  MonadGen.TailTy (MGMaybeT {M = M}) = TailTy {M}
  MonadGen.liftGen MGMaybeT = lift ∘ liftGen

--------------------------------------------------------------------------------

local' : ∀ {M A}
  ⦃ _ : MonadReader M ⦄
  ⦃ _ : IsTy (ReaderTy {M}) ⦄
  ⦃ _ : MonadGen M ⦄
  → (ReaderTy {M} → ReaderTy {M}) → M A → M A
local' f ma = do
  r ← ask
  r ← genLetIsTy (f r)
  local (λ _ → r) ma

put' : ∀ {M}⦃ _ : MonadState M ⦄ ⦃ _ : IsTy (StateTy {M}) ⦄ ⦃ _ : MonadGen M ⦄
       → StateTy {M} → M (↑V ⊤∘)
put' s = do s ← genLetIsTy s; put s; pure tt∘

modify' : ∀ {M}⦃ _ : MonadState M ⦄ ⦃ _ : IsTy (StateTy {M}) ⦄ ⦃ _ : MonadGen M ⦄
          → (StateTy {M} → StateTy {M}) → M (↑V ⊤∘)
modify' f = do s ← get; put' (f s)
