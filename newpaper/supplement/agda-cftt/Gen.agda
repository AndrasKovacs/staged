
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

record MonadGen (M : Set → Set) : Set₁  where
  field
    ⦃ monadM ⦄ : Monad M
    liftGen : ∀ {A} → Gen A → M A

  -- corresponds to "gen" in the paper
  genLet : ∀ {A} → ↑ A → M (↑ A)
  genLet a = liftGen $ gen (Let a)

  -- "corresponds to "genRec" in the paper
  genLetRec : ∀ {A} → (↑C A → ↑C A) → M (↑C A)
  genLetRec f = liftGen $ gen (LetRec _ f)
open MonadGen ⦃...⦄ public


--------------------------------------------------------------------------------
-- We use this class because the MonadReader definition in Agda is defined with
-- an associate type for the environment, instead of the additional class
-- parameter as in Haskell.

-- We do this to get better type inference. Haskell relies on a functional
-- parameter dependency, but there is no such feature in Agda.

-- Since we can't just say "MonadReader (↑ R) M", we need an additional
-- class to restrict the reader environment to lifted types.

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

--------------------------------------------------------------------------------

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

put' : ∀ {M}⦃ _ : MonadState M ⦄ ⦃ _ : IsTy (StateTy {M}) ⦄ ⦃ _ : MonadGen M ⦄ → StateTy {M} → M (↑V ⊤∘)
put' s = do s ← genLetIsTy s; put s; pure tt∘

modify' : ∀ {M}⦃ _ : MonadState M ⦄ ⦃ _ : IsTy (StateTy {M}) ⦄ ⦃ _ : MonadGen M ⦄
          → (StateTy {M} → StateTy {M}) → M (↑V ⊤∘)
modify' f = do s ← get; put' (f s)
