
{-# OPTIONS --type-in-type #-}

module Join where

open import Lib
open import Object
open import Gen
open import SOP
open import Split

-- tabulation for continuations
--------------------------------------------------------------------------------

data ProdC : List CTy → Set where
  []   : ProdC []
  _∷_  : ∀ {A As} → ↑C A → ProdC As → ProdC (A ∷ As)

infix 3 _→PT_
_→PT_ : Uₚ → Ty → CTy
[]    →PT B = ⊤∘ ⇒ B
a ∷ A →PT B = a ⇒ C (A →PT B)

infix 3 _→ST_
_→ST_ : Uₛ → Ty → List CTy
[]    →ST B = []
a ∷ A →ST B = (a →PT B) ∷ (A →ST B)

appₚₜ : ∀ {A B} → ↑C (A →PT B) → Elₚ A → ↑ B
appₚₜ {[]}    {B} f []       = f ∙ tt∘
appₚₜ {a ∷ A} {B} f (x ∷ xs) = appₚₜ {A}{B} (f ∙ x) xs

lamₚₜ : ∀ {A B} → (Elₚ A → ↑ B) → ↑C (A →PT B)
lamₚₜ {[]}    {B} f = Λ λ _ → f []
lamₚₜ {a ∷ A} {B} f = Λ λ a → lamₚₜ {A}{B} (λ as → f (a ∷ as))

index : ∀ {A B} → ProdC (A →ST B) → Elₛ A → ↑ B
index {a ∷ A} {B} (f ∷ fs) (here x)  = appₚₜ f x
index {a ∷ A} {B} (f ∷ fs) (there x) = index fs x

tabulate : ∀ {A B} → (Elₛ A → ↑ B) → ProdC (A →ST B)
tabulate {[]}    {B} f = []
tabulate {a ∷ A} {B} f = (lamₚₜ (f ∘ here)) ∷ tabulate (f ∘ there)

genLetₚ : ∀ {A} → ProdC A → Gen (ProdC A)
genLetₚ []       = return []
genLetₚ (x ∷ xs) = _∷_ <$> genLet x <*> genLetₚ xs

--------------------------------------------------------------------------------

record MonadJoin (M : Set → Set) : Set where
  field
    ⦃ monadM ⦄ : Monad M
    join : ∀ {A} ⦃ _ : IsSOP A ⦄ → M A → M A

  case'' : ∀ {A B}⦃ _ : Split A ⦄ ⦃ _ : IsSOP B ⦄ ⦃ _ : MonadGen M ⦄ → ↑V A → (SplitTo {A} → M B) → M B
  case'' a f = join (case' a f)
open MonadJoin ⦃...⦄ public

instance
  MonadJoinGen : MonadJoin Gen
  MonadJoin.join MonadJoinGen ma = gen λ k → runGen do
    conts ← genLetₚ (tabulate (k ∘ decode))
    a ← ma
    return (index conts (encode a))

  MonadJoinMaybeT : ∀ {M}⦃ _ : MonadJoin M ⦄ → MonadJoin (MaybeT M)
  MonadJoin.join MonadJoinMaybeT (maybeT ma) = maybeT (join ma)

  MonadJoinReaderT : ∀ {R M}⦃ _ : MonadJoin M ⦄ → MonadJoin (ReaderT R M)
  MonadJoin.join MonadJoinReaderT (readerT ma) = readerT (join ∘ ma)

  MonadJoinStateT : ∀ {S M}⦃ _ : MonadJoin M ⦄ ⦃ _ : IsSOP S ⦄ → MonadJoin (StateT S M)
  MonadJoin.join MonadJoinStateT (stateT ma) = stateT (join ∘ ma)
