
module Join where

open import Lib
open import Object
open import Gen
open import SOP

-- tabulation for continuations
--------------------------------------------------------------------------------

data ProdC : List CTy → Set where
  []   : ProdC []
  _∷_  : ∀ {A As} → ↑C A → ProdC As → ProdC (A ∷ As)

infix 3 _→ST_
_→ST_ : Uₛ → Ty → List CTy
[]    →ST B = []
a ∷ A →ST B = (a →PT B) ∷ (A →ST B)

index : ∀ {A B} → ProdC (A →ST B) → Elₛ A → ↑ B
index {a ∷ A} {B} (f ∷ fs) (here x)  = appₚₜ f x
index {a ∷ A} {B} (f ∷ fs) (there x) = index fs x

tabulate : ∀ {A B} → (Elₛ A → ↑ B) → ProdC (A →ST B)
tabulate {[]}    {B} f = []
tabulate {a ∷ A} {B} f = (lamₚₜ (f ∘ here)) ∷ tabulate (f ∘ there)

genLetPC : ∀ {A} → ProdC A → Gen (ProdC A)
genLetPC []       = pure []
genLetPC (x ∷ xs) = _∷_ <$> genLet x <*> genLetPC xs

--------------------------------------------------------------------------------

record MonadJoin (M : Set → Set) : Set₁ where
  field
    join : ∀ {A} ⦃ _ : IsSOP A ⦄ → M A → M A
open MonadJoin ⦃...⦄ public

instance
  MonadJoinGen : MonadJoin Gen
  MonadJoin.join MonadJoinGen ma = gen λ k → runGen do
    conts ← genLetPC (tabulate (k ∘ decode))
    a ← ma
    pure (index conts (encode a))

  MonadJoinMaybeT : ∀ {M}⦃ _ : MonadJoin M ⦄ → MonadJoin (MaybeT M)
  MonadJoin.join MonadJoinMaybeT (maybeT ma) = maybeT (join ma)

  MonadJoinReaderT : ∀ {R M}⦃ _ : MonadJoin M ⦄ → MonadJoin (ReaderT R M)
  MonadJoin.join MonadJoinReaderT (readerT ma) = readerT (join ∘ ma)

  MonadJoinStateT : ∀ {S M}⦃ _ : MonadJoin M ⦄ ⦃ _ : IsSOP S ⦄ → MonadJoin (StateT S M)
  MonadJoin.join MonadJoinStateT (stateT ma) = stateT (join ∘ ma)
