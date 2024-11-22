
module Join where

open import Lib
open import Object
open import Gen
open import SOP

-- tabulation for continuations
--------------------------------------------------------------------------------

FunSOP↑ : Uₛ → Ty → Set
FunSOP↑ []      R = ⊤
FunSOP↑ (A ∷ B) R = ↑C (A →PT R) × FunSOP↑ B R

index : ∀ {A B} → FunSOP↑ A B → Elₛ A → ↑ B
index fs (here  x) = appₚₜ (fs .₁) x
index fs (there x) = index (fs .₂) x

tabulate : ∀ {A B} → (Elₛ A → ↑ B) → FunSOP↑ A B
tabulate {[]}    f = tt
tabulate {A ∷ B} f = lamₚₜ (f ∘ here) , tabulate (f ∘ there)

genFunSOP↑ : ∀ {A B} → FunSOP↑ A B → Gen ⊥ (FunSOP↑ A B)
genFunSOP↑ {[]}    fs       = pure tt
genFunSOP↑ {A ∷ B} (f , fs) = _,_ <$> genLet f <*> genFunSOP↑ {B} fs

--------------------------------------------------------------------------------

record MonadJoin (M : Set → Set) : Set₁ where
  field
    join : ∀ {A} ⦃ _ : IsSOP A ⦄ → M A → M A
open MonadJoin ⦃...⦄ public

instance
  MonadJoinGen : ∀ {E} → MonadJoin (Gen E)
  MonadJoin.join MonadJoinGen ma = gen λ l r →
    unGen (genFunSOP↑ (tabulate (r ∘ decode))) (λ ()) λ conts →
    unGen ma l λ a →
    index conts (encode a)

  MonadJoinMaybeT : ∀ {M}⦃ _ : MonadJoin M ⦄ → MonadJoin (MaybeT M)
  MonadJoin.join MonadJoinMaybeT (maybeT ma) = maybeT (join ma)

  MonadJoinReaderT : ∀ {R M}⦃ _ : MonadJoin M ⦄ → MonadJoin (ReaderT R M)
  MonadJoin.join MonadJoinReaderT (readerT ma) = readerT (join ∘ ma)

  MonadJoinStateT : ∀ {S M}⦃ _ : MonadJoin M ⦄ ⦃ _ : IsSOP S ⦄ → MonadJoin (StateT S M)
  MonadJoin.join MonadJoinStateT (stateT ma) = stateT (join ∘ ma)
