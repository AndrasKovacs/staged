
module Join where

open import Lib
open import Object
open import Gen
open import SOP
open import Improve

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

genFunSOP↑ : ∀ {A B} → FunSOP↑ A B → Gen (FunSOP↑ A B)
genFunSOP↑ {[]}    fs       = pure tt
genFunSOP↑ {A ∷ B} (f , fs) = _,_ <$> genLet f <*> genFunSOP↑ {B} fs

--------------------------------------------------------------------------------

record MonadJoin (M : Set → Set) : Set₁ where
  field
    join : ∀ {F E A} ⦃ _ : IsSOP A ⦄ → TCT F E M A → TCT F E M A
open MonadJoin ⦃...⦄ public

instance
  MJGen : MonadJoin Gen
  MonadJoin.join MJGen ma = tct $ gen $ λ k → runGen do
    conts ← genFunSOP↑ (tabulate (k ∘ ret' ∘ decode))
    runTCT ma <&> λ where
      (tailcall' x) → k (tailcall' x)
      (tailret' x)  → k (tailret' x)
      (ret' x)      → index conts (encode x)

  MJMaybeT : ∀ {M}⦃ _ : MonadJoin M ⦄ ⦃ _ : Monad M ⦄ → MonadJoin (MaybeT M)
  MonadJoin.join (MJMaybeT {M}) {F} {E} {A} (tct (maybeT m)) =
    tct $ maybeT $
        (runTCT $ join {M}{F}{E}{Maybe A} $ tct $ (m <&> λ where
             (just (tailcall' x)) → tailcall' x
             (just (tailret' x))  → tailret' x
             (just (ret' x))      → ret' (just x)
             nothing              → ret' nothing))
        <&> λ where
           (tailcall' x)   → just (tailcall' x)
           (tailret' x)    → just (tailret' x)
           (ret' (just x)) → just (ret' x)
           (ret' nothing)  → nothing

  MJReaderT : ∀ {R M}⦃ _ : MonadJoin M ⦄ → MonadJoin (ReaderT R M)
  MonadJoin.join MJReaderT (tct (readerT ma)) = tct $ readerT λ r → runTCT $ join $ tct (ma r)

  MJStateT : ∀ {S M}⦃ _ : MonadJoin M ⦄ ⦃ _ : IsSOP S ⦄ ⦃ _ : Monad M ⦄ → MonadJoin (StateT S M)
  MonadJoin.join (MJStateT {S} {M}) {F} {E} {A} (tct (stateT ma)) =
    tct $ stateT $ λ s → runTCT (join {M}{F × S}{E × S} {A × S} (tct (ma s <&> λ where
      (tailcall' x , s) → tailcall' (x , s)
      (tailret' x  , s) → tailret' (x , s)
      (ret' x      , s) → ret' (x , s))))
      <&> λ where
      (tailcall' (x , s)) → tailcall' x , s
      (tailret' (x , s))  → tailret' x , s
      (ret' (x , s))      → ret' x , s
