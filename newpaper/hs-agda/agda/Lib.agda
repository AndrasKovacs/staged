
module Lib where

open import Relation.Binary.PropositionalEquality
  renaming (subst to tr; sym to infix 5 _⁻¹; trans to infixr 4 _◼_;
            cong to ap)
  hiding ([_])
  public

open import Data.Product
  renaming (proj₁ to ₁; proj₂ to ₂)
  hiding (_<*>_)
  public

open import Data.Maybe hiding (ap; map; zip) public
open import Data.Nat hiding (_+_; _*_) public
open import Data.Nat.Properties public
open import Data.Nat.Induction public
open import Function public
open import Data.Unit using (⊤; tt) public
open import Data.Empty public

--------------------------------------------------------------------------------

_⊗_ :
  ∀ {α β}{A : Set α}{B : Set β}
    {f g : A → B}(p : f ≡ g){a a' : A}(q : a ≡ a')
  → f a ≡ g a'
refl ⊗ refl = refl
infixl 8 _⊗_

return : ∀ {i}{A : Set i} → A → Maybe A
return = just

infixl 1 _>>_
_>>_ : ∀ {i j}{A : Set i}{B : Set j} → Maybe A → Maybe B → Maybe B
ma >> mb = ma >>= λ _ → mb

infixl 4 _<$>_
_<$>_ : ∀ {i j}{A : Set i}{B : Set j} → (A → B) → Maybe A → Maybe B
f <$> ma = do
  a ← ma
  just (f a)

infixl 4 _<*>_
_<*>_ : ∀ {i j}{A : Set i}{B : Set j} → Maybe (A → B) → Maybe A → Maybe B
mf <*> ma = do
  f ← mf
  a ← ma
  just (f a)
