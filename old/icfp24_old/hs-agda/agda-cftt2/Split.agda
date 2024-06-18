
module Split where

open import Lib
open import Object
open import Gen

record Split (A : VTy) : Set₁ where
  field
    SplitTo  : Set
    splitGen : ∀ {E} → ↑V A → Gen E SplitTo

  split : ∀ {M}⦃ _ : MonadGen M ⦄ → ↑V A → M SplitTo
  split a = liftGen (splitGen a)

  caseM : ∀ {M B}⦃ _ : MonadGen M ⦄ → ↑V A → (SplitTo → M B) → M B
  caseM a f = do x ← split a; f x
open Split ⦃...⦄ public

data SplitListTy (A : VTy) : Set where
  nil  : SplitListTy A
  cons : ↑V A → ↑V (List∘ A) → SplitListTy A

data SplitTreeTy (A : VTy) : Set where
  leaf : SplitTreeTy A
  node : ↑V A → ↑V (Tree∘ A) → ↑V (Tree∘ A) → SplitTreeTy A

instance
  SplitBool : Split Bool∘
  Split.SplitTo SplitBool = Bool
  Split.splitGen SplitBool x = gen λ _ k → caseBool∘ x (k true) (k false)

  SplitMaybe : ∀ {A} → Split (Maybe∘ A)
  Split.SplitTo (SplitMaybe {A}) = Maybe (↑V A)
  Split.splitGen SplitMaybe x = gen λ _ k → caseMaybe∘ x (k nothing) (λ a → k (just a))

  SplitList : ∀ {A} → Split (List∘ A)
  Split.SplitTo (SplitList {A}) = SplitListTy A
  Split.splitGen (SplitList {A}) x = gen λ _ k → caseList∘ x (k nil) λ a as → k (cons a as)

  Split× : ∀ {A B} → Split (A ×∘ B)
  Split.SplitTo (Split× {A} {B}) = ↑V A × ↑V B
  Split.splitGen Split× x = gen λ _ k → case×∘ x λ a b → k (a , b)

  SplitTree : ∀ {A} → Split (Tree∘ A)
  Split.SplitTo (SplitTree {A}) = SplitTreeTy A
  Split.splitGen SplitTree x = gen λ _ k → caseTree∘ x (k leaf) (λ x l r → k (node x l r))
