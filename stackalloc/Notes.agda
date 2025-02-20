{-# OPTIONS --type-in-type #-}

open import Data.Nat

infixr 3 _⇒_

postulate
  Ty  : Set
  ↑   : Ty → Set

  VTy : Set
  V   : VTy → Ty

  CTy : Set
  C   : CTy → Ty

↑C = λ A → ↑ (C A)
↑V = λ A → ↑ (V A)

postulate
  Loc   : Set
  Locs  : Set
  LNil  : Locs
  LCons : Loc → Locs → Locs

  Let    : ∀ {A B} → ↑ A → (↑ A → ↑ B) → ↑ B
  LetRec : ∀ A {B} → (↑C A → ↑C A) → (↑C A → ↑ B) → ↑ B

  _⇒_ : VTy → Ty → CTy
  Λ   : ∀ {A B} → (↑V A → ↑ B) → ↑C (A ⇒ B)
  _∙_ : ∀ {A B} → ↑C (A ⇒ B) → (↑V A → ↑ B)

  ⊤∘  : VTy
  tt∘ : ↑V ⊤∘

  Maybe∘     : VTy → VTy
  just∘     : ∀ {A} → ↑V A → ↑V (Maybe∘ A)
  nothing∘   : ∀ {A} → ↑V (Maybe∘ A)
  caseMaybe∘ : ∀ {A B} → ↑V (Maybe∘ A) → ↑ B → (↑V A → ↑ B) → ↑ B

  Bool∘  : VTy
  true∘  : ↑V Bool∘
  false∘ : ↑V Bool∘
  caseBool∘ : ∀ {A} → ↑V Bool∘ → ↑ A → ↑ A → ↑ A

  ℕ∘    : VTy
  lit∘  : ℕ → ↑V ℕ∘
  _+∘_  : ↑V ℕ∘ → ↑V ℕ∘ → ↑V ℕ∘
  _*∘_  : ↑V ℕ∘ → ↑V ℕ∘ → ↑V ℕ∘
  _-∘_  : ↑V ℕ∘ → ↑V ℕ∘ → ↑V ℕ∘
  _==∘_ : ↑V ℕ∘ → ↑V ℕ∘ → ↑V Bool∘
  _<∘_  : ↑V ℕ∘ → ↑V ℕ∘ → ↑V Bool∘

  _×∘_   : VTy → VTy → VTy
  _,∘_   : ∀ {A B} → ↑V A → ↑V B → ↑V (A ×∘ B)
  case×∘ : ∀ {A B C} → ↑V (A ×∘ B) → (↑V A → ↑V B → ↑ C) → ↑ C

  _×C_ : CTy → CTy → CTy
  _,C_ : ∀ {A B} → ↑C A → ↑C B → ↑C (A ×C B)
  fst∘ : ∀ {A B} → ↑C (A ×C B) → ↑C A
  snd∘ : ∀ {A B} → ↑C (A ×C B) → ↑C B
  ⊤C   : CTy
  ttC  : ↑C ⊤C

  List∘     : VTy → Locs → VTy
  nil∘      : ∀ {A} → ↑V (List∘ A LNil)
  cons∘     : ∀ {A R ls} → ↑V A → ↑V (List∘ A ls) → (∀ {l} → ↑V (List∘ A (LCons l ls)) → ↑ R) → ↑ R
  caseList∘ : ∀ {A ls}(P : Locs → Ty) → ↑V (List∘ A ls) → ↑ (P LNil) → (∀ {l ls} → ↑V A → ↑V (List∘ A ls) → ↑ (P (LCons l ls))) → ↑ (P ls)

map : ∀{A B ls} → (↑V A → ↑V B) → ↑V (List∘ A ls) → ∀ {ls'} → ↑V
map = {!!}
