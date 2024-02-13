
module Object where

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

infixl 8 _∙_
infixl 5 _+∘_
infixl 6 _*∘_
infix 4 _==_
infixr 4 _×∘_

postulate
  Let    : ∀ {A B} → ↑ A → (↑ A → ↑ B) → ↑ B
  LetRec : ∀ {A B} → (↑C A → ↑C A) → (↑C A → ↑ B) → ↑ B

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

  ℕ∘   : VTy
  lit  : ℕ → ↑V ℕ∘
  _+∘_ : ↑V ℕ∘ → ↑V ℕ∘ → ↑V ℕ∘
  _*∘_ : ↑V ℕ∘ → ↑V ℕ∘ → ↑V ℕ∘
  _==_ : ↑V ℕ∘ → ↑V ℕ∘ → ↑V Bool∘

  _×∘_   : VTy → VTy → VTy
  _,∘_   : ∀ {A B} → ↑V A → ↑V B → ↑V (A ×∘ B)
  case×∘ : ∀ {A B C} → ↑V (A ×∘ B) → (↑V A → ↑V B → ↑ C) → ↑ C
  fst∘   : ∀ {A B} → ↑V (A ×∘ B) → ↑V A
  snd∘   : ∀ {A B} → ↑V (A ×∘ B) → ↑V B

  List∘     : VTy → VTy
  nil∘      : ∀ {A} → ↑V (List∘ A)
  cons∘     : ∀ {A} → ↑V A → ↑V (List∘ A) → ↑V (List∘ A)
  caseList∘ : ∀ {A B} → ↑V (List∘ A) → ↑ B → (↑V A → ↑V (List∘ A) → ↑ B) → ↑ B

  StateT∘    : VTy → (VTy → Ty) → (VTy → Ty)
  stateT∘    : ∀ {S M A} → (↑V S → ↑ (M(A ×∘ S))) → ↑ (StateT∘ S M A)
  runStateT∘ : ∀ {S M A} → ↑ (StateT∘ S M A) → (↑V S → ↑ (M(A ×∘ S)))

  MaybeT∘    : (VTy → Ty) → (VTy → Ty)
  maybeT∘    : ∀ {M A} → ↑ (M (Maybe∘ A)) → ↑ (MaybeT∘ M A)
  runMaybeT∘ : ∀ {M A} → ↑ (MaybeT∘ M A) → ↑ (M (Maybe∘ A))

  ReaderT∘    : VTy → (VTy → Ty) → (VTy → Ty)
  readerT∘    : ∀ {R M A} → (↑V R → ↑ (M A)) → ↑ (ReaderT∘ R M A)
  runReaderT∘ : ∀ {R M A} → ↑ (ReaderT∘ R M A) → (↑V R → ↑ (M A))

  Identity∘    : VTy → Ty
  identity∘    : ∀ {A} → ↑V A → ↑ (Identity∘ A)
  runIdentity∘ : ∀ {A} → ↑ (Identity∘ A) → ↑V A
