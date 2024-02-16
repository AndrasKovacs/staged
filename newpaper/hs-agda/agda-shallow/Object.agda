
module Object where

open import Data.Nat
open import Lib

infixr 3 _⇒_

postulate
  Ty  : Set
  ↑   : Ty → Set

  VTy : Set
  V   : VTy → Ty

  CTy : Set
  C   : CTy → Ty

{-# INJECTIVE ↑ #-}
{-# INJECTIVE V #-}
{-# INJECTIVE C #-}

↑C = λ A → ↑ (C A)
↑V = λ A → ↑ (V A)

infixl 8 _∙_
infixl 5 _+∘_ _-∘_
infixl 6 _*∘_
infix 4 _==∘_ _<∘_
infixr 4 _×∘_ _×C_
infixr 4 _,∘_ _,C_

postulate
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

DefRec : ∀ {A} → (↑C A → ↑C A) → ↑C A
DefRec f = LetRec _ f id

loop∘ : ∀ {A} → ↑ A
loop∘ {A} = LetRec (⊤∘ ⇒ A) (λ f → f) (λ f → f ∙ tt∘)

fieldC1 : ∀ {A B} → ↑C (A ×C B) → ↑C A
fieldC1 x = fst∘ x

fieldC2 : ∀ {A B C} → ↑C (A ×C B ×C C) → ↑C B
fieldC2 x = fst∘ (snd∘ x)

fieldC3 : ∀ {A B C D} → ↑C (A ×C B ×C C ×C D) → ↑C C
fieldC3 x = fst∘ (snd∘ (snd∘ x))

fieldC4 : ∀ {A B C D E} → ↑C (A ×C B ×C C ×C D ×C E) → ↑C D
fieldC4 x = fst∘ (snd∘ (snd∘ (snd∘ x)))

fieldC5 : ∀ {A B C D E F} → ↑C (A ×C B ×C C ×C D ×C E ×C F) → ↑C E
fieldC5 x = fst∘ (snd∘ (snd∘ (snd∘ (snd∘ x))))

↓Bool : Bool → ↑V Bool∘
↓Bool true = true∘
↓Bool false = false∘

instance
  Numℕ∘ : Number (↑V ℕ∘)
  Number.fromNat Numℕ∘ x = lit∘ x
