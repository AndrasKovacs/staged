
--------------------------------------------------------------------------------

data Ty : Set where
  Bool : Ty
  _⇒_  : Ty → Ty → Ty
infixr 5 _⇒_

variable
   A B C : Ty

data Con : Set where
  ∙ : Con
  _,_ : Con → Ty → Con
infixl 2 _,_

variable
  Γ Δ Σ : Con

data Var : Con → Ty → Set where
  vz : Var (Γ , A) A
  vs : Var Γ A → Var (Γ , B) A

data Tm (Γ : Con) : Ty → Set where
  var   : Var Γ A → Tm Γ A
  lam   : Tm (Γ , A) B → Tm Γ (A ⇒ B)
  app   : Tm Γ (A ⇒ B) → Tm Γ A → Tm Γ B
  true  : Tm Γ Bool
  false : Tm Γ Bool
  ite   : Tm Γ Bool → Tm Γ A → Tm Γ A → Tm Γ A

--------------------------------------------------------------------------------

-- data Atom (Γ : Con) : Ty → Set where
--   var   : Var Γ A → Atom Γ A
--   true  : Atom Γ Bool
--   false : Atom Γ Bool

-- data ANF (Γ : Con) : Ty → Set where
--   letlam  : ANF (Γ , A) B → ANF (Γ , A ⇒ B) C → ANF Γ C
--   letcont : ANF (Γ , A) B →
--   letapp  : Var Γ (A ⇒ B) → Atom Γ A → ANF (Γ , A) B → ANF Γ B
