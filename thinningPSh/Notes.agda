
open import Data.Nat
open import Relation.Binary.PropositionalEquality
  renaming (trans to infixr 5 _◼_; sym to infix 6 _⁻¹; subst to tr; cong to ap)

coe : ∀{i}{A B : Set i} → A ≡ B → A → B
coe refl a = a

_&_ :
  ∀{i j}{A : Set i}{B : Set j}(f : A → B){a₀ a₁ : A}(a₂ : a₀ ≡ a₁)
  → f a₀ ≡ f a₁
f & refl = refl
infixl 9 _&_

_⊗_ :
  ∀ {α β}{A : Set α}{B : Set β}
    {f g : A → B}(p : f ≡ g){a a' : A}(q : a ≡ a')
  → f a ≡ g a'
refl ⊗ refl = refl
infixl 8 _⊗_

Con = ℕ
pattern _+ n = suc n
pattern ∙ = zero

variable
  Γ Δ Σ Ξ : ℕ

-- order-preserving embedding
data OPE : Con → Con → Set where
  ε    : OPE ∙ ∙
  drop : OPE Γ Δ → OPE (Γ +) Δ
  keep : OPE Γ Δ → OPE (Γ +) (Δ +)

id : ∀ {Γ} → OPE Γ Γ
id {∙}   = ε
id {Γ +} = keep (id {Γ})

infixr 5 _∘_
_∘_ : OPE Δ Σ → OPE Γ Δ → OPE Γ Σ
σ      ∘ ε      = σ
σ      ∘ drop δ = drop (σ ∘ δ)
drop σ ∘ keep δ = drop (σ ∘ δ)
keep σ ∘ keep δ = keep (σ ∘ δ)

idl : (σ : OPE Γ Δ) → id ∘ σ ≡ σ
idl ε        = refl
idl (drop σ) = drop & idl σ
idl (keep σ) = keep & idl σ

idr : (σ : OPE Γ Δ) → σ ∘ id ≡ σ
idr ε        = refl
idr (drop σ) = drop & idr σ
idr (keep σ) = keep & idr σ

ass :
    (σ : OPE Σ Ξ)(δ : OPE Δ Σ)(ν : OPE Γ Δ)
  → (σ ∘ δ) ∘ ν ≡ σ ∘ (δ ∘ ν)
ass σ        δ        ε        = refl
ass σ        δ        (drop ν) = drop & ass σ δ ν
ass σ        (drop δ) (keep ν) = drop & ass σ δ ν
ass (drop σ) (keep δ) (keep ν) = drop & ass σ δ ν
ass (keep σ) (keep δ) (keep ν) = keep & ass σ δ ν

data Var : Con → Set where
  zero : Var (Γ +)
  suc  : Var Γ → Var (Γ +)

wkVar : Var Γ → OPE Δ Γ → Var Δ
wkVar x       (drop σ) = suc (wkVar x σ)
wkVar zero    (keep σ) = zero
wkVar (suc x) (keep σ) = suc (wkVar x σ)

--------------------------------------------------------------------------------
