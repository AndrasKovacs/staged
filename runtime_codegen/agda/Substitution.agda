
{-# OPTIONS --without-K #-}

module Substitution where

open import Lib
open import Syntax
open import Embedding

infixr 6 _ₑ∘ₛ_ _ₛ∘ₑ_ _∘ₛ_

data Sub (Γ : Con) : Con → Set where
  ∙   : Sub Γ ∙
  _,_ : ∀ {A Δ} → Sub Γ Δ → Tm Γ A → Sub Γ (Δ , A)

_ₛ∘ₑ_ : ∀ {Γ Δ Σ} → Sub Δ Σ → OPE Γ Δ → Sub Γ Σ
∙       ₛ∘ₑ δ = ∙
(σ , t) ₛ∘ₑ δ = (σ ₛ∘ₑ δ) , Tmₑ δ t

_ₑ∘ₛ_ : ∀ {Γ Δ Σ} → OPE Δ Σ → Sub Γ Δ → Sub Γ Σ
∙      ₑ∘ₛ δ       = δ
drop σ ₑ∘ₛ (δ , t) = σ ₑ∘ₛ δ
keep σ ₑ∘ₛ (δ , t) = σ ₑ∘ₛ δ , t

dropₛ : ∀ {A Γ Δ} → Sub Γ Δ → Sub (Γ , A) Δ
dropₛ σ = σ ₛ∘ₑ wk

keepₛ : ∀ {A Γ Δ} → Sub Γ Δ → Sub (Γ , A) (Δ , A)
keepₛ σ = dropₛ σ , var vz

⌜_⌝ᵒᵖᵉ : ∀ {Γ Δ} → OPE Γ Δ → Sub Γ Δ
⌜ ∙      ⌝ᵒᵖᵉ = ∙
⌜ drop σ ⌝ᵒᵖᵉ = dropₛ ⌜ σ ⌝ᵒᵖᵉ
⌜ keep σ ⌝ᵒᵖᵉ = keepₛ ⌜ σ ⌝ᵒᵖᵉ

-- Action on ∈ and Tm
∈ₛ : ∀ {A Γ Δ} → Sub Γ Δ → A ∈ Δ → Tm Γ A
∈ₛ (σ , t) vz     = t
∈ₛ (σ , t) (vs v) = ∈ₛ σ v

Tmₛ : ∀ {A Γ Δ} → Sub Γ Δ → Tm Δ A → Tm Γ A
Tmₛ σ (var v)   = ∈ₛ σ v
Tmₛ σ (lam t)   = lam (Tmₛ (keepₛ σ) t)
Tmₛ σ (app f a) = app (Tmₛ σ f) (Tmₛ σ a)
Tmₛ σ < t >     = < Tmₛ σ t >
Tmₛ σ (~ t)     = ~ Tmₛ σ t

-- Identity and composition
idₛ : ∀ {Γ} → Sub Γ Γ
idₛ {∙}     = ∙
idₛ {Γ , A} = keepₛ idₛ

_∘ₛ_ : ∀ {Γ Δ Σ} → Sub Δ Σ → Sub Γ Δ → Sub Γ Σ
∙       ∘ₛ δ = ∙
(σ , t) ∘ₛ δ = σ ∘ₛ δ , Tmₛ δ t

-- Sub category laws and functor laws for (Tm _ A).
-- We need a number of different identity and associativity
-- lemmas depending on which parameters are embeddings or substitutions
assₛₑₑ :
  ∀ {Γ Δ Σ Ξ}(σ : Sub Σ Ξ)(δ : OPE Δ Σ)(ν : OPE Γ Δ)
  → (σ ₛ∘ₑ δ) ₛ∘ₑ ν ≡ σ ₛ∘ₑ (δ ∘ₑ ν)
assₛₑₑ ∙       δ ν = refl
assₛₑₑ (σ , t) δ ν = _,_ & assₛₑₑ σ δ ν ⊗ (Tm-∘ₑ δ ν t ⁻¹)

assₑₛₑ :
  ∀ {Γ Δ Σ Ξ}(σ : OPE Σ Ξ)(δ : Sub Δ Σ)(ν : OPE Γ Δ)
  → (σ ₑ∘ₛ δ) ₛ∘ₑ ν ≡ σ ₑ∘ₛ (δ ₛ∘ₑ ν)
assₑₛₑ ∙        δ       ν = refl
assₑₛₑ (drop σ) (δ , t) ν = assₑₛₑ σ δ ν
assₑₛₑ (keep σ) (δ , t) ν = (_, Tmₑ ν t) & assₑₛₑ σ δ ν

idlₑₛ : ∀ {Γ Δ}(σ : Sub Γ Δ) → idₑ ₑ∘ₛ σ ≡ σ
idlₑₛ ∙       = refl
idlₑₛ (σ , t) = (_, t) & idlₑₛ σ

idlₛₑ : ∀ {Γ Δ}(σ : OPE Γ Δ) → idₛ ₛ∘ₑ σ ≡ ⌜ σ ⌝ᵒᵖᵉ
idlₛₑ ∙        = refl
idlₛₑ (drop σ) =
      ((idₛ ₛ∘ₑ_) ∘ drop) & idrₑ σ ⁻¹
    ◾ assₛₑₑ idₛ σ wk ⁻¹
    ◾ dropₛ & idlₛₑ σ
idlₛₑ (keep σ) =
  (_, var vz) &
      (assₛₑₑ idₛ wk (keep σ)
    ◾ ((idₛ ₛ∘ₑ_) ∘ drop) & (idlₑ σ ◾ idrₑ σ ⁻¹)
    ◾ assₛₑₑ idₛ σ wk ⁻¹
    ◾ (_ₛ∘ₑ wk) & idlₛₑ σ )

idrₑₛ : ∀ {Γ Δ}(σ : OPE Γ Δ) → σ ₑ∘ₛ idₛ ≡ ⌜ σ ⌝ᵒᵖᵉ
idrₑₛ ∙        = refl
idrₑₛ (drop σ) = assₑₛₑ σ idₛ wk ⁻¹ ◾ dropₛ & idrₑₛ σ
idrₑₛ (keep σ) = (_, var vz) & (assₑₛₑ σ idₛ wk ⁻¹ ◾ (_ₛ∘ₑ wk) & idrₑₛ σ)

∈-ₑ∘ₛ : ∀ {A Γ Δ Σ}(σ : OPE Δ Σ)(δ : Sub Γ Δ)(v : A ∈ Σ) → ∈ₛ (σ ₑ∘ₛ δ) v ≡ ∈ₛ δ (∈ₑ σ v)
∈-ₑ∘ₛ ∙        δ       v      = refl
∈-ₑ∘ₛ (drop σ) (δ , t) v      = ∈-ₑ∘ₛ σ δ v
∈-ₑ∘ₛ (keep σ) (δ , t) vz     = refl
∈-ₑ∘ₛ (keep σ) (δ , t) (vs v) = ∈-ₑ∘ₛ σ δ v

Tm-ₑ∘ₛ : ∀ {A Γ Δ Σ}(σ : OPE Δ Σ)(δ : Sub Γ Δ)(t : Tm Σ A) → Tmₛ (σ ₑ∘ₛ δ) t ≡ Tmₛ δ (Tmₑ σ t)
Tm-ₑ∘ₛ σ δ (var v) = ∈-ₑ∘ₛ σ δ v
Tm-ₑ∘ₛ σ δ (lam t) =
  lam & ((λ x → Tmₛ (x , var vz) t) & assₑₛₑ σ δ wk ◾ Tm-ₑ∘ₛ (keep σ) (keepₛ δ) t)
Tm-ₑ∘ₛ σ δ (app f a) = app & Tm-ₑ∘ₛ σ δ f ⊗ Tm-ₑ∘ₛ σ δ a
Tm-ₑ∘ₛ σ δ < t >     = <_> & Tm-ₑ∘ₛ σ δ t
Tm-ₑ∘ₛ σ δ (~ t)     = ~_ & Tm-ₑ∘ₛ σ δ t

∈-ₛ∘ₑ : ∀ {A Γ Δ Σ}(σ : Sub Δ Σ)(δ : OPE Γ Δ)(v : A ∈ Σ) → ∈ₛ (σ ₛ∘ₑ δ) v ≡ Tmₑ δ (∈ₛ σ v)
∈-ₛ∘ₑ (σ , _) δ vz     = refl
∈-ₛ∘ₑ (σ , _) δ (vs v) = ∈-ₛ∘ₑ σ δ v

Tm-ₛ∘ₑ : ∀ {A Γ Δ Σ}(σ : Sub Δ Σ)(δ : OPE Γ Δ)(t : Tm Σ A) → Tmₛ (σ ₛ∘ₑ δ) t ≡ Tmₑ δ (Tmₛ σ t)
Tm-ₛ∘ₑ σ δ (var v)   = ∈-ₛ∘ₑ σ δ v
Tm-ₛ∘ₑ σ δ (lam t)   =
  lam &
      ((λ x → Tmₛ (x , var vz) t) &
          (assₛₑₑ σ δ wk
        ◾ (σ ₛ∘ₑ_) & (drop & (idrₑ δ ◾ idlₑ δ ⁻¹))
        ◾ assₛₑₑ σ wk (keep δ) ⁻¹)
    ◾ Tm-ₛ∘ₑ (keepₛ σ) (keep δ) t)
Tm-ₛ∘ₑ σ δ (app f a) = app & Tm-ₛ∘ₑ σ δ f ⊗ Tm-ₛ∘ₑ σ δ a
Tm-ₛ∘ₑ σ δ < t >     = <_> & Tm-ₛ∘ₑ σ δ t
Tm-ₛ∘ₑ σ δ (~ t)     = ~_ & Tm-ₛ∘ₑ σ δ t

assₛₑₛ :
  ∀ {Γ Δ Σ Ξ}(σ : Sub Σ Ξ)(δ : OPE Δ Σ)(ν : Sub Γ Δ)
  → (σ ₛ∘ₑ δ) ∘ₛ ν ≡ σ ∘ₛ (δ ₑ∘ₛ ν)
assₛₑₛ ∙       δ ν = refl
assₛₑₛ (σ , t) δ ν = _,_ & assₛₑₛ σ δ ν ⊗ (Tm-ₑ∘ₛ δ ν t ⁻¹)

assₛₛₑ :
  ∀ {Γ Δ Σ Ξ}(σ : Sub Σ Ξ)(δ : Sub Δ Σ)(ν : OPE Γ Δ)
  → (σ ∘ₛ δ) ₛ∘ₑ ν ≡ σ ∘ₛ (δ ₛ∘ₑ ν)
assₛₛₑ ∙       δ ν = refl
assₛₛₑ (σ , t) δ ν = _,_ & assₛₛₑ σ δ ν ⊗ (Tm-ₛ∘ₑ δ ν t ⁻¹)

∈-∘ₛ : ∀ {A Γ Δ Σ}(σ : Sub Δ Σ)(δ : Sub Γ Δ)(v : A ∈ Σ) → ∈ₛ (σ ∘ₛ δ) v ≡ Tmₛ δ (∈ₛ σ v)
∈-∘ₛ (σ , _) δ vz     = refl
∈-∘ₛ (σ , _) δ (vs v) = ∈-∘ₛ σ δ v

Tm-∘ₛ : ∀ {A Γ Δ Σ}(σ : Sub Δ Σ)(δ : Sub Γ Δ)(t : Tm Σ A) → Tmₛ (σ ∘ₛ δ) t ≡ Tmₛ δ (Tmₛ σ t)
Tm-∘ₛ σ δ (var v)   = ∈-∘ₛ σ δ v
Tm-∘ₛ σ δ (lam t)   =
  lam &
      ((λ x → Tmₛ (x , var vz) t) &
          (assₛₛₑ σ δ wk
        ◾ (σ ∘ₛ_) & (idlₑₛ  (dropₛ δ) ⁻¹) ◾ assₛₑₛ σ wk (keepₛ δ) ⁻¹)
    ◾ Tm-∘ₛ (keepₛ σ) (keepₛ δ) t)
Tm-∘ₛ σ δ (app f a) = app & Tm-∘ₛ σ δ f ⊗ Tm-∘ₛ σ δ a
Tm-∘ₛ σ δ < t >     = <_> & Tm-∘ₛ σ δ t
Tm-∘ₛ σ δ (~ t)     = ~_ & Tm-∘ₛ σ δ t

∈-idₛ : ∀ {A Γ}(v : A ∈ Γ) → ∈ₛ idₛ v ≡ var v
∈-idₛ vz     = refl
∈-idₛ (vs v) = ∈-ₛ∘ₑ idₛ wk v ◾ Tmₑ wk & ∈-idₛ v ◾ (var ∘ vs) & ∈-idₑ v

Tm-idₛ : ∀ {A Γ}(t : Tm Γ A) → Tmₛ idₛ t ≡ t
Tm-idₛ (var v)   = ∈-idₛ v
Tm-idₛ (lam t)   = lam & Tm-idₛ t
Tm-idₛ (app f a) = app & Tm-idₛ f ⊗ Tm-idₛ a
Tm-idₛ < t >     = <_> & Tm-idₛ t
Tm-idₛ (~ t)     = ~_ & Tm-idₛ t

idrₛ : ∀ {Γ Δ}(σ : Sub Γ Δ) → σ ∘ₛ idₛ ≡ σ
idrₛ ∙       = refl
idrₛ (σ , t) = _,_ & idrₛ σ ⊗ Tm-idₛ t

idlₛ : ∀ {Γ Δ}(σ : Sub Γ Δ) → idₛ ∘ₛ σ ≡ σ
idlₛ ∙       = refl
idlₛ (σ , t) = (_, t) & (assₛₑₛ idₛ wk (σ , t) ◾ idlₛ (idₑ ₑ∘ₛ σ) ◾ idlₑₛ σ)

assₛ :
  ∀ {Γ Δ Σ Ξ}(σ : Sub Σ Ξ)(δ : Sub Δ Σ)(ν : Sub Γ Δ)
  → (σ ∘ₛ δ) ∘ₛ ν ≡ σ ∘ₛ (δ ∘ₛ ν)
assₛ ∙       δ ν = refl
assₛ (σ , t) δ ν = _,_ & assₛ σ δ ν ⊗ (Tm-∘ₛ δ ν t ⁻¹)
