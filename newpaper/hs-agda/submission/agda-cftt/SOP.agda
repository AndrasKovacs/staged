
module SOP where

open import Lib
open import Object
open import Gen

open import Agda.Builtin.TrustMe

--------------------------------------------------------------------------------

Uₚ = List VTy

data Elₚ : Uₚ → Set where
  []  : Elₚ []
  _∷_ : ∀ {a A} → ↑V a → Elₚ A → Elₚ (a ∷ A)

loopₚ : ∀ {A} → Elₚ A
loopₚ {[]}    = []
loopₚ {a ∷ A} = loop∘ ∷ loopₚ

headₚ : ∀ {A B} → Elₚ (A ∷ B) → ↑V A
headₚ (x ∷ _) = x

tailₚ : ∀ {A B} → Elₚ (A ∷ B) → Elₚ B
tailₚ (_ ∷ xs) = xs

⊤ₚ : Uₚ
⊤ₚ = []

ttₚ : Elₚ ⊤ₚ
ttₚ = []

infixr 5 _*ₚ_
_*ₚ_ : Uₚ → Uₚ → Uₚ
A *ₚ B = A ++ B

pairₚ : ∀ {A B} → Elₚ A → Elₚ B → Elₚ (A *ₚ B)
pairₚ []       y = y
pairₚ (x ∷ xs) y = x ∷ pairₚ xs y

unpairₚ : ∀ {A B} → Elₚ (A *ₚ B) → Elₚ A × Elₚ B
unpairₚ {[]}    x       = [] , x
unpairₚ {a ∷ A} (x ∷ y) = case× (unpairₚ {A} y) λ xs ys → x ∷ xs , ys

*ₚβ : ∀ {A B}(x : Elₚ A)(y : Elₚ B) → unpairₚ (pairₚ x y) ≡ (x , y)
*ₚβ {[]} {B} [] y = refl
*ₚβ {a ∷ A} {B} (x ∷ xs) y rewrite *ₚβ {A}{B} xs y = refl

*ₚη : ∀ {A B}(x : Elₚ (A *ₚ B)) → pairₚ (unpairₚ {A}{B} x .₁) (unpairₚ x .₂) ≡ x
*ₚη {[]}        x        = refl
*ₚη {a ∷ A} {B} (x ∷ xs) = ap (x ∷_) (*ₚη {A}{B} xs)

infix 3 _→PT_
_→PT_ : Uₚ → Ty → CTy
[]            →PT B = ⊤∘ ⇒ B
a ∷ []        →PT B = a ⇒ B
a ∷ A@(_ ∷ _) →PT B = a ⇒ C (A →PT B)

appₚₜ : ∀ {A B} → ↑C (A →PT B) → Elₚ A → ↑ B
appₚₜ f []               = f ∙ tt∘
appₚₜ f (x ∷ [])         = f ∙ x
appₚₜ f (x ∷ xs@(_ ∷ _)) = appₚₜ (f ∙ x) xs

lamₚₜ : ∀ {A B} → (Elₚ A → ↑ B) → ↑C (A →PT B)
lamₚₜ {[]}            {B} f = Λ λ _ → f []
lamₚₜ {a ∷ []}        {B} f = Λ λ x → f (x ∷ [])
lamₚₜ {a ∷ A@(_ ∷ _)} {B} f = Λ λ a → lamₚₜ {A}{B} (λ as → f (a ∷ as))


--------------------------------------------------------------------------------
Uₛ = List Uₚ

data Elₛ : Uₛ → Set where
  here  : ∀ {a A} → Elₚ a → Elₛ (a ∷ A)
  there : ∀ {a A} → Elₛ A → Elₛ (a ∷ A)

⊥ₛ : Uₛ
⊥ₛ = []

exfalsoₛ : Elₛ ⊥ₛ → ⊥
exfalsoₛ ()

infixr 5 _+ₛ_
_+ₛ_ : Uₛ → Uₛ → Uₛ
A +ₛ B = A ++ B

leftₛ : ∀ {A B} → Elₛ A → Elₛ (A +ₛ B)
leftₛ (here a)  = here a
leftₛ (there a) = there (leftₛ a)

rightₛ : ∀ {A B} → Elₛ B → Elₛ (A +ₛ B)
rightₛ {[]}    x = x
rightₛ {a ∷ A} x = there (rightₛ x)

eitherₛ : ∀ {A B} → Elₛ (A +ₛ B) → Either (Elₛ A) (Elₛ B)
eitherₛ {[]}    x         = right x
eitherₛ {a ∷ A} (here x)  = left (here x)
eitherₛ {a ∷ A} (there x) = either (eitherₛ {A} x) (left ∘ there) right

leftₛβ : ∀ {A B}(x : Elₛ A) → eitherₛ {A}{B} (leftₛ x) ≡ left x
leftₛβ {a ∷ A} {B} (here x)  = refl
leftₛβ {a ∷ A} {B} (there x) = ap (λ x → either x (left ∘ there) right) (leftₛβ {A}{B} x)

rightₛβ : ∀ {A B}(x : Elₛ B) → eitherₛ {A}{B} (rightₛ x) ≡ right x
rightₛβ {[]}    {B} x = refl
rightₛβ {a ∷ A} {B} x = ap (λ x → either x (left ∘ there) right) (rightₛβ {A}{B} x)

leftₛη : ∀ {A B}(x : Elₛ (A +ₛ B))(y : Elₛ A) → eitherₛ x ≡ left y → x ≡ leftₛ y
leftₛη {a ∷ A} {B} (here x)  y refl = refl
leftₛη {a ∷ A} {B} (there x) (here  y) p with eitherₛ {A}{B} x
... | left  z = case p of λ ()
... | right z = case p of λ ()
leftₛη {a ∷ A} {B} (there x) (there y) p with eitherₛ {A}{B} x | inspect (eitherₛ {A}{B}) x | p
... | left z  | hide e | refl = ap there (leftₛη {A}{B} x y e)
... | right z | _ | ()

rightₛη : ∀ {A B}(x : Elₛ (A +ₛ B))(y : Elₛ B) → eitherₛ x ≡ right y → x ≡ rightₛ y
rightₛη {[]} {B} x .x refl = refl
rightₛη {a ∷ A} {B} (there x) y p with eitherₛ {A}{B} x | inspect (eitherₛ {A}{B}) x | p
... | right z | hide e | refl = ap there (rightₛη {A}{B} x y e)

+ₛ-η : ∀ {A B}(x : Elₛ (A +ₛ B)) → either (eitherₛ {A}{B} x) (leftₛ {A}{B}) (rightₛ {A}{B}) ≡ x
+ₛ-η {A} {B} x with eitherₛ {A}{B} x | inspect (eitherₛ {A}{B}) x
... | left  y | hide p = leftₛη x y p ⁻¹
... | right y | hide p = rightₛη x y p ⁻¹

infixr 5 _*ₚₛ_
_*ₚₛ_ : Uₚ → Uₛ → Uₛ
A *ₚₛ []    = []
A *ₚₛ b ∷ B = (A *ₚ b) ∷ (A *ₚₛ B)

pairₚₛ : ∀ {A B} → Elₚ A → Elₛ B → Elₛ (A *ₚₛ B)
pairₚₛ x (here y)  = here (pairₚ x y)
pairₚₛ x (there y) = there (pairₚₛ x y)

unpairₚₛ : ∀ {A B} → Elₛ (A *ₚₛ B) → Elₚ A × Elₛ B
unpairₚₛ {A}{B = b ∷ B} (here x)  = case unpairₚ {A}{b} x of λ {(x , y) → x , here y}
unpairₚₛ {A}{B = b ∷ B} (there x) = case× (unpairₚₛ {A}{B} x) (λ x y → x , there y)

*ₚₛβ : ∀ {A B}(x : Elₚ A)(y : Elₛ B) → unpairₚₛ (pairₚₛ x y) ≡ (x , y)
*ₚₛβ {A} {b ∷ B} x (here y) rewrite *ₚβ x y = refl
*ₚₛβ {A} {b ∷ B} x (there y) rewrite *ₚₛβ {A}{B} x y = refl

*ₚₛη : ∀ {A B}(x : Elₛ (A *ₚₛ B)) → pairₚₛ (unpairₚₛ {A}{B} x .₁) (unpairₚₛ x .₂) ≡ x
*ₚₛη {A} {b ∷ B} (here x)  = ap here (*ₚη {A}{b} x)
*ₚₛη {A} {b ∷ B} (there x) = ap there (*ₚₛη x)

infixr 5 _*ₛ_
_*ₛ_ : Uₛ → Uₛ → Uₛ
[]      *ₛ B = []
(a ∷ A) *ₛ B = (a *ₚₛ B) +ₛ (A *ₛ B)

pairₛ : ∀ {A B} → Elₛ A → Elₛ B → Elₛ (A *ₛ B)
pairₛ (here x)  y = leftₛ (pairₚₛ x y)
pairₛ (there x) y = rightₛ (pairₛ x y)

unpairₛ : ∀ {A B} → Elₛ (A *ₛ B) → Elₛ A × Elₛ B
unpairₛ {a ∷ A}{B} x = either (eitherₛ {a *ₚₛ B} {A *ₛ B} x)
  (λ x → case× (unpairₚₛ x) λ x y → here x , y)
  (λ x → case× (unpairₛ {A}{B} x) λ x y → there x , y)

*ₛβ : ∀ {A B}(x : Elₛ A)(y : Elₛ B) → unpairₛ (pairₛ x y) ≡ (x , y)
*ₛβ {a ∷ A} {B} (here x) y rewrite leftₛβ {_}{A *ₛ B} (pairₚₛ x y) | *ₚₛβ x y = refl
*ₛβ {a ∷ A} {B} (there x) y rewrite rightₛβ {a *ₚₛ B} (pairₛ x y) | *ₛβ {A}{B} x y = refl

*ₛη : ∀ {A B}(x : Elₛ (A *ₛ B)) → pairₛ (unpairₛ {A}{B} x .₁) (unpairₛ {A}{B} x .₂) ≡ x
*ₛη {a ∷ A} {B} x with eitherₛ {a *ₚₛ B}{A *ₛ B} x | inspect (eitherₛ {a *ₚₛ B}{A *ₛ B}) x
... | left y  | hide eq rewrite *ₚₛη {a}{B} y = leftₛη x y eq ⁻¹
... | right y | hide eq rewrite *ₛη  {A}{B} y = rightₛη x y eq ⁻¹

-- Closure under Σ
--------------------------------------------------------------------------------

-- POSTULATE
generative : ∀ {A} → (f : Elₚ A → Uₛ) → ∀ x y → f x ≡ f y
generative f x y = primTrustMe

Σₛ : (A : Uₛ) → (Elₛ A → Uₛ) → Uₛ
Σₛ []      B = []
Σₛ (a ∷ A) B = (a *ₚₛ B (here loopₚ)) +ₛ Σₛ A (B ∘ there)

pairΣₛ : ∀ {A B} (a : Elₛ A) → Elₛ (B a) → Elₛ (Σₛ A B)
pairΣₛ {a ∷ A} {B} (here x)  y =
  leftₛ {a *ₚₛ B (here loopₚ)} {Σₛ A (B ∘ there)}
        (tr (λ x → Elₛ (a *ₚₛ x)) (generative (B ∘ here) _ _) (pairₚₛ x y))
pairΣₛ {a ∷ A} {B} (there x) y =
  rightₛ {a *ₚₛ B (here loopₚ)} {Σₛ A (B ∘ there)} (pairΣₛ x y)

unpairΣₛ : ∀ {A B} → Elₛ (Σₛ A B) → Σ (Elₛ A) (Elₛ ∘ B)
unpairΣₛ {a ∷ A} {B} x = either (eitherₛ {a *ₚₛ B (here loopₚ)}{Σₛ A (B ∘ there)} x)
  (λ x → case× (unpairₚₛ x)  λ x y → here x , tr Elₛ (generative (B ∘ here) _ _) y)
  (λ x → let x , y = unpairΣₛ {A}{B ∘ there} x in there x , y)

tr-pairₚₛ : {A : Uₚ}{B B' : Uₛ}(e : B ≡ B')(x : Elₚ A)(y : Elₛ B)
            → tr (λ B → Elₛ (A *ₚₛ B)) e (pairₚₛ x y) ≡ pairₚₛ x (tr Elₛ e y)
tr-pairₚₛ refl x y = refl

tr-tr-UIP : ∀ {A : Set}{B : A → Set}{x y : A}(p : x ≡ y)(q : y ≡ x) b → tr B q (tr B p b) ≡ b
tr-tr-UIP refl refl b = refl

Σₛβ : ∀ {A}{B : Elₛ A → Uₛ}(x : Elₛ A)(y : Elₛ (B x)) → unpairΣₛ (pairΣₛ {A}{B} x y) ≡ (x , y)
Σₛβ {a ∷ A} {B} (here x) y rewrite
    leftₛβ {a *ₚₛ B (here loopₚ)} {Σₛ A (B ∘ there)}
           ((tr (λ x₁ → Elₛ (a *ₚₛ x₁)) (generative (B ∘ here) _ _) (pairₚₛ x y)))
  | tr-pairₚₛ (generative (B ∘ here) _ loopₚ) x y
  | *ₚₛβ x (tr Elₛ (generative (B ∘ here)_ loopₚ) y)
   = ap (here x ,_) (tr-tr-UIP (generative (B ∘ here) _ _) (generative (B ∘ here) _ _) y)

Σₛβ {a ∷ A} {B} (there x) y rewrite
  rightₛβ {a *ₚₛ B (here loopₚ)}{Σₛ A (B ∘ there)} (pairΣₛ x y)
  | Σₛβ {A}{B ∘ there} x y
  = refl

Σₛη : ∀ {A B}(x : Elₛ (Σₛ A B)) → pairΣₛ (unpairΣₛ {A}{B} x .₁) (unpairΣₛ {A}{B} x .₂) ≡ x
Σₛη {a ∷ A} {B} x
  with eitherₛ {a *ₚₛ B (here (loopₚ))} {Σₛ A (B ∘ there)} x
  | inspect (eitherₛ {a *ₚₛ B (here (loopₚ))} {Σₛ A (B ∘ there)}) x
... | left y  | hide eq rewrite
  tr-pairₚₛ (generative (B ∘ here) (unpairₚₛ y .₁) loopₚ) (₁ (unpairₚₛ y))
            (tr Elₛ (generative (λ x₁ → B (here x₁)) _ _) (₂ (unpairₚₛ y)))
  | tr-tr-UIP {Uₛ}{Elₛ}{B (here loopₚ)}{B (here (unpairₚₛ y .₁))}
                (generative (B ∘ here) loopₚ (unpairₚₛ y .₁))
                (generative (B ∘ here) (unpairₚₛ y .₁) loopₚ)
                (unpairₚₛ y .₂)
  | *ₚₛη {a}{B (here loopₚ)} y
  = leftₛη x y eq ⁻¹
... | right y | hide eq rewrite (Σₛη {A}{B ∘ there} y) = rightₛη x y eq ⁻¹

--------------------------------------------------------------------------------

record IsSOP (A : Set) : Set where
  field
    Rep    : Uₛ
    encode : A → Elₛ Rep
    decode : Elₛ Rep → A
    p      : ∀ x → encode (decode x) ≡ x
    q      : ∀ x → decode (encode x) ≡ x
open IsSOP ⦃...⦄ public

instance
  SOPBool : IsSOP Bool
  IsSOP.Rep SOPBool = [] ∷ [] ∷ []
  IsSOP.encode SOPBool true = here []
  IsSOP.encode SOPBool false = there (here [])
  IsSOP.decode SOPBool (here []) = true
  IsSOP.decode SOPBool (there (here [])) = false
  IsSOP.p SOPBool (here []) = refl
  IsSOP.p SOPBool (there (here [])) = refl
  IsSOP.q SOPBool true = refl
  IsSOP.q SOPBool false = refl

  SOP⊤ : IsSOP ⊤
  IsSOP.Rep SOP⊤ = [] ∷ []
  IsSOP.encode SOP⊤ _ = here []
  IsSOP.decode SOP⊤ _ = tt
  IsSOP.p SOP⊤ (here []) = refl
  IsSOP.q SOP⊤ x = refl

  SOPMaybe : ∀ {A}⦃ _ : IsSOP A ⦄ → IsSOP (Maybe A)
  IsSOP.Rep (SOPMaybe {A}) = [] ∷ Rep {A}
  IsSOP.encode (SOPMaybe {A}) (just x) = there (encode x)
  IsSOP.encode (SOPMaybe {A}) nothing = here []
  IsSOP.decode (SOPMaybe {A}) (here []) = nothing
  IsSOP.decode (SOPMaybe {A}) (there x) = just (decode x)
  IsSOP.p (SOPMaybe {A}) (here []) = refl
  IsSOP.p (SOPMaybe {A}) (there x) rewrite p x = refl
  IsSOP.q (SOPMaybe {A}) (just x) rewrite q x = refl
  IsSOP.q (SOPMaybe {A}) nothing = refl

  SOPEither : ∀ {A B}⦃ _ : IsSOP A ⦄ ⦃ _ : IsSOP B ⦄ → IsSOP (Either A B)
  IsSOP.Rep (SOPEither {A} {B}) = Rep {A} +ₛ Rep {B}
  IsSOP.encode (SOPEither {A} {B}) (left x)  = leftₛ {Rep{A}}{Rep{B}} (encode x)
  IsSOP.encode (SOPEither {A} {B}) (right x) = rightₛ {Rep{A}}{Rep{B}} (encode x)
  IsSOP.decode (SOPEither {A} {B}) x with eitherₛ {Rep{A}}{Rep{B}} x
  ... | left x = left (decode x)
  ... | right x = right (decode x)
  IsSOP.p (SOPEither {A} {B}) x with eitherₛ {Rep{A}}{Rep{B}} x | inspect (eitherₛ {Rep{A}}{Rep{B}}) x
  ... | left x'  | hide eq rewrite p {A} x' = leftₛη _ _ eq ⁻¹
  ... | right x' | hide eq rewrite p {B} x' = rightₛη _ _ eq ⁻¹
  IsSOP.q (SOPEither {A} {B}) (left x) rewrite leftₛβ {Rep{A}}{Rep{B}} (encode x) =
    ap left (q {A} x)
  IsSOP.q (SOPEither {A} {B}) (right x) rewrite rightₛβ {Rep{A}}{Rep{B}} (encode x) =
    ap right (q {B} x)

  SOP× : ∀ {A B}⦃ _ : IsSOP A ⦄ ⦃ _ : IsSOP B ⦄ → IsSOP (A × B)
  IsSOP.Rep (SOP× {A} {B}) = Rep {A} *ₛ Rep {B}
  IsSOP.encode (SOP× {A} {B}) (x , y) = pairₛ (encode x) (encode y)
  IsSOP.decode (SOP× {A} {B}) x with unpairₛ {Rep {A}}{Rep {B}} x
  ... | x , y = decode x , decode y
  IsSOP.p (SOP× {A} {B}) x rewrite
     p {A} (₁ (unpairₛ x)) | p {B} (₂ (unpairₛ {Rep{A}}{Rep{B}} x)) = *ₛη {Rep{A}}{Rep{B}} x
  IsSOP.q (SOP× {A} {B}) (x , y) rewrite
    *ₛβ (encode x) (encode y) | q {A} x | q {B} y = refl

  SOP↑ : ∀ {A} → IsSOP (↑V A)
  IsSOP.Rep (SOP↑ {A}) = (A ∷ []) ∷ []
  IsSOP.encode (SOP↑ {A}) x = here (x ∷ [])
  IsSOP.decode (SOP↑ {A}) (here (x ∷ [])) = x
  IsSOP.p (SOP↑ {A}) (here (x ∷ [])) = refl
  IsSOP.q (SOP↑ {A}) x = refl

  SOP⊥ : IsSOP ⊥
  IsSOP.Rep SOP⊥ = []
  IsSOP.encode SOP⊥ ()
  IsSOP.decode SOP⊥ ()
  IsSOP.p SOP⊥ ()
  IsSOP.q SOP⊥ ()

-- This is not an instance because usually we only want the vanilla _×_ instance instead,
-- and this one would overlap with it.
SOPΣ : ∀ {A}{B : A → Set}⦃ _ : IsSOP A ⦄ ⦃ _ : ∀ {x} → IsSOP (B x) ⦄ → IsSOP (Σ A B)
IsSOP.Rep (SOPΣ {A} {B}) = Σₛ (Rep {A}) (λ x → Rep {B (decode x)})
IsSOP.encode (SOPΣ {A} {B} ⦃ sopA ⦄ ⦃ sopB ⦄) (x , y) =

  pairΣₛ (encode x)
         (tr (λ x → Elₛ (IsSOP.Rep (sopB {x}))) (IsSOP.q sopA x ⁻¹) (encode y))

IsSOP.decode (SOPΣ {A} {B} ⦃ sopA ⦄ ⦃ sopB ⦄) x =
  case× (unpairΣₛ {Rep{A}}{λ x → Rep{B (decode x)}} x)
  λ x y → decode x , decode y

IsSOP.p (SOPΣ {A} {B} ⦃ sopA ⦄ ⦃ sopB ⦄) x
  rewrite IsSOP.p sopB (₂ (unpairΣₛ {Rep{A}}{λ x → Rep{B (decode x)}} x))
  | UIP (IsSOP.q sopA (IsSOP.decode sopA (₁ (unpairΣₛ x))) ⁻¹)
         (ap (IsSOP.decode sopA) ((IsSOP.p sopA (₁ (unpairΣₛ x))) ⁻¹))
  with (IsSOP.encode sopA (IsSOP.decode sopA (₁ (unpairΣₛ x)))) | IsSOP.p sopA (₁ (unpairΣₛ x))
... | _ | refl = Σₛη {Rep{A}} {λ x → Rep{B (decode x)}} x

IsSOP.q (SOPΣ {A} {B} ⦃ sopA ⦄ ⦃ sopB ⦄) (x , y)
  rewrite
      Σₛβ {Rep{A}}{λ x → Rep{B (decode x)}} (IsSOP.encode sopA x)
      (tr (λ x → Elₛ (IsSOP.Rep (sopB {x}))) (IsSOP.q sopA x ⁻¹) (IsSOP.encode sopB y))
  with IsSOP.decode sopA (IsSOP.encode sopA x) | IsSOP.q sopA x
... | _ | refl = ap (x ,_) (IsSOP.q sopB y)
