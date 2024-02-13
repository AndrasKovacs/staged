
module SumV where

open import Lib
open import Object
open import Gen

data SumV : List VTy → Set where
  here  : ∀ {A As} → ↑V A → SumV (A ∷ As)
  there : ∀ {A As} → SumV As → SumV (A ∷ As)

--------------------------------------------------------------------------------

EitherVS : List VTy → List VTy → List VTy
EitherVS A B = A ++ B

infixr 6 _×VSV_
_×VSV_ : VTy → List VTy → List VTy
_×VSV_ a B = map (a ×∘_) B

infixr 6 _×SV_
_×SV_ : List VTy → List VTy → List VTy
[]      ×SV B = []
(a ∷ A) ×SV B = (a ×VSV B) ++ (A ×SV B)

leftSV : ∀ {A B} → SumV A → SumV (A ++ B)
leftSV (here x)  = here x
leftSV (there a) = there (leftSV a)

rightSV : ∀ {A B} → SumV B → SumV (A ++ B)
rightSV {[]}    b = b
rightSV {x ∷ A} b = there (rightSV b)

eitherSV : ∀ {A B} → SumV (A ++ B) → Either (SumV A) (SumV B)
eitherSV {[]}    ab         = right ab
eitherSV {x ∷ A} (here  a)  = left (here a)
eitherSV {x ∷ A} (there ab) with eitherSV {A = A} ab
... | left a  = left (there a)
... | right b = right b

pairVSV : ∀ {a A} → ↑V a → SumV A → SumV (a ×VSV A)
pairVSV x (here y)  = here (x ,∘ y)
pairVSV x (there y) = there (pairVSV x y)

unpairVSV : ∀ {a A} → SumV (a ×VSV A) → ↑V a × SumV A
unpairVSV {A = a ∷ A} (here x)  = fst∘ x , here (snd∘ x)
unpairVSV {A = a ∷ A} (there x) with unpairVSV x
... | x , y = x , there y

pairSV : ∀ {A B} → SumV A → SumV B → SumV (A ×SV B)
pairSV (here x)  y = leftSV  (pairVSV x y)
pairSV (there x) y = rightSV (pairSV x y)

unpairSV : ∀ {A B} → SumV (A ×SV B) → SumV A × SumV B
unpairSV {a ∷ A} {B} abs with eitherSV {a ×VSV B} {A ×SV B} abs
... | left  x = case unpairVSV x of λ {(x , y) → (here x) , y}
... | right y = case unpairSV {A}{B} y of λ {(x , y) → (there x) , y}

-- Closure under Σ
--------------------------------------------------------------------------------

postulate
  generative : ∀ {A} → (f : ↑ A → List VTy) → ∀ {x y} → f x ≡ f y

ΣSV : (A : List VTy) → (SumV A → List VTy) → List VTy
ΣSV []      B = []
ΣSV (a ∷ A) B = a ×VSV B (here loop∘) ++ ΣSV A (B ∘ there)

pairΣSV : ∀ {A B} → (a : SumV A) → SumV (B a) → SumV (ΣSV A B)
pairΣSV {a ∷ A}{B} (here x)  y =
  leftSV  {a ×VSV B (here loop∘)}{ΣSV A (B ∘ there)}
          (tr (λ x → SumV (a ×VSV x)) (generative (B ∘ here)) (pairVSV x y))
pairΣSV {a ∷ A}{B} (there x) y =
  rightSV {a ×VSV B (here loop∘)}{ΣSV A (B ∘ there)} (pairΣSV x y)

unpairΣSV : ∀ {A B} → SumV (ΣSV A B) → Σ (SumV A) (SumV ∘ B)
unpairΣSV {a ∷ A} {B} x with eitherSV {a ×VSV B (here loop∘)} {ΣSV A (B ∘ there)} x
... | left  x = case unpairVSV x of λ {(x , y) → (here x) , tr SumV (generative (B ∘ here)) y}
... | right x = case unpairΣSV {A}{B ∘ there} x of λ {(x , y) → (there x) , y}

--------------------------------------------------------------------------------

record IsSumV (A : Set) : Set where
  field
    Rep    : List VTy
    encode : A → SumV Rep
    decode : SumV Rep → A
open IsSumV ⦃...⦄ public

instance
  SumVBool : IsSumV Bool
  IsSumV.Rep SumVBool    = ⊤∘ ∷ ⊤∘ ∷ []
  IsSumV.encode SumVBool true             = here tt∘
  IsSumV.encode SumVBool false            = there (here tt∘)
  IsSumV.decode SumVBool (here  x)        = true
  IsSumV.decode SumVBool (there (here x)) = false

  SumV⊤ : IsSumV ⊤
  IsSumV.Rep SumV⊤ = ⊤∘ ∷ []
  IsSumV.encode SumV⊤ _ = here tt∘
  IsSumV.decode SumV⊤ _ = tt

  SumVMaybe : ∀ {A}⦃ _ : IsSumV A ⦄ → IsSumV (Maybe A)
  IsSumV.Rep    (SumVMaybe {A}) = ⊤∘ ∷ Rep {A}
  IsSumV.encode (SumVMaybe {A}) (just a) = there (encode a)
  IsSumV.encode (SumVMaybe {A}) nothing  = here tt∘
  IsSumV.decode (SumVMaybe {A}) (here x)  = nothing
  IsSumV.decode (SumVMaybe {A}) (there x) = just (decode x)

  SumVEither : ∀ {A B}⦃ _ : IsSumV A ⦄ ⦃ _ : IsSumV B ⦄ → IsSumV (Either A B)
  IsSumV.Rep (SumVEither {A} {B})      = Rep {A} ++ Rep {B}
  IsSumV.encode (SumVEither {A} {B}) (left x)  = leftSV {Rep {A}}{Rep {B}} (encode x)
  IsSumV.encode (SumVEither {A} {B}) (right y) = rightSV {Rep {A}}{Rep {B}} (encode y)
  IsSumV.decode (SumVEither {A} {B}) x with eitherSV {Rep{A}}{Rep{B}} x
  ... | left  x = left (decode x)
  ... | right x = right (decode x)

  SumV× : ∀ {A B}⦃ _ : IsSumV A ⦄ ⦃ _ : IsSumV B ⦄ → IsSumV (A × B)
  IsSumV.Rep (SumV× {A} {B})    = Rep {A} ×SV Rep {B}
  IsSumV.encode (SumV× {A} {B}) (x , y) = pairSV (encode x) (encode y)
  IsSumV.decode (SumV× {A} {B}) x with unpairSV {Rep {A}}{Rep {B}} x
  ... | x , y = (decode x) , (decode y)

  SumV↑V : ∀ {A} → IsSumV (↑V A)
  IsSumV.Rep (SumV↑V {A}) = A ∷ []
  IsSumV.encode (SumV↑V {A}) x = here x
  IsSumV.decode (SumV↑V {A}) (here x) = x

  SumVΣ : ∀ {A B} ⦃ _ : IsSumV A ⦄ ⦃ _ : ∀ {a} → IsSumV (B a) ⦄ → IsSumV (Σ A B)
  IsSumV.Rep    (SumVΣ {A} {B}) = ΣSV (Rep {A}) (λ x → Rep {B (decode x)})
  IsSumV.encode (SumVΣ {A} {B} ⦃ p ⦄ ⦃ q ⦄) (x , y) =
    pairΣSV {Rep {A}}{λ x → Rep {B (decode x)}} (encode x) {!encode y!}
  IsSumV.decode (SumVΣ {A} {B}) = {!!}

-- Yep..

-- tabulated continuations
--------------------------------------------------------------------------------

data ProdC : List CTy → Set where
  []   : ProdC []
  _∷_  : ∀ {A As} → ↑C A → ProdC As → ProdC (A ∷ As)

infix 3 _→SVT_
_→SVT_ : List VTy → Ty → List CTy
[]    →SVT B = []
a ∷ A →SVT B = (a ⇒ B) ∷ (A →SVT B)

tabulate : ∀ {A B} → (SumV A → ↑ B) → ProdC (A →SVT B)
tabulate {[]}    f = []
tabulate {a ∷ A} f = Λ (f ∘ here) ∷ tabulate (f ∘ there)

index : ∀ {A B} → ProdC (A →SVT B) → (SumV A → ↑ B)
index (f ∷ fs) (here x)  = f ∙ x
index (f ∷ fs) (there x) = index fs x

genLetPC : ∀ {A} → ProdC A → Gen (ProdC A)
genLetPC []       = return []
genLetPC (x ∷ xs) = _∷_ <$> genLet x <*> genLetPC xs
