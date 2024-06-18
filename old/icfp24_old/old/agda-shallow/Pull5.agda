

module Pull5 where

open import Lib
open import Object
open import Gen
open import SOP
open import Split
open import Join

data Step (S A : Set) : Set where
  stop  : Step S A
  skip  : S → Step S A
  yield : A → S → Step S A

-- Pull can be equivalently written using St : Uₛ instead, which yields Pull A : Set₀, but
-- St : Set is just more convenient to use in Agda.
{-# NO_UNIVERSE_CHECK #-}
record Pull (A : Set) : Set where
  constructor pull
  field
    St        : Set
    skips     : Bool
    {{StSOP}} : IsSOP St
    seed      : Gen St
    step      : St → Gen (Step St A)
open Pull public

getStRep : ∀ {A} → Pull A → Uₛ
getStRep (pull _ _ {{StSOP}} _ _) = IsSOP.Rep StSOP

repeat : ∀ {A} → A → Pull A
St    (repeat a)   = ⊤
seed  (repeat a)   = pure tt
skips (repeat a)   = false
step  (repeat a) _ = pure $ yield a tt

infixl 4 _<$>ₚ_
_<$>ₚ_ : ∀ {A B} → (A → B) → Pull A → Pull B
f <$>ₚ pull S skips seed step =
  pull S skips seed (λ s → step s >>= λ where
    stop        → pure stop
    (skip s)    → pure $ skip s
    (yield a s) → pure $ yield (f a) s)

infixl 1 _<&>ₚ_
_<&>ₚ_ : ∀ {A B} → Pull A → (A → B) → Pull B
_<&>ₚ_ = flip _<$>ₚ_

infixl 4 _<*>ₚ_
_<*>ₚ_ : ∀ {A B}⦃ _ : IsSOP A ⦄ → Pull (A → B) → Pull A → Pull B
_<*>ₚ_ {A} (pull S false seed step) (pull S' false seed' step') =
     pull (S × S') false (_,_ <$> seed <*> seed') λ where
       (s , s') → step s >>= λ where
         stop        → pure stop
         (skip s)    → pure stop -- impossible
         (yield f s) → step' s' >>= λ where
           stop         → pure stop
           (skip s'   ) → pure stop -- impossible
           (yield a s') → pure $ yield (f a) (s , s')

_<*>ₚ_ {A} (pull S true seed step) (pull S' false seed' step') =
  pull (S × S') true (_,_ <$> seed <*> seed') λ where
    (s , s') → step s >>= λ where
      stop        → pure stop
      (skip s)    → pure $ skip (s , s')
      (yield f s) → step' s' >>= λ where
        stop         → pure stop
        (skip s')    → pure stop -- impossible
        (yield a s') → pure $ yield (f a) (s , s')

_<*>ₚ_ {A} (pull S false seed step) (pull S' true seed' step') =
  pull (S × S') true (_,_ <$> seed <*> seed') λ where
    (s , s') → step' s' >>= λ where
      stop         → pure stop
      (skip s')    → pure $ skip (s , s')
      (yield a s') → step s >>= λ where
        stop        → pure stop
        (skip s)    → pure stop -- impossible
        (yield f s) → pure $ yield (f a) (s , s')

_<*>ₚ_ {A} (pull S true seed step) (pull S' true seed' step') =
  pull (S × S' × Maybe A) true (_,_ <$> seed <*> ((_, nothing) <$> seed')) λ where
    (s , s' , just a)  → step s >>= λ where
      stop        → pure stop
      (skip s)    → pure $ skip (s , s' , just a)
      (yield f s) → pure $ yield (f a) (s , s' , nothing)
    (s , s' , nothing) → step' s' >>= λ where
      stop         → pure stop
      (skip s')    → pure $ skip (s , s' , nothing)
      (yield a s') → pure $ skip (s , s' , just a)

empty : ∀ {A} → Pull A
St   empty   = ⊤
seed empty   = pure tt
skips empty  = false
step empty _ = pure stop

consₚ : ∀ {A} → A → Pull A → Pull A
consₚ a (pull S skips seed step) =
  pull (Maybe S) skips (pure nothing) λ where
    nothing  → do s ← seed; pure $ yield a (just s)
    (just s) → step s >>= λ where
      stop        → pure stop
      (skip s)    → pure $ skip (just s)
      (yield a s) → pure $ yield a (just s)

append : ∀ {A} → Pull A → Pull A → Pull A
St    (append as as') = Either (St as) (St as')
seed  (append as as') = left <$> seed as
skips (append as as') = true
step  (append as as') (left s)  =
  step as s >>= λ where
    stop        → skip ∘ right <$> seed as'
    (skip s)    → pure $ skip (left s)
    (yield a s) → pure $ yield a (left s)
step (append as as') (right s) =
  step as' s <&> λ where
    stop        → stop
    (skip s)    → skip (right s)
    (yield a s) → yield a (right s)

instance
  SemigroupPull : ∀ {A} → Semigroup (Pull A)
  Semigroup._<>_ SemigroupPull = append

  MonoidPull : ∀ {A} → Monoid (Pull A)
  Monoid.mempty MonoidPull = empty

mapGen : ∀ {A B} → Pull A → (A → Gen B) → Pull B
St (mapGen as f)     = St as
seed (mapGen as f)   = seed as
skips (mapGen as f)  = skips as
step (mapGen as f) s = step as s >>= λ where
  stop        → pure stop
  (skip s)    → pure $ skip s
  (yield a s) → do b ← f a; pure $ yield b s

single : ∀ {A} → A → Pull A
St    (single a) = Bool
seed  (single a) = pure true
skips (single a) = false
step (single a) true  = pure $ yield a false
step (single a) false = pure stop

forEach : ∀ {A B} ⦃ _ : IsSOP A ⦄ → Pull A → (A → Pull B) → Pull B
St    (forEach {A} {B} as f)            = St as × Maybe (Σ A (St ∘ f))
skips (forEach {A} {B} as f)            = true
StSOP (forEach {A} {B} {{ sopA }} as f) = SOP× {{ StSOP as }}{{ SOPMaybe {{ SOPΣ {{ sopA }} {{ λ {x} → StSOP (f x) }}}}}}
seed  (forEach {A} {B} as f)            = (_, nothing) <$> seed as
step (forEach {A} {B} as f) (s , just (a , s')) = step (f a) s' <&> λ where
  stop         → skip (s , nothing)
  (skip s')    → skip (s , just (a , s'))
  (yield b s') → yield b (s , (just (a , s')))
step (forEach {A} {B} as f) (s , nothing) = step as s >>= λ where
  stop        → pure stop
  (skip s)    → pure $ skip (s , nothing)
  (yield a s) → do s' ← seed (f a); pure {F = Gen} $ skip (s , just (a , s'))

bindSingle : ∀ {A A' B}⦃ _ : IsSOP A' ⦄ → ↑V A → (↑V A → Gen A') → (A' → Pull B) → Pull B
bindSingle {A} {A'} {B}{{sopA'}} a f g =
  pull (Σ A' (St ∘ g)) true {{SOPΣ{{sopA'}}{{λ {x} → StSOP (g x)}}}}
       (do a' ← f a; s ← seed (g a'); pure {F = Gen} (a' , s)) λ where
         (a' , s) → step (g a') s >>= λ where
            stop        → pure stop
            (skip s)    → pure $ skip (a' , s)
            (yield b s) → pure $ yield b (a' , s)

genLetₚ : ∀ {A B} → ↑V A → (↑V A → Pull B) → Pull B
genLetₚ a = bindSingle a genLet

caseₚ : ∀ {A B}⦃ _ : Split A ⦄ ⦃ _ : IsSOP (SplitTo {A}) ⦄ → ↑V A → (SplitTo {A} → Pull B) → Pull B
caseₚ a = bindSingle a splitGen

countFrom : ↑V ℕ∘ → Pull (↑V ℕ∘)
St   (countFrom n)   = ↑V ℕ∘
seed (countFrom n)   = pure n
skips (countFrom n)  = false
step (countFrom n) s = pure $ yield s (s +∘ lit∘ 1)

count : Pull (↑V ℕ∘)
count = countFrom (lit∘ 0)

take : ∀ {A} → ↑V ℕ∘ → Pull A → Pull A
St    (take n as) = ↑V ℕ∘ × St as
seed  (take n as) = (n ,_) <$> seed as
skips (take n as) = skips as
step  (take n as) (i , s) = caseM (i ==∘ lit∘ 0) λ where
  true  → pure stop
  false → step as s <&> λ where
    stop        → stop
    (skip s)    → skip (i , s)
    (yield a s) → yield a ((i -∘ lit∘ 1) , s)

drop : ∀ {A} → ↑V ℕ∘ → Pull A → Pull A
St   (drop n as) = Either (↑V ℕ∘) (St as)
seed (drop n as) = pure $ left n
skips (drop n as) = true
step (drop n as) (left i)  = caseM (i ==∘ lit∘ 0) λ where
  true  → skip ∘ right <$> seed as
  false → pure $ skip $ left (i -∘ lit∘ 1)
step (drop n as) (right s) = step as s <&> λ where
  stop        → stop
  (skip s)    → skip (right s)
  (yield a s) → yield a (right s)

filter : ∀ {A} → (A → Gen Bool) → Pull A → Pull A
St   (filter f as) = St as
skips (filter f as) = true
seed (filter f as) = seed as
step (filter f as) s = step as s >>= λ where
  stop        → pure stop
  (skip s)    → pure $ skip s
  (yield a s) → f a >>= λ where
                  true  → pure $ yield a s
                  false → pure $ skip s

--------------------------------------------------------------------------------

-- TODO: this is the same as what we have for joins in the paper!
funTypes : Uₛ → Ty → CTy
funTypes []      B = ⊤C
funTypes (a ∷ A) B = (a →PT B) ×C funTypes A B

callFun : ∀ {A B} → ↑C (funTypes A B) → Elₛ A → ↑ B
callFun {a ∷ A} {B} fs (here x)  = appₚₜ (fst∘ fs) x
callFun {a ∷ A} {B} fs (there x) = callFun (snd∘ fs) x

absFun : ∀ {A B} → (Elₛ A → ↑ B) → ↑C (funTypes A B)
absFun {[]}    {B} f = ttC
absFun {a ∷ A} {B} f = lamₚₜ (f ∘ here) ,C absFun {A}{B} (f ∘ there)

foldrPull : ∀ {A B} → Pull A → (A → ↑ B → ↑ B) → ↑ B → ↑ B
foldrPull {A} {B} (pull S _ seed step) f b =
  LetRec (funTypes (Rep {S}) B)
         (λ fs → absFun λ s → unGen (step (decode s)) λ where
                     stop        → b
                     (skip s)    → callFun fs (encode s)
                     (yield a s) → f a (callFun fs (encode s)))
         (λ fs → unGen seed λ s → callFun fs (encode s))

--------------------------------------------------------------------------------

toList : ∀ {A} → Pull (↑V A) → ↑V (List∘ A)
toList as = foldrPull as cons∘ nil∘

foldlPull : ∀ {A B} → Pull A → (↑V B → A → ↑V B) → ↑V B → ↑V B
foldlPull as f b = foldrPull as (λ a hyp → Λ λ b → hyp ∙ f b a) (Λ λ b → b) ∙ b

dup : ∀ {A} → Pull A → Pull (A × A)
dup as = (λ x → x , x) <$>ₚ as

zip : ∀ {A B} → Pull (↑V A) → Pull (↑V B) → Pull (↑V (A ×∘ B))
zip as bs = _,∘_ <$>ₚ as <*>ₚ bs

sumPull : Pull (↑V ℕ∘) → ↑V ℕ∘
sumPull as = foldlPull as _+∘_ 0

--------------------------------------------------------------------------------
