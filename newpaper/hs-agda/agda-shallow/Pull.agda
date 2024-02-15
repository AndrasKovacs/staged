{-# OPTIONS --type-in-type #-} -- Because Pull : Set → Set₁ is annoying

module Pull where

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

record Pull (A : Set) : Set where
  constructor pull
  field
    St        : Set
    {{StSOP}} : IsSOP St
    seed      : St
    step      : St → Gen (Step St A)
open Pull public

getStRep : ∀ {A} → Pull A → Uₛ
getStRep (pull _ {{StSOP}} _ _) = IsSOP.Rep StSOP

repeat : ∀ {A} → A → Pull A
St    (repeat a)   = ⊤
seed  (repeat a)   = tt
step  (repeat a) _ = pure $ yield a tt

applyPull : ∀ {A B} → Pull (A → B) → Pull A → Pull B
St   (applyPull fs as) = St fs × St as
seed (applyPull fs as) = (seed fs , seed as)
step (applyPull fs as) (s , s') =
  step fs s >>= λ where
    stop        → pure stop
    (skip s)    → pure $ skip (s , s')
    (yield f s) → step as s' >>= λ where
      stop         → pure stop
      (skip s')    → pure $ skip (s , s')
      (yield a s') → pure $ yield (f a) (s , s')

emptyPull : ∀ {A} → Pull A
St   emptyPull   = ⊤
seed emptyPull   = tt
step emptyPull _ = pure stop

appendPull : ∀ {A} → Pull A → Pull A → Pull A
St    (appendPull as as') = Either (St as) (St as')
seed  (appendPull as as') = left (seed as)
step  (appendPull as as') (left s)  =
  step as s <&> λ where
    stop        → skip (right (seed as'))
    (skip s)    → skip (left s)
    (yield a s) → yield a (left s)
step (appendPull as as') (right s) =
  step as' s <&> λ where
    stop        → stop
    (skip s)    → skip (right s)
    (yield a s) → yield a (right s)

instance
  APull : Applicative Pull
  Applicative.pure APull  = repeat
  Applicative._<*>_ APull = applyPull

  SemigroupPull : ∀ {A} → Semigroup (Pull A)
  Semigroup._<>_ SemigroupPull = appendPull

  MonoidPull : ∀ {A} → Monoid (Pull A)
  Monoid.mempty MonoidPull = emptyPull

mapGen : ∀ {A B} → Pull A → (A → Gen B) → Pull B
St (mapGen as f)     = St as
seed (mapGen as f)   = seed as
step (mapGen as f) s = step as s >>= λ where
  stop        → pure stop
  (skip s)    → pure $ skip s
  (yield a s) → do b ← f a; pure $ yield b s

single : ∀ {A} → A → Pull A
St    (single a) = Bool
seed  (single a) = true
step (single a) true  = pure $ yield a false
step (single a) false = pure stop

forEach : ∀ {A B} ⦃ _ : IsSOP A ⦄ → Pull A → (A → Pull B) → Pull B
St    (forEach {A} {B} as f)            = St as × Maybe (Σ A (St ∘ f))
StSOP (forEach {A} {B} {{ sopA }} as f) = SOP× {{ StSOP as }}{{ SOPMaybe {{ SOPΣ {{ sopA }} {{ λ {x} → StSOP (f x) }}}}}}
seed  (forEach {A} {B} as f)            = seed as , nothing

step (forEach {A} {B} as f) (s , just (a , s')) = step (f a) s' <&> λ where
  stop         → skip (s , nothing)
  (skip s')    → skip (s , just (a , s'))
  (yield b s') → yield b (s , (just (a , s')))
step (forEach {A} {B} as f) (s , nothing) = step as s <&> λ where
  stop        → stop
  (skip s)    → skip (s , nothing)
  (yield a s) → skip (s , just (a , seed (f a)))

genLetPull : ∀ {A} → ↑V A → Pull (↑V A)
genLetPull a = mapGen (single a) genLet

countFrom : ↑V ℕ∘ → Pull (↑V ℕ∘)
St   (countFrom n)   = ↑V ℕ∘
seed (countFrom n)   = n
step (countFrom n) s = pure $ yield s (s +∘ lit∘ 1)

count : Pull (↑V ℕ∘)
count = countFrom (lit∘ 0)

take : ∀ {A} → ↑V ℕ∘ → Pull A → Pull A
St    (take n as) = ↑V ℕ∘ × St as
seed  (take n as) = n , seed as
step  (take n as) (i , s) = case' (i ==∘ lit∘ 0) λ where
  true  → pure stop
  false → step as s <&> λ where
    stop        → stop
    (skip s)    → skip (i , s)
    (yield a s) → yield a ((i -∘ lit∘ 1) , s)

drop : ∀ {A} → ↑V ℕ∘ → Pull A → Pull A
St   (drop n as) = Either (↑V ℕ∘) (St as)
seed (drop n as) = left n
step (drop n as) (left i)  = case' (i ==∘ lit∘ 0) λ where
  true  → pure $ skip $ right (seed as)
  false → pure $ skip $ left (i -∘ lit∘ 1)
step (drop n as) (right s) = step as s <&> λ where
  stop        → stop
  (skip s)    → skip (right s)
  (yield a s) → yield a (right s)

filter : ∀ {A} → (A → Gen Bool) → Pull A → Pull A
St   (filter f as) = St as
seed (filter f as) = seed as
step (filter f as) s = step as s >>= λ where
  stop        → pure stop
  (skip s)    → pure $ skip s
  (yield a s) → f a >>= λ where
                  true  → pure $ yield a s
                  false → pure $ skip s

--------------------------------------------------------------------------------

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
foldrPull {A} {B} (pull S seed step) f b =
  LetRec (funTypes (Rep {S}) B)
         (λ fs → absFun λ s → unGen (step (decode s)) λ where
                     stop        → b
                     (skip s)    → callFun fs (encode s)
                     (yield a s) → f a (callFun fs (encode s)))
         (λ fs → callFun fs (encode seed))

--------------------------------------------------------------------------------

toList : ∀ {A} → Pull (↑V A) → ↑V (List∘ A)
toList as = foldrPull as cons∘ nil∘

foldlPull : ∀ {A B} → Pull A → (↑V B → A → ↑V B) → ↑V B → ↑V B
foldlPull as f b = foldrPull as (λ a hyp → Λ λ b → hyp ∙ f b a) (Λ λ b → b) ∙ b

dup : ∀ {A} → Pull A → Pull (A × A)
dup as = (λ x → x , x) <$> as

zip : ∀ {A B} → Pull (↑V A) → Pull (↑V B) → Pull (↑V (A ×∘ B))
zip as bs = _,∘_ <$> as <*> bs

casePull : ∀ {A B}⦃ _ : Split A ⦄ ⦃ _ : IsSOP (SplitTo {A}) ⦄ → ↑V A → (SplitTo {A} → Pull B) → Pull B
casePull {A} {B} a f = forEach (mapGen (single a) split) f

sumPull : Pull (↑V ℕ∘) → ↑V ℕ∘
sumPull as = foldlPull as _+∘_ 0

--------------------------------------------------------------------------------
