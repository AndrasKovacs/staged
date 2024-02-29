
module Pull2 where

open import Lib
open import Object
open import Gen
open import SOP
open import Split
open import Join

-- Inlining linearly used states??



-- Pull can be equivalently written using St : Uₛ instead, which yields Pull A : Set₀, but
-- St : Set is just more convenient to use in Agda.
{-# NO_UNIVERSE_CHECK #-}
record Pull (A : Set) : Set where
  constructor pull
  field
    St        : Set
    {{StSOP}} : IsSOP St
    start     : Maybe St
    step      : St → Gen (Maybe A × Maybe St)
open Pull public

single : ∀ {A} → A → Pull A
St    (single a)   = ⊤
start (single a)   = just tt
step  (single a) _ = pure (just a , nothing)

empty : ∀ {A} → Pull A
St    empty = ⊥
start empty = nothing
step  empty ()

append : ∀ {A} → Pull A → Pull A → Pull A
St    (append as as') = Either (St as) (St as')
start (append as as') = left <$> start as
step  (append as as') = λ where
  (left  s) → step as s >>= λ where
    (a , just s)  → pure (a , just (left s))
    (a , nothing) → pure (a , (right <$> start as'))
  (right s) → do
    (a , s) ← step as' s
    pure (a , (right <$> s))

getStRep : ∀ {A} → Pull A → Uₛ
getStRep (pull _ {{StSOP}} _ _) = IsSOP.Rep StSOP

StSize : ∀ {A} → Pull A → ℕ
StSize x = length (getStRep x)

repeat : ∀ {A} → A → Pull A
St    (repeat a)   = ⊤
start (repeat a)   = just tt
step  (repeat a) _ = pure (just a , just tt)

infixl 4 _<$>ₚ_
_<$>ₚ_ : ∀ {A B} → (A → B) → Pull A → Pull B
St    (f <$>ₚ as)   = St as
start (f <$>ₚ as)   = start as
step  (f <$>ₚ as) s = do a , s ← step as s; pure ((f <$> a) , s)

infixl 1 _<&>ₚ_
_<&>ₚ_ : ∀ {A B} → Pull A → (A → B) → Pull B
_<&>ₚ_ = flip _<$>ₚ_

infixl 4 _<*>ₚ_
_<*>ₚ_ : ∀ {A B}⦃ _ : IsSOP A ⦄ → Pull (A → B) → Pull A → Pull B
St (_<*>ₚ_ {A} fs as) = St fs × Either (St as) (A × Maybe (St as))
start (fs <*>ₚ as) = do s ← start fs; s' ← start as; pure (s , left s')
step (fs <*>ₚ as) (s , left s') = step as s' >>= λ where
  (just a  , ms') → pure (nothing , just (s , right (a , ms')))
  (nothing , ms') → pure (nothing , (do s' ← ms'; pure (s , left s')))
step (fs <*>ₚ as) (s , right (a , ms')) = step fs s >>= λ where
  (just f  , ms)  → pure (just (f a) , (do s ← ms; s' ← ms'; pure (s , left s')))
  (nothing , ms)  → pure (nothing    , (do s ← ms; pure (s , right (a , ms'))))

instance
  SemigroupPull : ∀ {A} → Semigroup (Pull A)
  Semigroup._<>_ SemigroupPull = append

  MonoidPull : ∀ {A} → Monoid (Pull A)
  Monoid.mempty MonoidPull = empty

mapGen : ∀ {A B} → (A → Gen B) → Pull A → Pull B
St    (mapGen f as)   = St as
start (mapGen f as)   = start as
step  (mapGen f as) s = step as s >>= λ where
  (nothing , s) → pure (nothing , s)
  (just a  , s) → do b ← f a; pure (just b , s)

forEach : ∀ {A B} ⦃ _ : IsSOP A ⦄ → Pull A → (A → Pull B) → Pull B
St (forEach {A} {B} as f) = St as × Maybe (Σ A (St ∘ f))
StSOP (forEach {A} {B}{{sopA}} as f) = SOP×{{StSOP as}}{{SOPMaybe {{SOPΣ {{sopA}}{{λ {x} → StSOP (f x)}}}}}}
start (forEach {A} {B} as f) = (_, nothing) <$> start as
step (forEach {A} {B} as f) (s , just (a , s')) = step (f a) s' >>= λ where
  (mb , nothing) → pure (mb , just (s , nothing))
  (mb , just s') → pure (mb , just (s , just (a , s')))
step (forEach {A} {B} as f) (s , nothing) = step as s >>= λ where
  (_  , nothing) → pure (nothing , nothing)
  (ma , just s ) → pure (nothing , just (s , (do a ← ma; s' ← start (f a); pure {F = Maybe} (a , s') )))

genLetₚ : ∀ {A B} → ↑V A → (↑V A → Pull B) → Pull B
genLetₚ a f = forEach (mapGen genLet (single a)) f

caseₚ : ∀ {A B}⦃ _ : Split A ⦄ ⦃ _ : IsSOP (SplitTo {A}) ⦄ → ↑V A → (SplitTo {A} → Pull B) → Pull B
caseₚ a f = forEach (mapGen splitGen (single a)) f

countFrom : ↑V ℕ∘ → Pull (↑V ℕ∘)
St (countFrom n)     = ↑V ℕ∘
start (countFrom n)  = just n
step (countFrom _) n = pure (just n , just (n +∘ 1))

count : Pull (↑V ℕ∘)
count = countFrom (lit∘ 0)

take : ∀ {A} → ↑V ℕ∘ → Pull A → Pull A
St    (take n as)         = ↑V ℕ∘ × St as
start (take n as)         = (n ,_) <$> start as
step  (take _ as) (n , s) = caseM (n ==∘ 0) λ where
  true  → pure (nothing , nothing)
  false → step as s >>= λ where
    (ma      , nothing) → pure (ma , nothing)
    (just a  , just s)  → pure (just a  , just (n -∘ 1 , s))
    (nothing , just s)  → pure (nothing , just (n , s))

drop : ∀ {A} → ↑V ℕ∘ → Pull A → Pull A
St    (drop n as) = Either (↑V ℕ∘ × St as) (St as)
start (drop n as) = do s ← start as; pure (left (n , s))
step (drop _ as) (left (n , s)) = caseM (n ==∘ 0) λ where
  true  → pure (nothing , just (right s))
  false → step as s >>= λ where
    (_       , nothing) → pure (nothing , nothing)
    (just _  , just s ) → pure (nothing , just (left ((n -∘ 1) , s)))
    (nothing , just s)  → pure (nothing , just (left (n        , s)))
step (drop n as) (right s) = do
  ma , ms ← step as s
  pure (ma , (right <$> ms))

filter : ∀ {A} → (A → Gen Bool) → Pull A → Pull A
St (filter f as) = St as
start (filter f as) = start as
step (filter f as) s = step as s >>= λ where
  (just a  , ms) → f a >>= λ where
    true  → pure (just a , ms)
    false → pure (nothing , ms)
  (nothing , ms) → pure (nothing , ms)

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
foldrPull {A} {B} (pull S nothing  step) f b = b
foldrPull {A} {B} (pull S (just s) step) f b =
  LetRec (funTypes (Rep {S}) B)
         (λ fs → absFun λ s → unGen (step (decode s)) λ where
               (just a  , just s)  → f a (callFun fs (encode s))
               (just a  , nothing) → f a b
               (nothing , just s)  → callFun fs (encode s)
               (nothing , nothing) → b)
         (λ fs → callFun fs (encode s))

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
