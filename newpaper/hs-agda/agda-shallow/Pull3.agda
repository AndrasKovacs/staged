
module Pull3 where

open import Lib
open import Object
open import Gen
open import SOP
open import Split
open import Join

data Step (S A : Set) : Set where
  stop     : Step S A
  stopWith : A → Step S A
  skip     : S → Step S A
  yield    : A → S → Step S A

{-# NO_UNIVERSE_CHECK #-}
record Pull (A : Set) : Set where
  constructor pull
  field
    St        : Set
    {{SOPSt}} : IsSOP St
    canSkip   : Bool
    getSeed   : Maybe St
    getStep   : St → Gen (Step St A)
open Pull public

single : ∀ {A} → A → Pull A
St (single a)      = ⊤
canSkip (single a) = false
getSeed (single a)    = pure tt
getStep (single a) _  = pure $ stopWith a

empty : ∀ {A} → Pull A
St empty = ⊥
canSkip empty = false
getSeed empty = fail
getStep empty ()

appendSeeds : ∀ {A B} → Maybe A → Maybe B → Maybe (Either A B)
appendSeeds (just a) b = just (left a)
appendSeeds nothing  b = right <$> b

append : ∀ {A} → Pull A → Pull A → Pull A
append (pull _ _ nothing _) as' = as'
append as (pull _ _ nothing _)  = as
append (pull S _ (just seed) step) (pull S' _ (just seed') step') =
  pull (Either S S') true (just (left seed)) λ where
    (left s) → step s >>= λ where
      stop         → pure $ skip (right seed')
      (stopWith a) → pure $ yield a (right seed')
      (skip s)     → pure $ skip (left s)
      (yield a s)  → pure $ yield a (left s)
    (right s) → step' s >>= λ where
      stop         → pure $ stop
      (stopWith a) → pure $ stopWith a
      (skip s)     → pure $ skip (right s)
      (yield a s)  → pure $ yield a (right s)


instance
  SemigroupPull : ∀ {A} → Semigroup (Pull A)
  Semigroup._<>_ SemigroupPull = append

  MonoidPull : ∀ {A} → Monoid (Pull A)
  Monoid.mempty MonoidPull = empty

mapGen : ∀ {A B} → (A → Gen B) → Pull A → Pull B
St (mapGen f as) = St as
canSkip (mapGen f as) = canSkip as
getSeed (mapGen f as) = getSeed as
getStep (mapGen f as) s = getStep as s >>= λ where
  stop → pure stop
  (stopWith a) → do b ← f a; pure $ stopWith b
  (skip s) → pure $ skip s
  (yield a s) → do b ← f a; pure $ yield b s

-- Sadly, I need to add +1 to S! That makes this kinda explodey in nested loops.
forEach : ∀ {A B} ⦃ _ : IsSOP A ⦄ → Pull A → (A → Pull B) → Pull B
forEach {A} {B}{{sopA}} (pull S {{sopS}} _ seed step) f =
  pull (S × Maybe (Σ A (St ∘ f))) {{SOP×{{sopS}}{{SOPMaybe{{SOPΣ{{sopA}}{{λ {x} → SOPSt (f x)}}}}}}}}
       true (do s ← seed; pure (s , nothing)) λ where
         (s , nothing) → step s >>= λ where
           stop         → pure stop
           (stopWith a) → pure (skip ({!!} , ((a ,_) <$> getSeed (f a))))
           (skip s)     → pure $ skip (s , nothing)
           (yield a s)  → pure $ skip (s , (((a ,_)) <$> getSeed (f a)))
         (s , just (a , s')) → getStep (f a) s' >>= λ where
           stop         → pure $ skip (s , nothing)
           (stopWith b) → {!!}
           (skip s')    → {!!}
           (yield b s') → {!!}

-- forEach : ∀ {A B} ⦃ _ : IsSOP A ⦄ → Pull A → (A → Pull B) → Pull B
-- forEach {A} {B}{{sopA}} (pull S {{sopS}} _ seed step) f =
--   pull (S × Maybe (Σ A (St ∘ f))) {{SOP×{{sopS}}{{SOPMaybe{{SOPΣ{{sopA}}{{λ {x} → SOPSt (f x)}}}}}}}}
--        true (do s ← seed; pure (s , nothing)) λ where
--          (s , nothing) → step s >>= λ where
--            stop         → pure stop
--            (stopWith a) → pure {!!}
--            (skip s)     → pure $ skip (s , nothing)
--            (yield a s)  → pure $ skip (s , (((a ,_)) <$> getSeed (f a)))
--          (s , just (a , s')) → {!!}

genLetₚ : ∀ {A B} → ↑V A → (↑V A → Pull B) → Pull B
genLetₚ a f = forEach (mapGen genLet (single a)) f

caseₚ : ∀ {A B}⦃ _ : Split A ⦄ ⦃ _ : IsSOP (SplitTo {A}) ⦄ → ↑V A → (SplitTo {A} → Pull B) → Pull B
caseₚ a f = forEach (mapGen splitGen (single a)) f

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
foldrPull {A} {B} (pull S canSkip nothing step) f b = b
foldrPull {A} {B} (pull S canSkip (just seed) step) f b = runGen do
    fs ← genLetRec {A = funTypes (Rep {S}) B} λ fs → absFun λ s → unGen (step (decode s)) λ where
       stop         → b
       (stopWith a) → f a b
       (skip s)     → callFun fs (encode s)
       (yield a s)  → f a (callFun fs (encode s))
    pure $ callFun fs (encode seed)

toList : ∀ {A} → Pull (↑V A) → ↑V (List∘ A)
toList as = foldrPull as cons∘ nil∘
