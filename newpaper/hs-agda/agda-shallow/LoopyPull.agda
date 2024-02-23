{-# OPTIONS --type-in-type #-}

module LoopyPull where

open import Lib
open import Object
open import Gen
open import SOP
open import Split
open import Join

-- who cares about fusing filters???
-- try to avoid stop in definitions! StopWith is better.

data Step (S A : Set) : Set where
  stop      : Step S A
  stopWith  : A → Step S A
  yield     : A → S → Step S A

{-# NO_UNIVERSE_CHECK #-}
record Pull' (A : Set) : Set where
  constructor pull
  field
    St        : Set
    {{StSOP}} : IsSOP St
    seed      : Maybe St
    step      : St → Gen (Step St A)
open Pull' public

mapStep : ∀ {S S' A} → (S → S') → Gen (Step S A) → Gen (Step S' A)
mapStep f x = x >>= λ where
  stop → pure stop
  (stopWith a) → pure $ stopWith a
  (yield a s) → pure $ yield a (f s)

Pull : Set → Set
Pull A = Gen (Pull' A)

repeat : ∀ {A} → A → Pull A
repeat a = pure $ pull ⊤ (just tt) λ _ → pure $ yield a tt

apply : ∀ {A B} → Pull (A → B) → Pull A → Pull B
apply fs as = do
  pull S seed step ← fs
  pull S' seed' step' ← as
  pure $ pull (S × S') (_,_ <$> seed <*> seed') λ where (s , s') → step s >>= λ where
    stop         → pure stop
    (stopWith f) → step' s' >>= λ where
      stop         → pure stop
      (stopWith a) → pure $ stopWith (f a)
      (yield a s') → pure $ stopWith (f a)
    (yield f s)  → step' s' >>= λ where
      stop         → pure stop
      (stopWith a) → pure $ stopWith (f a)
      (yield a s') → pure $ yield (f a) (s , s')

funTypes : Uₛ → Ty → CTy
funTypes []      B = ⊤C
funTypes (a ∷ A) B = (a →PT B) ×C funTypes A B

callFun : ∀ {A B} → ↑C (funTypes A B) → Elₛ A → ↑ B
callFun {a ∷ A} {B} fs (here x)  = appₚₜ (fst∘ fs) x
callFun {a ∷ A} {B} fs (there x) = callFun (snd∘ fs) x

absFun : ∀ {A B} → (Elₛ A → ↑ B) → ↑C (funTypes A B)
absFun {[]}    {B} f = ttC
absFun {a ∷ A} {B} f = lamₚₜ (f ∘ here) ,C absFun {A}{B} (f ∘ there)

empty : ∀ {A} → Pull A
empty = pure (pull ⊥ nothing λ ())

append : ∀ {A} → Pull A → Pull A → Pull A
append {A} as as' = do
  pull S seed step ← as
  case seed of λ where
    nothing → as'
    (just seed) → do
      pull S' seed' step' ← as'
      case seed' of λ where
        nothing → as
        (just seed') → do
          pure $ pull (Either S S') (just (left seed)) λ where
            (left s)   → step s >>= λ where
              stop         → mapStep right (step' seed')
              (stopWith a) → pure $ yield a (right seed')
              (yield a s)  → pure $ yield a (left s)
            (right s') → mapStep right (step' s')

instance
  APull : Applicative Pull
  Applicative.pure APull  = repeat
  Applicative._<*>_ APull = apply

  SemigroupPull : ∀ {A} → Semigroup (Pull A)
  Semigroup._<>_ SemigroupPull = append

  MonoidPull : ∀ {A} → Monoid (Pull A)
  Monoid.mempty MonoidPull = empty

mapGen : ∀ {A B} → Pull A → (A → Gen B) → Pull B
mapGen as f = do
  pull S seed step ← as
  pure $ pull S seed λ s → step s >>= λ where
    stop         → pure stop
    (stopWith a) → do b ← f a; pure $ stopWith b
    (yield a s)  → do b ← f a; pure $ yield b s

single : ∀ {A} → A → Pull A
single a = pure $ pull ⊤ (just tt) λ _ → pure $ stopWith a

countFrom : ↑V ℕ∘ → Pull (↑V ℕ∘)
countFrom n = pure $ pull (↑V ℕ∘) (just n) λ n → pure $ yield n (n +∘ 1)

take : ∀ {A} → ↑V ℕ∘ → Pull A → Pull A
take n as = do
  pull S seed step ← as
  pure $ pull (↑V ℕ∘ × S) ((n ,_) <$> seed) λ where (n , s) → caseM (n ==∘ 0) λ where
    true  → pure stop
    false → step s >>= λ where
      stop         → pure stop
      (stopWith a) → pure $ stopWith a
      (yield a s)  → pure $ yield a ((n -∘ 1) , s)

takeSuc : ∀ {A} → ↑V ℕ∘ → Pull A → Pull A
takeSuc n as = do
  pull S seed step ← as
  pure $ pull (↑V ℕ∘ × S) ((n ,_) <$> seed) λ where (n , s) → step s >>= λ where
    stop         → pure stop
    (stopWith a) → pure $ stopWith a
    (yield a s)  → caseM (n ==∘ 0) λ where
      true  → pure $ stopWith a
      false → pure $ yield a ((n -∘ 1) , s)

foldr : ∀ {A B} → Pull A → (A → ↑ B → ↑ B) → ↑ B → ↑ B
foldr {A}{B} as f b = runGen do
  pull S seed step ← as
  case seed of λ where
    nothing     → pure b
    (just seed) → do
      loop ← genLetRec {A = funTypes (Rep{S}) B} λ loop → absFun λ s → unGen (step (decode s)) λ where
         stop         → b
         (stopWith a) → f a b
         (yield a s)  → f a (callFun loop (encode s))
      pure $ callFun loop (encode seed)

toList : ∀ {A} → Pull (↑V A) → ↑V (List∘ A)
toList as = foldr as cons∘ nil∘

{-# DISPLAY stateT∘ x = x #-}
{-# DISPLAY maybeT∘ x = x #-}
{-# DISPLAY identity∘ x = x #-}
{-# DISPLAY runIdentity∘ x = x #-}
{-# DISPLAY runMaybeT∘ x = x #-}
{-# DISPLAY runStateT∘ x = x #-}
{-# DISPLAY _∙_ f x = f x #-}
{-# DISPLAY lit∘ x = x #-}
{-# DISPLAY C x = x #-}
{-# DISPLAY V x = x #-}
{-# DISPLAY Λ f = f #-}

-- DOESNT WORK
forEach : ∀ {A B} ⦃ _ : IsSOP A ⦄ → Pull A → (A → Pull B) → Pull B
forEach {A}{B}{{sopA}} as f = do
  pull S seed step ← as
  case seed of λ where
    nothing     → empty
    (just seed) → step seed >>= λ where
      stop         → empty
      (stopWith a) → f a
      (yield a s)  →
        pure (pull (S × Σ A λ a → {!St <$>  f a!}) {!!} {!!})
-- A → Gen (Set)


-- filter : ∀ {A} → (A → Gen Bool) → Pull A → Pull A
-- filter {A} f as = do
--   pull S seed step ← as
--   foo ← genLetRec
--   -- skip ← gen λ {R} k →
--   --   LetRec (funTypes (Rep{S}) R)
--   --          (λ fs → absFun λ s → unGen (step (decode s)) λ where
--   --              stop         → {!!}
--   --              (stopWith a) → {!!}
--   --              (yield a s)  → {!!}
--   --          )
--   --          λ fs → {!fs!}
--   pure $ pull S seed {!!}


-- filter : ∀ {A} → (A → Gen Bool) → Pull A → Pull A
-- St    (filter f as) = St as
-- seed  (filter f as) = seed as
-- step  (filter f as) s = gen λ {R} k →
--   LetRec (funTypes (Rep{St as}) R)
--          (λ fs → absFun λ s → unGen (step as (decode s)) λ where
--            stop        → k stop
--            (halt a)    → unGen (f a) λ where
--                            true  → k (halt a)
--                            false → k stop
--            (yield a s) → unGen (f a) λ where
--                            true  → k (yield a s)
--                            false → callFun fs (encode s)
--          )
--          (λ fs → callFun fs (encode s))


-- forEach : ∀ {A B} ⦃ _ : IsSOP A ⦄ → Pull A → (A → Pull B) → Pull B
-- -- St (forEach {A} {B} as f) = St as × Maybe (Σ A (St ∘ f))
-- -- StSOP (forEach {A} {B}{{sopA}} as f) = SOP× {{ StSOP as }}{{ SOPMaybe {{ SOPΣ {{ sopA }} {{ λ {x} → StSOP (f x) }}}}}}
-- -- seed (forEach {A} {B} as f) = do s ← seed as; pure (s , nothing)
-- -- step (forEach {A} {B} as f) (s , just (a , s')) = {!!}
-- -- step (forEach {A} {B} as f) (s , nothing)       = {!!}



-- -- {-# NON_TERMINATING #-}
-- -- bindNothing : ∀ {A B : Set} (as : Stream A) (f : A → Stream B)
-- --         → S as → Step (S as × Maybe (∃ (S ∘ f))) B
-- -- bindJust : ∀ {A B : Set} (as : Stream A) (f : A → Stream B)
-- --         → S as × ∃ (S ∘ f) → Step (S as × Maybe (∃ (S ∘ f))) B
-- -- bindNothing as f sa with step as sa
-- -- ... | done        = done
-- -- ... | yield a sa' = bindJust as f (sa , a , seed (f a))
-- -- bindJust as f (sa , a , sb) with step (f a) sb
-- -- ... | done       = bindNothing as f sa
-- -- ... | yield b sb = yield b (sa , just (a , sb))

-- -- bind : ∀ {A B} → Stream A → (A → Stream B) → Stream B
-- -- S    (bind as f)                     = S as × Maybe (∃ (S ∘ f))
-- -- step (bind as f) (s , nothing)       = bindNothing as f s
-- -- step (bind as f) (s , just (a , s')) = bindJust as f (s , a , s')
-- -- seed (bind as f)                     = seed as , nothing


-- -- St    (forEach {A} {B} as f)            = St as × Maybe (Σ A (St ∘ f))
-- -- StSOP (forEach {A} {B} {{ sopA }} as f) = SOP× {{ StSOP as }}{{ SOPMaybe {{ SOPΣ {{ sopA }} {{ λ {x} → StSOP (f x) }}}}}}
-- -- seed  (forEach {A} {B} as f)            = seed as , nothing

-- -- step (forEach {A} {B} as f) (s , just (a , s')) = step (f a) s' <&> λ where
-- --   stop         → skip (s , nothing)
-- --   (skip s')    → skip (s , just (a , s'))
-- --   (yield b s') → yield b (s , (just (a , s')))
-- -- step (forEach {A} {B} as f) (s , nothing) = step as s <&> λ where
-- --   stop        → stop
-- --   (skip s)    → skip (s , nothing)
-- --   (yield a s) → skip (s , just (a , seed (f a)))

-- genLetPull : ∀ {A} → ↑V A → Pull (↑V A)
-- genLetPull a = mapGen (single a) genLet

-- -- countFrom : ↑V ℕ∘ → Pull (↑V ℕ∘)
-- -- St   (countFrom n)   = ↑V ℕ∘
-- -- seed (countFrom n)   = just n
-- -- step (countFrom n) s = pure $ yield s (s +∘ lit∘ 1)

-- -- count : Pull (↑V ℕ∘)
-- -- count = countFrom (lit∘ 0)

-- -- take : ∀ {A} → ↑V ℕ∘ → Pull A → Pull A
-- -- St    (take n as) = ↑V ℕ∘ × St as
-- -- seed  (take n as) = (n ,_) <$> seed as
-- -- step  (take n as) (i , s) = caseM (i ==∘ lit∘ 0) λ where
-- --   true  → pure stop
-- --   false → step as s <&> λ where
-- --     stop        → stop
-- --     (halt a)    → halt a
-- --     (yield a s) → yield a ((i -∘ lit∘ 1) , s)

-- -- drop : ∀ {A} → ↑V ℕ∘ → Pull A → Pull A
-- -- drop = {!!}

-- --------------------------------------------------------------------------------

-- -- foldrPull : ∀ {A B} → Pull A → (A → ↑ B → ↑ B) → ↑ B → ↑ B
-- -- foldrPull {A} {B} (pull S nothing step) f b = b
-- -- foldrPull {A} {B} (pull S (just s) step) f b =
-- --   LetRec (funTypes (Rep {S}) B)
-- --          (λ fs → absFun λ s → unGen (step (decode s)) λ where
-- --                      stop        → b
-- --                      (halt a)    → f a b
-- --                      (yield a s) → f a (callFun fs (encode s)))
-- --          (λ fs → callFun fs (encode s))

-- --------------------------------------------------------------------------------

-- -- toList : ∀ {A} → Pull (↑V A) → ↑V (List∘ A)
-- -- toList as = foldrPull as cons∘ nil∘

-- -- foldlPull : ∀ {A B} → Pull A → (↑V B → A → ↑V B) → ↑V B → ↑V B
-- -- foldlPull as f b = foldrPull as (λ a hyp → Λ λ b → hyp ∙ f b a) (Λ λ b → b) ∙ b

-- dup : ∀ {A} → Pull A → Pull (A × A)
-- dup as = (λ x → x , x) <$> as

-- zip : ∀ {A B} → Pull (↑V A) → Pull (↑V B) → Pull (↑V (A ×∘ B))
-- zip as bs = _,∘_ <$> as <*> bs

-- -- casePull : ∀ {A B}⦃ _ : Split A ⦄ ⦃ _ : IsSOP (SplitTo {A}) ⦄ → ↑V A → (SplitTo {A} → Pull B) → Pull B
-- -- casePull {A} {B} a f = forEach (mapGen (single a) split) f

-- -- -- casePull' : ∀ {A B}⦃ _ : Split A ⦄ ⦃ _ : IsSOP (SplitTo {A}) ⦄ → ↑V A → (SplitTo {A} → Pull B) → Pull B
-- -- -- casePull' {A} {B} {{_}}{{sopA}} a f =
-- -- --   pull (Σ (SplitTo {A}) (St ∘ f)) {{SOPΣ{{sopA}}{{λ {x} → StSOP (f x)}}}} ({!!} , {!!}) {!!}

-- -- sumPull : Pull (↑V ℕ∘) → ↑V ℕ∘
-- -- sumPull as = foldlPull as _+∘_ 0

-- -- --------------------------------------------------------------------------------
