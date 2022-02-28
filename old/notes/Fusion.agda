{-# OPTIONS --type-in-type #-}

module Fusion where

open import Data.Product renaming  (proj₁ to ₁; proj₂ to ₂) hiding (map; zip)
open import Relation.Binary.PropositionalEquality
open import Data.Sum hiding (map)
open import Data.Bool
open import Data.Nat
open import Data.Unit
open import Data.Empty
open import Function
open import Data.Maybe hiding (map; zip; zipWith)
open import Data.List hiding (map; zip; _++_; filter; foldr; foldl; zipWith; drop; take)

module Stream1 where

  data Step (S A : Set) : Set where
    stop  : Step S A
    yield : A → S → Step S A
    skip  : S → Step S A

  mapStep : ∀ {A A' S S'} → (A → A') → (S → S') → Step S A → Step S' A'
  mapStep f g stop        = stop
  mapStep f g (yield a s) = yield (f a) (g s)
  mapStep f g (skip s)    = skip (g s)

  record Stream (A : Set) : Set where
    constructor stream
    field
      S    : Set
      seed : S
      step : S → Step S A
  open Stream public

  infixr 4 _++_
  _++_ : ∀ {A} → Stream A → Stream A → Stream A
  S    (xs ++ ys)           = S xs ⊎ S ys
  seed (xs ++ ys)           = inj₁ (seed xs)
  step (xs ++ ys) (inj₁ s ) with step xs s
  ... | stop       = skip (inj₂ (seed ys))
  ... | yield a s' = yield a (inj₁ s')
  ... | skip s'    = skip (inj₁ s')
  step (xs ++ ys) (inj₂ s) = mapStep id inj₂ (step ys s)

  filter : ∀ {A} → (A → Bool) → Stream A → Stream A
  S    (filter f as) = S as
  seed (filter f as) = seed as
  step (filter f as) s with step as s
  ... | stop       = stop
  ... | yield a s' = if f a then yield a s' else skip s'
  ... | skip s'    = skip s'

  map : ∀ {A B} → (A → B) → Stream A → Stream B
  S    (map f as) = S as
  seed (map f as) = seed as
  step (map f as) = mapStep f id ∘ step as

  empty : ∀ {A} → Stream A
  S    empty = ⊤
  seed empty = tt
  step empty = λ _ → stop

  pure : ∀ {A} → A → Stream A
  S    (pure a)       = Bool
  seed (pure a)       = true
  step (pure a) false = stop
  step (pure a) true  = yield a false

  bind : ∀ {A B} → Stream A → (A → Stream B) → Stream B
  S    (bind as f) = S as ⊎ (S as × ∃ (S ∘ f))
  seed (bind as f) = inj₁ (seed as)
  step (bind as f) (inj₁ s) with step as s
  ... | stop       = stop
  ... | yield a s' = skip (inj₂ (s' , a , seed (f a)))
  ... | skip s'    = skip (inj₁ s')
  step (bind as f) (inj₂ (s , a , s')) with step (f a) s'
  ... | stop        = skip (inj₁ s)
  ... | yield b s'' = yield b (inj₂ (s , a , s''))
  ... | skip s''    = skip (inj₂ (s , a , s''))

  zip : ∀ {A B} → Stream A → Stream B → Stream (A × B)
  S    (zip {A} as bs)                     = S as × S bs × Maybe A
  seed (zip {A} as bs)                     = seed as , seed bs , nothing
  step (zip {A} as bs) (sa , sb , nothing) with step as sa
  ... | stop        = stop
  ... | yield a sa' = skip (sa' , sb , just a)
  ... | skip sa'    = skip (sa' , sb , nothing)
  step (zip {A} as bs) (sa , sb , just a) with step bs sb
  ... | stop        = stop
  ... | yield b sb' = yield (a , b) (sa , sb' , nothing)
  ... | skip sb'    = skip (sa , sb' , just a)

  {-# NON_TERMINATING #-}
  foldr : ∀ {A B : Set} → (A → B → B) → B → Stream A → B
  foldr f b (stream S s step) with step s
  ... | stop       = b
  ... | yield a s' = f a (foldr f b (stream S s' step))
  ... | skip s'    = foldr f b (stream S s' step)

  {-# NON_TERMINATING #-}
  foldl : ∀ {A B : Set} → (B → A → B) → B → Stream A → B
  foldl f b (stream S s step) with step s
  ... | stop       = b
  ... | yield a s' = foldl f (f b a) (stream S s' step)
  ... | skip s'    = foldl f b (stream S s' step)

module Stream2 where

  data Step (S A : Set) : Set where
    stop  : Step S A
    yield : A → S → Step S A
    skip  : S → Step S A

  mapStep : ∀ {A A' S S'} → (A → A') → (S → S') → Step S A → Step S' A'
  mapStep f g stop        = stop
  mapStep f g (yield a s) = yield (f a) (g s)
  mapStep f g (skip s)    = skip (g s)

  record Prod (A : Set) : Set where
    constructor stream
    field
      S    : Set
      seed : S
      step : S → Step S A
  open Prod public

  infixr 4 _++_
  data Stream (A : Set) : Set where
    prod    : Prod A → Stream A
    _++_    : Stream A → Stream A → Stream A
    bind    : ∀ {B} → Stream B → (B → Stream A) → Stream A
    zipWith : ∀ {B C} → (B → C → A) → Stream B → Stream C → Stream A

  filter : ∀ {A} → (A → Bool) → Stream A → Stream A
  filter f (prod x) = {!!}
  filter f (str ++ str₁) = filter f str ++ filter f str₁
  filter f (bind str g) = bind str λ b → filter f (g b)
  filter f (zipWith x str str₁) = {!!} -- issue

  map : ∀ {A B} → (A → B) → Stream A → Stream B
  map f (prod as) = {!!}
  map f (as ++ as') = map f as ++ map f as'
  map f (bind bs g) = bind bs λ b → map f (g b)
      -- no bind fusion! (i.e. monad laws)
  map f (zipWith g bc cs) = {!!} -- can convert to prod, but better to freely add map...
                                 -- but then we don't have map fusion unless we just add it
                                 -- perhaps first-order syntax is OK, if we hand-write
                                 -- enough fusion?

  {-# NON_TERMINATING #-}
  foldr : ∀ {A B : Set} → (A → B → B) → B → Stream A → B
  foldr f b (prod x) = {!!}
  foldr f b (as ++ as') = foldr f (foldr f b as') as
  foldr {A} {B} f b (bind {C} cs g) = foldr (λ c b → foldr f b (g c)) b cs
  foldr {A} {B} f b (zipWith {C} {D} g cs ds) = {!!}
    -- first I have to convert cs/ds to producers to fold them!
    -- this means that optimization from first-order rep is blocked at the first zip!
    -- general solution: robust producer-to-state-machine compilation

module Stream3 where

  Step : Set → Set → Set
  Step S A = (Step : Set) → Step → (A → S → Step) → Step

  record Pull (A : Set) : Set where
    constructor pull
    field
      S    : Set
      seed : S
      step : S → Step S A
  open Pull public

  zip : ∀ {A B} → Pull A → Pull B → Pull (A × B)
  S    (zip as bs) = S as × S bs
  seed (zip as bs) = seed as , seed bs
  step (zip as bs) (sa , sb) _ stop yield =
    step as sa _
      stop
      λ a sa' → step bs sb _
        stop
        λ b sb' → yield (a , b) (sa , sb')

  map : ∀ {A B} → (A → B) → Pull A → Pull B
  S    (map f as) = S as
  seed (map f as) = seed as
  step (map f as) s _ stop yield = step as s _
    stop
    λ a s → yield (f a) s

  {-# NON_TERMINATING #-}
  find : ∀ {A S} → (A → Bool) → (S → Step S A) → S → Step S A
  find f step s _ stop yield = step s _
    stop
    λ a s → if f a then yield a s
                   else find f step s _ stop yield

  -- fusing filter without Skip!

  -- we can inline continuations in tail-recursive functions, and skipping to the next
  -- yield is such a loop
  filter : ∀ {A} → (A → Bool) → Pull A → Pull A
  S    (filter f as) = S as
  seed (filter f as) = seed as
  step (filter f as) = find f (step as)

  {-# NON_TERMINATING #-}
  drop' : ∀ {A S} → ℕ → (S → Step S A) → S → S
  drop' zero    step s = s
  drop' (suc n) step s = step s _ s λ a s → drop' n step s

  drop : ∀ {A} → ℕ → Pull A → Pull A
  S    (drop n as) = S as
  seed (drop n as) = drop' n (step as) (seed as)
  step (drop n as) = step as

  take : ∀ {A} → ℕ → Pull A → Pull A
  S    (take n as) = S as × ℕ
  seed (take n as) = seed as , n
  step (take n as) (s , zero)  _ stop yield = stop
  step (take n as) (s , suc x) _ stop yield = step as s _ stop λ a s → yield a (s , x)

  infixr 4 _++_
  _++_ : ∀ {A} → Pull A → Pull A → Pull A
  S    (xs ++ ys) = S xs ⊎ S ys
  seed (xs ++ ys) = inj₁ (seed xs)
  step (xs ++ ys) (inj₁ s) _ stop yield =
    step xs s _
      (step ys (seed ys) _
        stop
        λ a s → yield a (inj₂ s))
      λ a s → yield a (inj₁ s)
  step (xs ++ ys) (inj₂ s) _ stop yield =
    step ys s _
      stop
      λ a s → yield a (inj₂ s)

  {-# NON_TERMINATING #-}
  bind' : ∀ {A B : Set} (as : Pull A) (f : A → Pull B)
          → S as × Maybe (∃ (S ∘ f)) → Step (S as × Maybe (∃ (S ∘ f))) B
  bind' as f (sa , nothing)       _ stop yield =
    step as sa _
      stop
      λ a sa → step (f a) (seed (f a)) _
        (bind' as f (sa , nothing) _ stop yield)
        λ b sb → yield b (sa , (just (a , sb)))
  bind' as f (sa , just (a , sb)) _ stop yield =
    step (f a) sb _
      (bind' as f (sa , nothing) _ stop yield)
      λ b sb → yield b (sa , just (a , sb))

  {-# NON_TERMINATING #-}
  bindNothing' : ∀ {A B : Set} (as : Pull A) (f : A → Pull B)
          → S as → Step (S as × Maybe (∃ (S ∘ f))) B
  bindJust' : ∀ {A B : Set} (as : Pull A) (f : A → Pull B)
          → S as × ∃ (S ∘ f) → Step (S as × Maybe (∃ (S ∘ f))) B
  bindNothing' as f sa _ stop yield =
    step as sa _
      stop
      λ a sa → bindJust' as f (sa , a , seed (f a)) _ stop yield
  bindJust' as f (sa , a , sb) _ stop yield =
    step (f a) sb _
      (bindNothing' as f sa _ stop yield)
      λ b sb → yield b (sa , just (a , sb))

  bind : ∀ {A B} → Pull A → (A → Pull B) → Pull B
  S    (bind as f) = S as × Maybe (∃ (S ∘ f))
  seed (bind as f) = seed as , nothing
  step (bind as f) (sa , nothing) _ stop yield = bindNothing' as f sa _ stop yield
  step (bind as f) (sa , just x)  _ stop yield = bindJust' as f (sa , x) _ stop yield
