
module PullM where

open import Agda.Builtin.TrustMe

open import Lib
open import Object
open import Gen
open import SOP
open import Split
open import Join
open import Improve

data Step (S A : Set) : Set where
  stop  : Step S A
  skip  : S → Step S A
  yield : A → S → Step S A

-- Pull can be equivalently written using St : Uₛ instead, which yields Pull A : Set₀,
-- but St : Set is just more convenient to use in Agda.
{-# NO_UNIVERSE_CHECK #-}
record Pull (M : Set → Set)(A : Set) : Set where
  constructor pull
  field
    St        : Set
    skips     : Bool -- we track whether a stream can skip, to optimize zipping
    {{StSOP}} : IsSOP St
    seed      : M St
    step      : St → M (Step St A)
open Pull public

-- Convenience function for printing out all state shapes.
getStRep : ∀ {M A} → Pull M A → Uₛ
getStRep (pull _ _ {{StSOP}} _ _) = IsSOP.Rep StSOP


-- Zipping
--------------------------------------------------------------------------------

repeat : ∀ {M A}⦃ _ : Applicative M ⦄ → A → Pull M A
St    (repeat a)   = ⊤
seed  (repeat a)   = pure tt
skips (repeat a)   = false
step  (repeat a) _ = pure $ yield a tt

infixl 4 _<$>ₚ_
_<$>ₚ_ : ∀ {M A B}⦃ _ : Applicative M ⦄ → (A → B) → Pull M A → Pull M B
f <$>ₚ pull S skips seed step =
  pull S skips seed (λ s → step s <&> λ where
    stop        → stop
    (skip s)    → skip s
    (yield a s) → yield (f a) s)

infixl 1 _<&>ₚ_
_<&>ₚ_ : ∀ {A B M}⦃ _ : Applicative M ⦄ → Pull M A → (A → B) → Pull M B
_<&>ₚ_ = flip _<$>ₚ_

-- We have four cases, depending on whether the involved streams can skip.  We
-- don't enforce the "skip : Bool" flag in the types of transitions, we do it in
-- a weakly typed way, and just halt on "impossible" skips.
-- Only in the skip-skip case do we have to extend the state with a Maybe.
infixl 4 _<*>ₚ_
_<*>ₚ_ : ∀ {A B M}⦃ _ : IsSOP A ⦄ ⦃ _ : Monad M ⦄ → Pull M (A → B) → Pull M A → Pull M B
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
      (skip s')    → pure $ stop -- impossible
      (yield a s') → step s >>= λ where
        stop        → pure stop
        (skip s)    → pure $ skip (s , s')
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

zip : ∀ {A B M}⦃ _ : Monad M ⦄ → Pull M (↑V A) → Pull M (↑V B) → Pull M (↑V (A ×∘ B))
zip as bs = _,∘_ <$>ₚ as <*>ₚ bs


-- Appending
--------------------------------------------------------------------------------

empty : ∀ {A M}⦃ _ : Applicative M ⦄ → Pull M A
St   empty   = ⊤
seed empty   = pure tt
skips empty  = false
step empty _ = pure stop

append : ∀ {A M}⦃ _ : Monad M ⦄ → Pull M A → Pull M A → Pull M A
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

-- optimized definition for prepending an element
consₚ : ∀ {A M}⦃ _ : Monad M ⦄ → A → Pull M A → Pull M A
consₚ a (pull S skips seed step) =
  pull (Maybe S) skips (pure nothing) λ where
    nothing  → do s ← seed; pure $ yield a (just s)
    (just s) → step s >>= λ where
      stop        → pure stop
      (skip s)    → pure $ skip (just s)
      (yield a s) → pure $ yield a (just s)

instance
  SemigroupPull : ∀ {A M}⦃ _ : Monad M ⦄ → Semigroup (Pull M A)
  Semigroup._<>_ SemigroupPull = append

  MonoidPull : ∀ {A M}⦃ _ : Monad M ⦄ → Monoid (Pull M A)
  Monoid.mempty MonoidPull = empty


-- Binding
--------------------------------------------------------------------------------

single : ∀ {A M}⦃ _ : Applicative M ⦄ → A → Pull M A
St    (single a) = Bool
seed  (single a) = pure true
skips (single a) = false
step (single a) true  = pure $ yield a false
step (single a) false = pure stop

-- We manually give the IsSOP instance for the state, because Agda is not able
-- to implicitly project instances from the (A → Pull B) function.
forEach : ∀ {A B M} ⦃ _ : IsSOP A ⦄ ⦃ _ : Monad M ⦄ → Pull M A → (A → Pull M B) → Pull M B
St    (forEach {A} {B} as f)            = St as × Maybe (Σ A (St ∘ f))
skips (forEach {A} {B} as f)            = true
StSOP (forEach {A} {B} {{ sopA }} as f) = SOP× {{StSOP as}}{{SOPMaybe {{SOPΣ {{sopA}} {{λ{x} → StSOP (f x)}}}}}}
seed  (forEach {A} {B} as f)            = (_, nothing) <$> seed as
step (forEach {A} {B} as f) (s , just (a , s')) = step (f a) s' <&> λ where
  stop         → skip (s , nothing)
  (skip s')    → skip (s , just (a , s'))
  (yield b s') → yield b (s , (just (a , s')))
step (forEach {A} {B} {M} as f) (s , nothing) = step as s >>= λ where
  stop        → pure stop
  (skip s)    → pure $ skip (s , nothing)
  (yield a s) → do s' ← seed (f a); pure {F = M} $ skip (s , just (a , s'))

-- Let-insertion and case splitting
--------------------------------------------------------------------------------

-- relies on generative skips fields
famSkips' : ∀ {A B M} → (Elₛ A → Pull M B) → Bool
famSkips' {[]}    f = false
famSkips' {A ∷ B} f = or ((f ∘ here) (loopₚ {A}) .skips) (famSkips' {B} (f ∘ there))

famSkips : ∀ {A B M}⦃ _ : IsSOP A ⦄ → (A → Pull M B) → Bool
famSkips {A} f = famSkips' {Rep{A}} (f ∘ decode)

bindM : ∀ {A B M}⦃ _ : IsSOP A ⦄ ⦃ _ : Monad M ⦄ → M A → (A → Pull M B) → Pull M B
bindM {A} {B}{M}{{sopA}} ma f =
  pull (Σ A (St ∘ f)) (famSkips f) {{SOPΣ{{sopA}}{{λ {x} → StSOP (f x)}}}}
       (do a ← ma; s ← seed (f a); pure {F = M} (a , s)) λ where
         (a , s) → step (f a) s <&> λ where
            stop        → stop
            (skip s)    → skip (a , s)
            (yield b s) → yield b (a , s)

genLetₚ : ∀ {A B M}⦃ _ : MonadGen M ⦄ → ↑V A → (↑V A → Pull M B) → Pull M B
genLetₚ a = bindM (genLet a)

caseₚ : ∀ {A B M}⦃ _ : Split A ⦄ ⦃ _ : IsSOP (SplitTo {A})⦄ ⦃ _ : MonadGen M ⦄
        → ↑V A → (SplitTo {A} → Pull M B) → Pull M B
caseₚ a = bindM (split a)

-- Random library functions
--------------------------------------------------------------------------------

dup : ∀ {A M}⦃ _ : Applicative M ⦄ → Pull M A → Pull M (A × A)
dup as = (λ x → x , x) <$>ₚ as

countFrom : ∀{M}⦃ _ : Applicative M ⦄ → ↑V ℕ∘ → Pull M (↑V ℕ∘)
St   (countFrom n)   = ↑V ℕ∘
seed (countFrom n)   = pure n
skips (countFrom n)  = false
step (countFrom n) s = pure $ yield s (s +∘ lit∘ 1)

count : ∀{M}⦃ _ : Applicative M ⦄ → Pull M (↑V ℕ∘)
count = countFrom (lit∘ 0)

take : ∀{M}⦃ _ : MonadGen M ⦄{A} → ↑V ℕ∘ → Pull M A → Pull M A
St    (take n as) = ↑V ℕ∘ × St as
seed  (take n as) = (n ,_) <$> seed as
skips (take n as) = skips as
step  (take n as) (i , s) = caseM (i ==∘ lit∘ 0) λ where
  true  → pure stop
  false → step as s <&> λ where
    stop        → stop
    (skip s)    → skip (i , s)
    (yield a s) → yield a ((i -∘ lit∘ 1) , s)

drop : ∀ {A}{M}⦃ _ : MonadGen M ⦄ → ↑V ℕ∘ → Pull M A → Pull M A
St   (drop n as) = Either (↑V ℕ∘ × St as) (St as)
seed (drop n as) = left ∘ (_,_ n) <$> seed as
skips (drop n as) = true
step (drop n as) (left (i , s)) = caseM (i ==∘ lit∘ 0) λ where
  true  → pure $ skip (right s)
  false → step as s >>= λ where
    stop        → pure stop
    (skip s)    → pure $ skip (left (i , s))
    (yield _ s) → pure $ skip (left (i -∘ 1 , s))
step (drop n as) (right s) = step as s <&> λ where
  stop        → stop
  (skip s)    → skip (right s)
  (yield a s) → yield a (right s)

filter : ∀ {A M}⦃ _ : Monad M ⦄ → (A → M Bool) → Pull M A → Pull M A
St   (filter f as) = St as
skips (filter f as) = true
seed (filter f as) = seed as
step (filter f as) s = step as s >>= λ where
  stop        → pure stop
  (skip s)    → pure $ skip s
  (yield a s) → f a >>= λ where
                  true  → pure $ yield a s
                  false → pure $ skip s

-- Folding
--------------------------------------------------------------------------------

FunSOP↑C : Uₛ → Ty → CTy
FunSOP↑C []      R = ⊤C
FunSOP↑C (A ∷ B) R = (A →PT R) ×C FunSOP↑C B R

tailcallC : ∀ {A B}(F : VTy → Ty) → ↑C (FunSOP↑C A (F B)) → Elₛ A → ↑ (F B)
tailcallC F fs (here  x) = appₚₜ (fst∘ fs) x
tailcallC F fs (there x) = tailcallC F (snd∘ fs) x

tabulateC : ∀ {A B} → (Elₛ A → ↑ B) → ↑C (FunSOP↑C A B)
tabulateC {[]}    f = ttC
tabulateC {A ∷ B} f = lamₚₜ (f ∘ here) ,C tabulateC (f ∘ there)

foldr : ∀ {A B F M}⦃ _ : Improve F M ⦄ → (A → ↑(F B) → M (TailCall F B))
                                       → M (↑V B) → Pull M A → ↑ (F B)
foldr {A} {B}{F}{M} f b (pull S _ seed step) =
  LetRec (FunSOP↑C (Rep {S}) (F B))
         (λ go → tabulateC λ s → downTC $ step (decode s) >>= λ where
           stop        → ret' <$> b
           (skip s)    → call $ tailcallC F go (encode s)
           (yield a s) → f a (tailcallC F go (encode s))
         )
  λ go → downTC (do s ← seed; call $ tailcallC F go (encode s))

toList : ∀ {A F M}⦃ _ : Improve F M ⦄ → Pull M (↑V A) → ↑ (F (List∘ A))
toList as = foldr (λ a as → do as ← up as; ret (cons∘ a as)) (pure nil∘) as

mapPull : ∀ {M N A} → (∀{A} → M A → N A) → Pull M A → Pull N A
mapPull f (pull S skips seed step) = pull S skips (f seed) (f ∘ step)

foldl : ∀ {A B F M}⦃ _ : Improve F M ⦄ → (↑V B → A → ↑V B) → ↑V B → Pull M A → ↑(F B)
foldl {A} {B} {F} {M} f b as =
  runReaderT∘ (foldr (λ a hyp → call (readerT∘ λ r → runReaderT∘ hyp (f r a)))
                     (pure b)
                     (mapPull lift as))              b

sum : ∀ {F M}⦃ _ : Improve F M ⦄ → Pull M (↑V ℕ∘) → ↑ (F ℕ∘)
sum = foldl _+∘_ 0

-- --------------------------------------------------------------------------------
