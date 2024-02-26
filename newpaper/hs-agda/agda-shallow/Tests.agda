
module Tests where

open import Lib
open import Gen
open import Object
open import Pull5
open import Join
open import Split
open import SOP
open import Improve
open import MonadTailRec

--------------------------------------------------------------------------------

-- Comment this in for streamlined printing:

postulate
  CALL : {A : Set} → A

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
{-# DISPLAY _,C_ x y  = x , y #-}
{-# DISPLAY fst∘ x = CALL #-}

--------------------------------------------------------------------------------


test1 : Pull (↑V ℕ∘)
test1 = single 10 <> single 20

test2 : Pull (↑V ℕ∘)
test2 = mempty

test3 : Pull (↑V ℕ∘)
test3 = forEach (take 10 count) λ x →
        (take 20 count) <&>ₚ λ y → x +∘ y

test3'' : Pull (↑V ℕ∘)
test3'' = forEach (take 10 count) λ x →
          forEach (take 20 count) λ y →
          forEach (take 30 count) λ z →
          single (x +∘ y +∘ z)

foo = toList (zip count count)

-- the normal form of (sumPull test3)
sumPullTest3Code =
  LetRec _
  (λ fs →
     (Λ λ a → Λ λ x →
         caseBool∘ (a ==∘ 0)
           (Λ (λ b → b))
           (fieldC2 fs ∙ (a -∘ 1) ∙ (x +∘ 1) ∙ x ∙ 5 ∙ 0))
     ,C
     (Λ λ a → Λ λ a₁ → Λ λ a₂ → Λ λ a₃ → Λ λ x →
                    caseBool∘ (a₃ ==∘ 0)
                      (fieldC1 fs ∙ a ∙ a₁)
                      (fieldC3 fs ∙ a ∙ a₁ ∙ a₂ ∙ (a₃ -∘ 1) ∙ (x +∘ 1) ∙ x))
     ,C
     (Λ λ a → Λ λ a₁ → Λ λ a₂ → Λ λ a₃ → Λ λ a₄ → Λ λ x → Λ λ b →
                          fieldC4 fs ∙ a ∙ a₁ ∙ a₂ ∙ a₃ ∙ a₄ ∙ x ∙ (b +∘ (a₂ +∘ x)))

     ,C (Λ λ a → Λ λ a₁ → Λ λ a₂ → Λ λ a₃ → Λ λ a₄ → Λ λ x → fieldC2 fs ∙ a ∙ a₁ ∙ a₂ ∙ a₃ ∙ a₄)

     ,C ttC)
  (λ fs → fst∘ fs ∙ 10 ∙ 0)
  ∙ 0

test4 : Pull (↑ (V ℕ∘))
test4 = _*∘_ <$>ₚ take 10 count <*>ₚ take 10 (countFrom 20)

test5 : Pull (↑ (V ℕ∘))
test5 = _+∘_ <$>ₚ test3 <*>ₚ test3

test6 : Pull (↑ (V ℕ∘))
test6 = forEach (take 100 (countFrom 0)) λ x →
        genLetₚ (x *∘ 2) λ y →
        caseₚ (x <∘ 50) λ where
          true  → take y (countFrom x)
          false → single y

--------------------------------------------------------------------------------

-- Code size exponential in the number of "case'" here, because
-- everything gets inlined in case branches.
test7 : ↑C (ℕ∘ ⇒ StateT∘ ℕ∘ (MaybeT∘ Identity∘) ⊤∘)
test7 = Λ λ x → down do
  caseM (x ==∘ 10) λ where
    true  → modify' (_+∘_ 10)
    false → modify' (_+∘_ 20)
  caseM (x ==∘ 10) λ where
    true  → modify' (_+∘_ 10)
    false → modify' (_+∘_ 20)
  caseM (x ==∘ 10) λ where
    true  → modify' (_+∘_ 10)
    false → modify' (_+∘_ 20)
  caseM (x ==∘ 10) λ where
    true  → modify' (_+∘_ 10)
    false → modify' (_+∘_ 20)

-- Code size is linear.
test8 : ↑C (ℕ∘ ⇒ StateT∘ ℕ∘ (MaybeT∘ Identity∘) ⊤∘)
test8 = Λ λ x → down do
  join $ caseM (x ==∘ 10) λ where
    true  → modify' (_+∘_ 10)
    false → modify' (_+∘_ 20)
  join $ caseM (x ==∘ 10) λ where
    true  → modify' (_+∘_ 10)
    false → modify' (_+∘_ 20)
  join $ caseM (x ==∘ 10) λ where
    true  → modify' (_+∘_ 10)
    false → modify' (_+∘_ 20)
  caseM (x ==∘ 10) λ where
    true  → modify' (_+∘_ 10)
    false → modify' (_+∘_ 20)

test8' : ↑C (ℕ∘ ⇒ StateT∘ ℕ∘ (MaybeT∘ Identity∘) ⊤∘)
test8' = Λ λ x → down do
  up =<< (genLet $ down {F = StateT∘ ℕ∘ (MaybeT∘ Identity∘)} $ caseM (x ==∘ 10) λ where
    true  → modify' (_+∘_ 10)
    false → modify' (_+∘_ 20))
  modify' (_+∘_ 10)

-- The "fail" branches jump immediately to the "catch" code
test9 : ↑C (ℕ∘ ⇒ StateT∘ ℕ∘ (MaybeT∘ Identity∘) ⊤∘)
test9 = Λ λ x → down $
  catch (do
    join $ caseM (x ==∘ 10) λ where
      true  → modify' (_+∘_ 10)
      false → fail
    caseM (x ==∘ 15) λ where
      true  → modify' (_+∘_ 10)
      false → fail)
    (modify' (_+∘_ 11))

-- Monadic tail recursion on lists
test10 : ↑C (List∘ ℕ∘ ⇒ StateT∘ ℕ∘ (MaybeT∘ Identity∘) ⊤∘)
test10 = DefRec λ rec → Λ λ ns → downTC do
  caseM ns λ where
    nil         → ret tt∘
    (cons n ns) → caseM (n ==∘ 10) λ where
      true  → fail
      false → do modify' (_+∘_ 20); tailcall1 rec ns

-- Same but with less-efficient non-tail call
test10' : ↑C (List∘ ℕ∘ ⇒ StateT∘ ℕ∘ (MaybeT∘ Identity∘) ⊤∘)
test10' = DefRec λ rec → Λ λ ns → down do
  caseM ns λ where
    nil         → pure tt∘
    (cons n ns) → caseM (n ==∘ 10) λ where
      true  → fail
      false → do modify' (_+∘_ 20); up $ rec ∙ ns

-- Non-tail recursion on lists
test11 : ↑C (List∘ ℕ∘ ⇒ StateT∘ ℕ∘ (MaybeT∘ Identity∘) (List∘ ℕ∘))
test11 = DefRec λ rec → Λ λ ns → down do
  caseM ns λ where
    nil         → pure nil∘
    (cons n ns) → do
      ns ← up (rec ∙ ns)
      caseM (n ==∘ 10) λ where
        true  → fail
        false → do
          modify' (_*∘ 10)
          pure $ cons∘ (n +∘ 1) ns

filterM : ∀ {F M A}⦃ _ : Improve F M ⦄ → (↑V A → M Bool) → ↑C (List∘ A ⇒ F (List∘ A))
filterM f = DefRec λ rec → Λ λ as → down $ caseM as λ where
  nil         → pure nil∘
  (cons a as) → do
    as ← up $ rec ∙ as
    f a >>= λ where
      true  → pure (cons∘ a as)
      false → pure as

-- Interesting!
filterM' : ∀ {F M A}⦃ _ : Improve F M ⦄ → (↑V A → M Bool) → ↑C (List∘ A ⇒ F (List∘ A))
filterM' f = DefRec λ rec → Λ λ as → down $ caseM as λ where
  nil         → pure nil∘
  (cons a as) → (λ {true as → cons∘ a as; false as → as}) <$> f a <*> up (rec ∙ as)

myfilter : ↑C (List∘ ℕ∘ ⇒ MaybeT∘ Identity∘ (List∘ ℕ∘))
myfilter = filterM λ n → caseM (n ==∘ 10) λ where
  true  → pure false
  false → caseM (n ==∘ 0) λ where
    true  → fail
    false → pure true

-- fill the leaves of a tree from a list
-- throw error if we have 0 in list.
papertest : ↑C (Tree∘ ℕ∘ ⇒ StateT∘ (List∘ ℕ∘) (MaybeT∘ Identity∘) (Tree∘ ℕ∘))
papertest = DefRec λ f → Λ λ t → down $
  caseM t λ where
    leaf → pure leaf∘
    (node n l r) → do
      caseM (n ==∘ 0) λ where
        true → lift (maybeT (pure nothing))
        false → pure tt∘
      ns ← get
      n ← join $ caseM ns λ where
        nil         → pure n
        (cons n ns) → do put' ns; pure n
      node∘ n <$> up (f ∙ l) <*> up (f ∙ r)


--------------------------------------------------------------------------------

test12 : Pull (↑V ℕ∘)
test12 =
         forEach (take 30 (countFrom 0)) λ n →
         forEach (take 10 (countFrom (n *∘ 2))) λ m →
         drop 10 (countFrom m)

test12' : Pull (↑V ℕ∘)
test12' = mapGen (take 30 (countFrom 0)) λ n → do
  m ← genLet (n *∘ 2)
  caseM (m <∘ 20) λ where
    true  → pure (n +∘ 10)
    false → pure (n +∘ 20)

test13 : Pull (↑V ℕ∘)
test13 = genLetₚ (lit∘ 0) single
-- forEach (single (lit∘ 0)) single


--------------------------------------------------------------------------------
