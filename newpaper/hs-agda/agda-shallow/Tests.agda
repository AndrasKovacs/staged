
module Tests where

open import Lib
open import Gen
open import Object
open import Pull
open import Join
open import Split
open import SOP
open import Improve
open import MonadTailRec

--------------------------------------------------------------------------------

-- Comment this in for streamlined printing:

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

--------------------------------------------------------------------------------


test1 : Pull (↑V ℕ∘)
test1 = single 10 <> single 20

test2 : Pull (↑V ℕ∘)
test2 = mempty

test3 : Pull (↑V ℕ∘)
test3 = forEach (take 10 count) λ x →
        forEach (take 5  count) λ y →
        single (x +∘ y)

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
test4 = _*∘_ <$> take 10 count <*> take 10 (countFrom 20)

test5 : Pull (↑ (V ℕ∘))
test5 = _+∘_ <$> test3 <*> test3

test6 : Pull (↑ (V ℕ∘))
test6 = forEach (take 20 count) λ x →
        casePull (x <∘ 10) λ where
          true  → forEach (take 10 count) λ y → single (x *∘ y)
          false → single (x *∘ 5)

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



--------------------------------------------------------------------------------
