
module Examples where

{-
Examples. You can use Agda's normalization command (C-c-n in Emacs) to print object code output.
-}

open import Lib
open import Object
open import Gen
open import Improve4
open import Join
open import Split
open import SOP
-- open import Pull

-- Customizing printing
--------------------------------------------------------------------------------

-- If you comment these in, you get a clean-up printing output that
-- erases a bunch of newtype and object language artifacts.
-- Comment them out to get faithful printing output that can be re-checked.

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

-- Comment this in to only display "CALL" for functions in a mutual block; the
-- iterated projections can get very verbose if we have many functions.
-- Comment out to get the projections in all their glory.
postulate
  CALL : {A : Set} → A
{-# DISPLAY fst∘ x = CALL #-}

-- Basics
--------------------------------------------------------------------------------

-- id2 = <λ b. ~(id <b>)>
id2 : ↑C (Bool∘ ⇒ V Bool∘)
id2 = Λ λ b → id b

-- add := letrec go n m := case (n == 0) of
--                 True  → m
--                 False → 1 + go (n - 1) m;
--        go
add : ↑C (ℕ∘ ⇒ C (ℕ∘ ⇒ V ℕ∘))
add = LetRec
  _
  (λ go → Λ λ n → Λ λ m → caseBool∘ (n ==∘ 0) m (1 +∘ go ∙ (n -∘ 1) ∙ m))
  λ go → go

{-
map f as =
  <letrec go as := case as of
             Nil       → Nil
             Cons a as → Cons ~(f <a>) (go as);
   go ~as>
-}
map∘ : {A B : VTy} → (↑V A → ↑V B) → ↑V (List∘ A) → ↑V (List∘ B)
map∘ f as = LetRec
  _
  (λ go → Λ λ as → caseList∘ as
                      nil∘
                      (λ a as → cons∘ (f a) (go ∙ as)))
  λ go → go ∙ as


-- Monads
--------------------------------------------------------------------------------

-- Code size exponential in the number of caseM-s here, because
-- everything gets inlined in case branches.
exM1 : ↑C (ℕ∘ ⇒ StateT∘ ℕ∘ (MaybeT∘ Identity∘) ⊤∘)
exM1 = Λ λ x → down do
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
exM2 : ↑C (ℕ∘ ⇒ StateT∘ ℕ∘ (MaybeT∘ Identity∘) ⊤∘)
exM2 = Λ λ x → down do
  join $ caseM (x ==∘ 10) λ where
    true  → modify (_+∘_ 10)
    false → modify (_+∘_ 20)
  join $ caseM (x ==∘ 10) λ where
    true  → modify (_+∘_ 10)
    false → modify (_+∘_ 20)
  join $ caseM (x ==∘ 10) λ where
    true  → modify (_+∘_ 10)
    false → modify (_+∘_ 20)
  caseM (x ==∘ 10) λ where
    true  → modify' (_+∘_ 10)
    false → modify' (_+∘_ 20)

-- The "fail" branches jump immediately to the "catch" clause
exM3 : ↑C (ℕ∘ ⇒ StateT∘ ℕ∘ (MaybeT∘ Identity∘) ⊤∘)
exM3 = Λ λ x → down $
  catch (join $ do
    caseM (x ==∘ 10) λ where
      true  → modify' (_+∘_ 10)
      false → fail
    caseM (x ==∘ 15) λ where
      true  → modify' (_+∘_ 10)
      false → fail)
    (do modify' (_+∘_ 11))

-- Monadic tail recursion on lists, the "tailcall1" result does not get matched.
exM4 : ↑C (List∘ ℕ∘ ⇒ StateT∘ ℕ∘ (MaybeT∘ Identity∘) ⊤∘)
exM4 = DefRec λ rec → Λ λ ns → down do
  caseM ns λ where
    nil         → pure tt∘
    (cons n ns) → caseM (n ==∘ 10) λ where
      true  → fail
      false → do modify' (_+∘_ 20)
                 {!!}
                 -- s ← get
                 -- tailcall (runIdentity∘ (runMaybeT∘ (runStateT∘ (rec ∙ ns) s)))

-- Section 3.6. example from the paper
exM5 : ↑C (Tree∘ ℕ∘ ⇒ StateT∘ (List∘ ℕ∘) (MaybeT∘ Identity∘) (Tree∘ ℕ∘))
exM5 = DefRec λ f → Λ λ t → down $
  caseM t λ where
    leaf         → pure leaf∘
    (node n l r) → do
      caseM (n ==∘ 0) λ where
        true  → fail
        false → pure tt∘
      ns ← get
      n  ← join $ caseM ns λ where
             nil         → pure n
             (cons n ns) → do put ns; pure n
      l ← up (f ∙ l)
      r ← up (f ∙ r)
      pure (node∘ n l r)

-- -- filterM where the recursive call is performed first
-- filterM : ∀ {F M A}⦃ _ : Improve F M ⦄ → (↑V A → M Bool) → ↑C (List∘ A ⇒ F (List∘ A))
-- filterM f = DefRec λ rec → Λ λ as → down $ caseM as λ where
--   nil         → pure nil∘
--   (cons a as) → do
--     as ← up $ rec ∙ as
--     f a >>= λ where
--       true  → pure (cons∘ a as)
--       false → pure as

-- -- filterM where we make a tail call in the false case
-- filterM' : ∀ {F M A}⦃ _ : Improve F M ⦄ → (↑V A → M Bool) → ↑C (List∘ A ⇒ F (List∘ A))
-- filterM' f = DefRec λ rec → Λ λ as → down $ caseM as λ where
--   nil         → pure nil∘
--   (cons a as) → f a >>= λ where
--       true  → do as ← up (rec ∙ as); pure (cons∘ a as)
--       false → tailcall (rec ∙ as)

-- exM6 : ↑C (List∘ ℕ∘ ⇒ StateT∘ ℕ∘ (MaybeT∘ Identity∘) (List∘ ℕ∘))
-- exM6 = filterM' λ n → caseM (n ==∘ 0) λ where
--   true  → fail
--   false → split (n <∘ 20)

-- exM7 : ↑C (List∘ ℕ∘ ⇒ StateT∘ ℕ∘ (MaybeT∘ Identity∘) (List∘ ℕ∘))
-- exM7 = filterM λ n → caseM (n ==∘ 0) λ where
--   true  → fail
--   false → split (n <∘ 20)

-- -- -- Streams
-- -- --------------------------------------------------------------------------------

-- -- -- To print stream code, it is usally the clearest to use "toList", e.g.
-- -- -- "toList exS1".

-- -- exS1 : Pull (↑V ℕ∘)
-- -- exS1 = consₚ 10 $ consₚ 20 empty

-- -- exS2 : Pull (↑V ℕ∘)
-- -- exS2 = forEach (take 20 count) λ x → (take 20 count) <&>ₚ (λ y → x +∘ y)

-- -- exS3 : Pull (↑V ℕ∘)
-- -- exS3 = forEach (take 10 count) λ x →
-- --        forEach (take 20 count) λ y →
-- --        forEach (take 30 count) λ z →
-- --        single (x +∘ y +∘ z)

-- -- exS4 : ↑V (List∘ (ℕ∘ ×∘ ℕ∘))
-- -- exS4 = toList (zip count exS2)

-- -- exS5 : Pull (↑ (V ℕ∘))
-- -- exS5 = _*∘_ <$>ₚ take 10 count <*>ₚ take 10 (countFrom 20)

-- -- exS6 : Pull (↑ (V ℕ∘))
-- -- exS6 = _+∘_ <$>ₚ exS2 <*>ₚ count

-- -- -- Section 4.4 example in paper:
-- -- exS7 : Pull (↑ (V ℕ∘))
-- -- exS7 = forEach (take 100 (countFrom 0)) λ x →
-- --        genLetₚ (x *∘ 2) λ y →
-- --        caseₚ (x <∘ 50) λ where
-- --          true  → take y (countFrom x)
-- --          false → single y

-- -- -- Medium-sized zip
-- -- exS8 = zip exS7 exS2
