
{-# language LambdaCase, TemplateHaskell, BlockArguments, RankNTypes,
    MultiParamTypeClasses, FunctionalDependencies, TypeApplications,
    ScopedTypeVariables, UndecidableInstances, QuantifiedConstraints,
    TypeFamilies, CPP, PartialTypeSignatures, AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Tests where

import Prelude hiding (filter, zip, zipWith, drop, take)
import qualified Prelude as P
import Language.Haskell.TH
import Control.Monad.State.Strict
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Trans
import GHC.Types
import GHC.Exts
import Control.Monad.Morph
import Control.Monad.Trans.Maybe

import Up (Up)
import qualified Up as U
import GenNoCurry4

#include "Sugar.h"

--------------------------------------------------------------------------------

p :: [Int]
p = $$(list $ push $
     bindPull (range 0 10) \n ->
     bindPull (ifPull (U.even n) (range 0 n) (range n (n + 4))) \m ->
     bindPull (range 0 10) \k ->
     single (n + m + k)
     )

q :: [Int]
q = $$(list $ do
  n <- push (range 0 10)
  m <- caseG' (U.even n) \case
    True  -> push $ range 0 n
    False -> push $ range n (n + 4)
  k <- push (range 0 10)
  pure ( n + m + k))

-- state size is additive in bindPull...

-- q :: [Int]
-- q = $$(list $ fmap (+20) $ push $ range 0 10)

-- single introduces an administrative machine state
-- not too bad

-- p2 :: [Int]
-- p2 = $$(list $ push $
--   bindPull (range 0 10) \n ->
--   genPull (n + 100) \m ->
--   caseP (U.even n) \case
--     True -> _
--     False -> _
--     (range n (n + 5))
--     (range 0 n)
--    )

-- Up a -> Gen b
-- Up a -> Pull b

-- Up a -> Gen (Split a)

-- p1 :: Int -> Int -> [Int]
-- p1 x y = $$(list $ push $
--   bindUp (range [||x||] [||y||]) \n ->
--   bindUp (range (10::Up Int) 20) \m ->
--   single (m + 2000)
--   )

-- p2 :: Int -> Int -> [Int]
-- p2 x y = $$(list $ do
--   n <- push $ range [||x||] [||y||]
--   m <- push $ range 10 20
--   pure $ n + 2000)

-- p3 :: [Int]
-- p3 = $$(list $ push $
--        branchPull (bindGen (range 10 100) \n -> caseG' (n U.< 30) \case
--              True  -> pure (Left n);
--              False -> pure (Right n))
--          (const <$> range 0 5)
--          ((\a b _ -> a + b) <$> range 0 10 <*> range 90 100))

-- p4 :: [Int]
-- p4 = $$(list $ push $
--        branchE (range 0 10)
--                (\n -> caseG' (U.even n) \case True -> pure $ Left n
--                                               False -> pure $ Right n)
--                (\n -> range n (n + 10))
--                (\n -> range 0 n)
--                )


-- printUp :: Up a -> IO ()
-- printUp a = do
--   x <- unType <$> runQ (examineCode a)
--   print $ ppr x

-- p1 :: Int
-- p1 = $$(sumPush $ push Q([(0::Int)..10]))

-- -- p1 :: State Int Int
-- -- p1 = $$(down do
-- --   modifyG (+10)
-- --   get
-- --   )

-- -- downBool :: Bool -> Gen (Up

-- ((), Int)
-- [()] * [Int]  = [Up (Pair () Int)]
-- Product overhead comes from only having sums, not SOP-s

-- find' :: [Int] -> (Int -> Bool) -> MaybeT Identity Int
-- find' as f = $$(down $ find [||as||] (upFun [||f||]))

-- p1 :: State Int ()
-- p1 = $$(down do
--   n <- get
--   caseG (n U.== 10) \case
--     True  -> putG 100
--     False -> pure U.tt
--   )



-- p2 :: (Bool -> Bool) -> ReaderT Int (ExceptT Bool (State Bool)) ()
-- p2 f = $$(down do
--   n <- get
--   caseG n \case
--     True  -> modifyG (\x -> Q(f $$x))
--     False -> pure U.tt -- throwError [||True||]
--   caseG n \case
--     True  -> modifyG (\x -> Q(f $$x))
--     False -> pure U.tt
--   modifyG (\x -> Q(f $$x))
--   modifyG (\x -> Q(f $$x))
--   modifyG (\x -> Q(f $$x))
--   modifyG (\x -> Q(f $$x))
--   b <- get
--   caseG b \case
--     True -> throwError [||False||]
--     _    -> pure U.tt
--   )

-- p2' ::  (ReaderT Int (StateT Bool (Except Bool)) ())
-- p2' = $$(down $ hoist (switchState upBool (pure . downBool)) do
--   n <- get
--   case n of
--     True  -> modify not >> pure U.tt
--     False -> pure U.tt
--   modify not
--   modify not
--   pure U.tt
--   )

-- -- p2' :: (Bool -> Bool) -> State Bool ()
-- -- p2' f = do
-- --   n <- get
-- --   case n of
-- --     True  -> modify not
-- --     False -> pure ()
-- --   modify f
-- --   modify f
-- --   modify f
-- --   modify f


-- -- bleh :: Up (Tree Bool) -> ListE (Up ()) (Up ())
-- -- bleh t = do
-- --   x <- listTree t
-- --   caseG x \case
-- --     True  -> earlyExit U.tt
-- --     False -> pure U.tt

-- -- p1 :: Int -> Int -> [Int]
-- -- p1 x y = $$(down $ push $ (*) <$> range Q(x) Q(y) <*> range Q(x) Q(y))

-- -- p2 :: Int -> Int -> [Int]
-- -- p2 x y = $$(down $ push $ (uncurry (*)) <$> tee (range Q(x) Q(y)))

-- -- p3 :: Int -> Int -> [Int]
-- -- p3 x y = $$(down $ push $ (\a b c -> a * b * c) <$> range Q(x) Q(y) <*> range Q(x) Q(y) <*> range Q(x) Q(y))

-- -- p10 :: Except () [Int]
-- -- p10 = $$(traversePush (\n -> caseG (n U.== 50) \case True -> throwError U.tt; _ -> pure (n * 2))
-- --                       (push $ range Q(10) Q(100)))

-- -- p10' :: Except () [Int]
-- -- p10' = traverse (\n -> if n == 50 then throwError () else pure (n * 2)) [10..100]

--    -- traverse (\n -> if n == 10 then throwError () else pure (n * 2)) (push $ range 10 100)

--    -- for (push $ range 10 100) \n -> do
--    --   when (n == 50) throwError;
--    --   n * 2;

-- -- p2 :: (Int -> Int) -> State Int ()
-- -- p2 f = do
-- --   n <- get
-- --   case n == 10 of
-- --     True -> modify (+10)
-- --     _    -> modify (*10)
-- --   modify (+100)
-- --   modify f
-- --   modify f
-- --   modify f


-- -- f <$> x <*> x

-- -- uncurry f <$> tee x

-- -- p3 :: [Int] -> Int
-- -- p3 xs = $$(

-- -- p3 :: [Int] -> [Int]
-- -- p3 xs = $$(list $
-- --   (<$>) (+10) $
-- --   (<$>) (+10) $
-- --   filter (U.< 10) $
-- --   drop 10 $
-- --   push Q(xs)
-- --   )

-- -- p4 :: [Int] -> State Int ()
-- -- p4 [] = $$(down do
-- --   pure U.tt)
-- -- p4 (n:ns) = $$(down do
-- --   modifyG (+Q(n))
-- --   up Q(p4 ns))
