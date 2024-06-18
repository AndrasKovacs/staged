{-# language ScopedTypeVariables #-}

module CFTT.Tree where

import Data.Typeable
import CFTT.Up
import CFTT.Gen
import CFTT.Split
import CFTT.Pull
import CFTT.SOP

data Tree a = Leaf | Node a (Tree a) (Tree a)

instance Split (Tree a) (Maybe (Up a, Up (Tree a), Up (Tree a))) where
  splitGen x = Gen \k -> [|| case $$x of
    Leaf -> $$(k Nothing)
    Node a l r -> $$(k (Just ([||a||], [||l||], [||r||]))) ||]

instance Typeable a => CasePull (Tree a) (Maybe (Up a, Up (Tree a), Up (Tree a))) where
  casePull x f = case (f Nothing, unravel3 (f . Just)) of
    (Pull @s skip seed step, Pull' @s' skip' seed' step') ->
      Pull @( (Either s (Up a, (Up (Tree a), Up (Tree a)), s'))) (skip || skip')
           (caseM x \case Nothing        -> Left <$> seed
                          Just (a, l, r) -> do s' <- seed' $ Cons a $ Cons l $ Cons r Nil
                                               pure $ Right (a, (l, r), s'))
         \case
         (Left s) -> step s >>= \case
          Stop      -> pure Stop
          Skip s    -> pure $ Skip ( (Left s))
          Yield c s -> pure $ Yield c ( (Left s))
         (Right (a, (l, r), s)) -> step' (Cons a $ Cons l $ Cons r Nil) s >>= \case
          Stop      -> pure Stop
          Skip s    -> pure $ Skip (Right (a, (l, r), s))
          Yield c s -> pure $ Yield c ((Right (a, (l, r), s)))


-- myMap :: forall (a :: Up Type)(b :: Up Type). (Up $$a -> Up $$b) -> Up [$$a] -> Up [$$b]
-- myMap f as = [||
--    let go :: [$$a] -> [$$b]
--        go []     = []
--        go (a:as) = $$(f [||a||]) : go as
--    in go $$as
--   ||]
