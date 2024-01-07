
{-# language LambdaCase, TemplateHaskell, BlockArguments, RankNTypes,
    MultiParamTypeClasses, FunctionalDependencies, TypeApplications,
    ScopedTypeVariables, UndecidableInstances, QuantifiedConstraints,
    ImpredicativeTypes, MagicHash #-}

module Tests where

import Gen
import Language.Haskell.TH
import Control.Monad.State.Strict
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Trans
import GHC.Types
import GHC.Exts

printUp :: Up a -> IO ()
printUp a = do
  x <- unType <$> runQ (examineCode a)
  print $ ppr x

-- addI :: Int -> Int -> Int
-- addI (I# x) (I# y) = I# (x +# y)

-- p1 :: State Int Int
-- p1 = $$(downM @(StateT _ Gen) do
--   modifyG (\x -> [||addI $$x $$x||])
--   modifyG (\x -> [||addI $$x $$x||])
--   modifyG (\x -> [||addI $$x $$x||])
--   modifyG (\x -> [||addI $$x $$x||])
--   get)

p2 ::  (State Bool ())
p2 = $$(downM @(StateT (Up Bool) Gen) do
  b <- get
  caseUp b \case
    True  -> modifyG (\x -> [|| not $$x ||])
    False -> pure [||()||]
  (modifyG (\x -> [|| not $$x ||]))
  (modifyG (\x -> [|| not $$x ||]))
  (modifyG (\x -> [|| not $$x ||]))
  (modifyG (\x -> [|| not $$x ||]))
  (modifyG (\x -> [|| not $$x ||]))
  )

-- p3 ::  (State Bool ())
-- p3 = $$(downM @(StateT (Up Bool) Gen) $ upstate @Bool @(Gen Bool) do
--   b <- liftGen =<< get
--   if b then modify (fmap not)
--        else pure ()
--   modify (fmap not)
--   modify (fmap not)
--   modify (fmap not)
--   modify (fmap not)
--   modify (fmap not)
--   pure [||()||]

p4 :: [Int] -> State Int ()
p4 [] = $$(downM @(StateT (Up Int) Gen) do
  pure [||()||])
p4 (n:ns) = $$(downM @(StateT (Up Int) Gen) do
  modifyG \x -> [|| $$x + n ||]
  upM @(State Int) [||p4 ns||]
  )

p4' :: [Int] -> State Int ()
p4' [] =
  pure ()
p4' (n:ns) = do
  modify (+n)
  p4 ns
