
{-# language LambdaCase, TemplateHaskell, BlockArguments, RankNTypes,
   MultiParamTypeClasses, FunctionalDependencies, TypeApplications,
   ScopedTypeVariables, UndecidableInstances, TypeFamilies,
   TypeFamilyDependencies, PartialTypeSignatures, AllowAmbiguousTypes,
   DataKinds, PolyKinds, MagicHash #-}

module LibTest where

import Prelude
import qualified Prelude as P

import Up
import Lib
import Control.Monad.State.Strict

foo :: Int -> ((), Int)
foo = runState $$(down $ do
  -- modifyG (\x -> x + 10)
  -- modifyG (\x -> x + 10)
  modify (\x -> x + x)
  modify (\x -> x + x)
  pure Up.tt)
