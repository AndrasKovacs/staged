
{-# language LambdaCase, TemplateHaskell, BlockArguments, RankNTypes,
    MultiParamTypeClasses, FunctionalDependencies, TypeApplications,
    ScopedTypeVariables, UndecidableInstances, QuantifiedConstraints,
    ImpredicativeTypes, TypeFamilies #-}

module Up where

import Prelude (Num(..))
import qualified Prelude as P

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

type Up = CodeQ

instance Num a => Num (Up a) where
  (+) x y       = [|| $$x + $$y ||]
  (*) x y       = [|| $$x * $$y ||]
  abs x         = [|| abs $$x ||]
  signum x      = [|| signum $$x ||]
  fromInteger x = [|| fromInteger $$(liftTyped x) ||]
  negate x      = [|| negate $$x ||]

class Eq a where
  (==) :: Up a -> Up a -> Up P.Bool
  (/=) :: Up a -> Up a -> Up P.Bool
  (/=) x y = [|| P.not $$(x == y) ||]

instance P.Eq a  => Eq a where
  (==) x y = [|| $$x P.== $$y ||]

not :: Up P.Bool -> Up P.Bool
not x = [|| P.not $$x ||]

tt :: Up ()
tt = [|| () ||]

class Ord a where
  (<)  :: Up a -> Up a -> Up P.Bool
  (<=) :: Up a -> Up a -> Up P.Bool
  (>)  :: Up a -> Up a -> Up P.Bool
  (>=) :: Up a -> Up a -> Up P.Bool

instance P.Ord a => Ord a where
  (<)  x y = [|| (P.<)  $$x $$y ||]
  (<=) x y = [|| (P.<=)  $$x $$y ||]
  (>)  x y = [|| (P.>)  $$x $$y ||]
  (>=) x y = [|| (P.>=)  $$x $$y ||]
