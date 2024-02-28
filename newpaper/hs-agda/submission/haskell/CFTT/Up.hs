
{-# options_ghc -Wno-orphans #-}

module CFTT.Up where

import Prelude (Num(..))
import qualified Prelude as P
import Data.Bits

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

type Up = CodeQ

data Pair a b = Pair !a !b
proj1 (Pair a b) = a
proj2 (Pair a b) = b

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

down1 :: (Up a -> Up b) -> Up (a -> b)
down1 f = [|| \a -> $$(f [||a||]) ||]

up1 :: Up (a -> b) -> Up a -> Up b
up1 f a = [|| $$f $$a ||]

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

even :: Up P.Int -> Up P.Bool
even n = [|| $$n .&. 1 P.== 0 ||]

odd :: Up P.Int -> Up P.Bool
odd n = [|| $$n .&. 1 P.== 1 ||]
