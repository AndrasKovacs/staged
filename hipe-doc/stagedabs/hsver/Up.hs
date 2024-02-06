
{-# language
  TemplateHaskell, NoImplicitPrelude, UndecidableInstances,
  MonoLocalBinds, FlexibleInstances, ConstraintKinds #-}
{-# options_ghc -Wno-orphans #-}

{-|
  This module defines `Up` as a synonym of `CodeQ`, the basic type used in typed
  Template Haskell.  It also redefines a fragment of Prelude where everything is
  lifted to operate on @Up a@ instead of @a@.  The Prelude fragment is rather
  ad-hoc, guided by what I needed to grab in examples and benchmarks.
-}

module Up (
  module Up,
  Lift(..)
  )where

import qualified Prelude as P
import qualified Data.Bits as P
import Data.Bits
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

type Up = CodeQ

data Pair a b = Pair !a !b

pair :: Up a -> Up b -> Up (Pair a b)
pair a b = [|| Pair $$a $$b ||]

bindPair :: Up (Pair a b) -> (Up a -> Up b -> Up c) -> Up c
bindPair ab k = [|| case $$ab of Pair a b -> $$(k [||a||] [||b||]) ||]

data Tup3 a b c = Tup3 !a !b !c

bindTup3 :: Up (Tup3 a b c) -> (Up a -> Up b -> Up c -> Up d) -> Up d
bindTup3 abc k = [|| case $$abc of Tup3 a b c -> $$(k [||a||] [||b||] [||c||]) ||]

tup3 :: Up a -> Up b -> Up c -> Up (Tup3 a b c)
tup3 a b c = [|| Tup3 $$a $$b $$c ||]

qt1 :: Up (a -> b) -> Up a -> Up b
qt1 f a = [|| $$f $$a ||]

qt2 :: Up (a -> b -> c) -> Up a -> Up b -> Up c
qt2 f a b = [|| $$f $$a $$b ||]

qt3 :: Up (a -> b -> c -> d) -> Up a -> Up b -> Up c -> Up d
qt3 f a b c = [|| $$f $$a $$b $$c ||]

type Num a = (P.Num a, Lift a)

instance (P.Num a, Lift a) => P.Num (Up a) where
  (+)    = qt2[||(P.+)||]
  (-)    = qt2[||(P.-)||]
  (*)    = qt2[||(P.*)||]
  negate = qt1[||P.negate||]
  abs    = qt1[||P.abs||]
  signum = qt1[||P.signum||]
  fromInteger x = liftTyped (P.fromInteger x)

(+),(-),(*) :: Num a => Up a -> Up a -> Up a
(+) = (P.+)
(-) = (P.-)
(*) = (P.*)
negate,abs,signum :: Num a => Up a -> Up a
negate = P.negate
abs    = P.abs
signum = P.signum

even :: (P.Bits a, P.Num a) => Up a -> Up P.Bool
even n = [|| $$n .&. 1 P.== 0 ||]

odd :: (P.Bits a, P.Num a) => Up a -> Up P.Bool
odd n = [|| $$n .&. 1 P.== 1 ||]

class Integral a where
  quot :: Up a -> Up a -> Up a
  rem :: Up a -> Up a -> Up a
  div :: Up a -> Up a -> Up a
  mod :: Up a -> Up a -> Up a
  quotRem :: Up a -> Up a -> Up (a, a)
  divMod :: Up a -> Up a -> Up (a, a)
  toInteger :: Up a -> Up P.Integer

instance P.Integral a => Integral a where
  quot      = qt2[||P.quot||]
  rem       = qt2[||P.rem||]
  div       = qt2[||P.div||]
  mod       = qt2[||P.mod||]
  quotRem   = qt2[||P.quotRem||]
  divMod    = qt2[||P.divMod||]
  toInteger = qt1[||P.toInteger||]

true :: Up P.Bool
true = [||P.True||]

false :: Up P.Bool
false = [||P.False||]

bool :: Up P.Bool -> Up a -> Up a -> Up a
bool b t f = [|| case $$b of P.True -> $$t; _ -> $$f ||]

infixr 3 &&
(&&) :: Up P.Bool -> Up P.Bool -> Up P.Bool
(&&) x y = bool x y false

infixr 2 ||
(||) x y = bool x true y

not :: Up P.Bool -> Up P.Bool
not x = bool x false true

infix 4 ==
infix 4 /=

class Eq a where
  (==) :: Up a -> Up a -> Up P.Bool
  (==) x y = not (x /= y)

  (/=) :: Up a -> Up a -> Up P.Bool
  (/=) x y = not (x == y)
  {-# minimal (==) | (/=) #-}

instance P.Eq a => Eq a where
  (==) x y = [|| $$x P.== $$y ||]
  (/=) x y = [|| $$x P./= $$y ||]

maybe :: Up b -> (Up a -> Up b) -> Up (P.Maybe a) -> Up b
maybe n j m = [|| case $$m of P.Just a -> $$(j [||a||]); _ -> $$n ||]

left :: Up a -> Up (P.Either a b)
left a = [|| P.Left $$a ||]

right :: Up b -> Up (P.Either a b)
right b = [|| P.Right $$b ||]

either :: (Up a -> Up c) -> (Up b -> Up c) -> Up (P.Either a b) -> Up c
either f g x = [|| case $$x of P.Left a -> $$(f [||a||]); P.Right b -> $$(g [||b||]) ||]

tt :: Up ()
tt = [||()||]

infix 4 <
infix 4 >
infix 4 <=
infix 4 >=

class Eq a => Ord a where
  (<)  :: Up a -> Up a -> Up P.Bool
  (>)  :: Up a -> Up a -> Up P.Bool
  (<=) :: Up a -> Up a -> Up P.Bool
  (>=) :: Up a -> Up a -> Up P.Bool
  max  :: Up a -> Up a -> Up a
  min  :: Up a -> Up a -> Up a

instance P.Ord a => Ord a where
  (<)  x y = [|| (P.<)  $$x $$y ||]
  (>)  x y = [|| (P.>)  $$x $$y ||]
  (<=) x y = [|| (P.<=) $$x $$y ||]
  (>=) x y = [|| (P.>=) $$x $$y ||]
  max  x y = [|| P.max  $$x $$y ||]
  min  x y = [|| P.min  $$x $$y ||]
