
{-# language TemplateHaskell, UndecidableInstances, MagicHash,
             StandaloneKindSignatures, UnliftedDatatypes, PolyKinds,
             ScopedTypeVariables, TypeApplications, AllowAmbiguousTypes #-}

module UnliftedUp where

import qualified Prelude as P
import Data.Bits
import GHC.Exts hiding (Bool(..), Ordering(..))
import Data.Kind

import Language.Haskell.TH hiding (Type(..))
import Language.Haskell.TH.Syntax hiding (Type(..))

type Up :: TYPE r -> Type
type Up a = CodeQ a

type UType = UnliftedType

data Bool :: UType where
  True :: Bool
  False :: Bool

data Ordering :: UType where
  LT :: Ordering
  EQ :: Ordering
  GT :: Ordering

not :: Bool -> Bool
not True = False
not _    = True

class Num (a :: TYPE r) where
  (+) :: Up a -> Up a -> Up a
  (*) :: Up a -> Up a -> Up a
  infixl 6 +
  infixl 7 *

class Eq (a :: TYPE r) where
  (==) :: Up a -> Up a -> Up Bool
  (/=) :: Up a -> Up a -> Up Bool
  (/=) x y = [|| case $$(x == y) of True -> False; _ -> True ||]
  infix 4 ==
  infix 4 /=

instance Eq Int# where
  (==) x y = [|| case $$x ==# $$y of 1# -> True; _ -> False ||]
  (/=) x y = [|| case $$x ==# $$y of 1# -> False; _ -> True ||]

-- instance Eq Bool where
--   (==) x y = [|| case $$x of
--                  True -> case $$y of
--                    True -> True
--                  _ -> case $$y of
--                    True -> False
--                    _    -> True ||]

--   (/=) x y = [|| case $$x of
--     True -> case $$y of
--       False -> True
--     _ -> case $$y of
--       True ->

class Eq a => Ord a where
  compare :: Up a -> Up a -> Up Ordering
  compare x y = [|| case $$(x < y) of True -> LT; _ -> case $$(x == y) of True -> EQ; _ -> GT ||]

  (<) :: Up a -> Up a -> Up Bool
  (<) x y = [|| case $$(compare x y) of LT -> True; _ -> False ||]

  (>) :: Up a -> Up a -> Up Bool
  (>) x y = [|| case $$(compare x y) of GT -> True; _ -> False ||]

  (<=) :: Up a -> Up a -> Up Bool
  (<=) x y = [|| case $$(compare x y) of GT -> False; _ -> True ||]

  (>=) :: Up a -> Up a -> Up Bool
  (>=) x y = [|| case $$(compare x y) of LT -> False; _ -> True ||]

  infix 4 <
  infix 4 >
  infix 4 <=
  infix 4 >=


-- class Eq a where
--   (==) :: Up a -> Up a -> Up P.Bool
--   (/=) :: Up a -> Up a -> Up P.Bool
--   (/=) x y = [|| P.not $$(x == y) ||]

-- instance P.Eq a  => Eq a where
--   (==) x y = [|| $$x P.== $$y ||]

-- not :: Up P.Bool -> Up P.Bool
-- not x = [|| P.not $$x ||]

-- tt :: Up ()
-- tt = [|| () ||]

-- down1 :: (Up a -> Up b) -> Up (a -> b)
-- down1 f = [|| \a -> $$(f [||a||]) ||]

-- up1 :: Up (a -> b) -> Up a -> Up b
-- up1 f a = [|| $$f $$a ||]

-- class Ord a where
--   (<)  :: Up a -> Up a -> Up P.Bool
--   (<=) :: Up a -> Up a -> Up P.Bool
--   (>)  :: Up a -> Up a -> Up P.Bool
--   (>=) :: Up a -> Up a -> Up P.Bool

-- instance P.Ord a => Ord a where
--   (<)  x y = [|| (P.<)  $$x $$y ||]
--   (<=) x y = [|| (P.<=)  $$x $$y ||]
--   (>)  x y = [|| (P.>)  $$x $$y ||]
--   (>=) x y = [|| (P.>=)  $$x $$y ||]

-- even :: Up P.Int -> Up P.Bool
-- even n = [|| $$n .&. 1 P.== 0 ||]

-- odd :: Up P.Int -> Up P.Bool
-- odd n = [|| $$n .&. 1 P.== 1 ||]
