{-|
Bitset for de Bruijn levels. It's currently just a 64-bit 'Int'. We throw an
error when there are more than 64 local bound variables. If and when the need
for more than 64 variables arises, we can switch to 128 bit or add a new
constructor for large sets.
-}

module LvlSet where

import Data.Coerce
import Data.Bits
import Data.List (foldl')
import Common

newtype LvlSet = LvlSet Int deriving (Eq, Bits) via Int

instance Semigroup LvlSet where
  (<>) = (.|.)
  {-# inline (<>) #-}

instance Monoid LvlSet where
  mempty = LvlSet 0
  {-# inline mempty #-}

insert :: Lvl -> LvlSet -> LvlSet
insert (Lvl x) (LvlSet s)
  | x > 63    = error "LvlSet.insert: element out of range"
  | otherwise = LvlSet (unsafeShiftL 1 x .|. s)
{-# inline insert #-}

single :: Lvl -> LvlSet
single x = insert x mempty
{-# inline single #-}

delete :: Lvl -> LvlSet -> LvlSet
delete (Lvl x) (LvlSet s)
  | x > 63    = error "LvlSet.delete: element out of range"
  | otherwise = LvlSet (complement (unsafeShiftL 1 x) .&. s)
{-# inline delete #-}

member :: Lvl -> LvlSet -> Bool
member (Lvl x) (LvlSet s)
  | x > 63    = error "LvlSet.member: element out of range"
  | otherwise = (unsafeShiftL 1 x .&. s) /= 0
{-# inline member #-}

toList :: LvlSet -> [Lvl]
toList s = filter (`member` s) (coerce [0..63::Int])
{-# inline toList #-}

fromList :: [Lvl] -> LvlSet
fromList = foldl' (flip insert) mempty
{-# inline fromList #-}

instance Show LvlSet where
  show = show . toList
