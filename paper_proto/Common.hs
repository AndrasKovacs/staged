{-# options_ghc -Wno-unused-imports #-}

module Common (
    module Common
  , SourcePos(..)
  , Pos
  , unPos
  , module Debug.Trace
  , module Data.Coerce
  , module Data.Foldable
  , initialPos) where

import Data.Coerce
import Data.Foldable
import Debug.Trace
import GHC.Exts
import GHC.Stack
import Text.Megaparsec

type Dbg :: Constraint

type Dbg = HasCallStack
-- type Dbg = ()

impossible :: Dbg => a
impossible = error "impossible"

type Name = String

data Icit = Impl | Expl deriving (Eq)

icit :: Icit -> a -> a -> a
icit Impl a _ = a
icit Expl _ b = b

instance Show Icit where
  show Impl = "implicit"
  show Expl = "explicit"

newtype DontShow a = DontShow a

instance Show (DontShow a) where
  showsPrec _ _ str = str

-- | Stages.
data Stage = S0 | S1
  deriving (Eq, Show, Ord, Enum)

-- | De Bruijn index.
newtype Ix  = Ix {unIx :: Int} deriving (Eq, Show, Num) via Int

-- | De Bruijn level.
newtype Lvl = Lvl {unLvl :: Int} deriving (Eq, Ord, Show, Num) via Int

newtype MetaVar = MetaVar {unMetaVar :: Int} deriving (Eq, Show, Num) via Int


-- Snoc lists
--------------------------------------------------------------------------------

infixl 4 :>

pattern (:>) :: [a] -> a -> [a]
pattern xs :> x <- x:xs where (:>) xs ~x = x:xs
{-# complete (:>), [] #-}
