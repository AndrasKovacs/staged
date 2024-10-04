{-# options_ghc -Wno-unused-imports #-}

module Common (
    module Common
  , SourcePos(..)
  , Pos
  , unPos
  , module Debug.Trace
  , module Data.Coerce
  , module Control.Monad
  , initialPos) where

import Control.Monad
import Data.Coerce
import Data.Kind
import Data.List
import Debug.Trace
import GHC.Stack
import Text.Megaparsec
import Text.Printf

--------------------------------------------------------------------------------
-- We use a more custom debugging function here. We can comment out one
-- definition to toggle debug printing.

-- debug :: (Applicative f) => [String] -> f ()
-- debug strs = traceM (intercalate " | " strs ++ " END")

type Dbg :: Constraint
type Dbg = HasCallStack

debug :: (Applicative f) => [String] -> f ()
debug strs = pure ()

-- type Dbg :: Constraint
-- type Dbg = ()

debug2 :: (Applicative f) => [String] -> f ()
debug2 strs = traceM (intercalate " | " strs ++ " END")

--------------------------------------------------------------------------------

($$!) :: (a -> b) -> a -> b
($$!) f a = f a
{-# inline ($$!) #-}
infixl 8 $$!

(<*!>) :: Monad m => m (a -> b) -> m a -> m b
(<*!>) f a = do
  f <- f
  a <- a
  pure $! f a
{-# inline (<*!>) #-}
infixl 4 <*!>

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

-- | De Bruijn index.
newtype Ix  = Ix {unIx :: Int} deriving (Eq, Show, Num, Enum) via Int

-- | De Bruijn level.
newtype Lvl = Lvl {unLvl :: Int} deriving (Eq, Ord, Show, Num, Enum) via Int

-- | Metavariable.
newtype MetaVar = MetaVar {unMetaVar :: Int} deriving (Eq, Show, Num) via Int

-- | Identifier of a delayed checking problem.
newtype CheckVar = CheckVar {unCheckVar :: Int} deriving (Eq, Show, Num, Ord) via Int

lvl2Ix :: Lvl -> Lvl -> Ix
lvl2Ix (Lvl l) (Lvl x) = Ix (l - x - 1)

newtype NoShow a = NoShow a

instance Show (NoShow a) where
  showsPrec _ x acc = acc

-- Snoc lists
--------------------------------------------------------------------------------

infixl 4 :>

pattern (:>) :: [a] -> a -> [a]
pattern xs :> x <- x:xs where (:>) xs ~x = x:xs
{-# complete (:>), [] #-}
