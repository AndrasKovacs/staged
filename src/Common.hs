
module Common (
    module Common
  , FlatParse.Stateful.Span(..)
  , FlatParse.Stateful.Pos(..)
  , module Data.Coerce
  ) where

import GHC.Exts
import qualified Data.ByteString as B

import Data.Bits
import Data.Hashable
import FNV164
import FlatParse.Stateful
import Data.Coerce
-- import GHC.Stack

--------------------------------------------------------------------------------

type Dbg = () :: Constraint
-- type Dbg = HasCallStack

impossible :: Dbg => a
impossible = error "impossible"
{-# noinline impossible #-}

--------------------------------------------------------------------------------

newtype Icit = Icit# Int deriving Eq

pattern Impl :: Icit
pattern Impl = Icit# 0

pattern Expl :: Icit
pattern Expl = Icit# 1
{-# complete Impl, Expl #-}

instance Show Icit where
  show Impl = "Impl"
  show Expl = "Expl"

data ArgInfo
  = NoName Icit
  | Named {-# unpack #-} Span
  deriving Show

data U
  = UVal          -- value types
  | UComp         -- computation types
  | UMeta         -- meta types
  | UVar UMetaVar
  deriving Show

newtype Ix = Ix Int
  deriving (Eq, Ord, Show, Num) via Int

newtype Lvl = Lvl Int
  deriving (Eq, Ord, Show, Num, Bits) via Int

newtype MetaVar = MetaVar Int
  deriving (Eq, Ord, Show, Num) via Int

newtype UMetaVar = UMetaVar Int
  deriving (Eq, Ord, Show, Num) via Int

lvlToIx :: Lvl -> Lvl -> Ix
lvlToIx (Lvl envl) (Lvl l) = Ix (envl - l - 1)
{-# inline lvlToIx #-}

--------------------------------------------------------------------------------

newtype RawName = RawName {unRawName :: B.ByteString}
  deriving (Show, IsString, Eq) via B.ByteString

instance Hashable RawName where
  hashWithSalt salt (RawName str) = fnv164 str salt
  {-# inline hashWithSalt #-}

data Name
  = NName {-# unpack #-} RawName
  | NEmpty
  deriving (Eq, Show)
