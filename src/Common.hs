
module Common (
    module Common
  , B.ByteString
  , FlatParse.Stateful.Pos(..)
  , FlatParse.Stateful.Span(..)
  , unpackUTF8
  , packUTF8
  , coerce
  , module Control.Monad
  , module Data.Kind
  , module Lens.Micro
  ) where

import qualified Data.ByteString as B
import Control.Monad
import Data.Bits
import Data.Coerce
import Data.Foldable
import Data.Kind
import Data.List
import FlatParse.Stateful
import GHC.Exts
import Lens.Micro

#ifdef DEBUG
import GHC.Stack
#endif

-- Debug printing, toggled by "debug" cabal flag
--------------------------------------------------------------------------------

-- define DEBUG

#ifdef DEBUG
type Dbg = HasCallStack

debug :: [String] -> IO ()
debug strs = U.io $ putStrLn (intercalate " | " strs ++ " END")

debugging :: IO () -> IO ()
debugging act = act
{-# inline debugging #-}
#else
type Dbg = () :: Constraint

debug :: [String] -> IO ()
debug strs = pure ()
{-# inline debug #-}

debugging :: IO () -> IO ()
debugging _ = pure ()
{-# inline debugging #-}
#endif

debug' :: [String] -> IO ()
debug' strs = putStrLn (intercalate " | " strs ++ " END")

debugging' :: IO () -> IO ()
debugging' act = act
{-# inline debugging' #-}

--------------------------------------------------------------------------------

data Fields a
  = FNil
  | FCons Name a (Fields a)

instance Functor Fields where
  fmap f = go where
    go FNil = FNil
    go (FCons x a as) = FCons x (f a) (go as)
  {-# inline fmap #-}

instance Foldable Fields where
  foldr f z = go where
    go FNil           = z
    go (FCons x a fs) = f a $! go fs
  {-# inline foldr #-}
  foldl' f z = go z where
    go acc FNil = acc
    go acc (FCons x a fs) = go (f acc a) fs
  {-# inline foldl' #-}

instance Traversable Fields where
  traverse f = go where
    go FNil           = pure FNil
    go (FCons x a fs) = FCons x <$> f a <*> go fs
  {-# inline traverse #-}
  mapM f = go where
    go FNil           = pure FNil
    go (FCons x a fs) = FCons x <$!> f a <*!> go fs
  {-# inline mapM #-}

instance Show a => Show (Fields a) where
  show = show . go where
    go :: Fields a -> [(Name, a)]
    go FNil = []
    go (FCons x a as) = (x, a) : go as

data Cases a
  = CNil
  | CCons Lvl [Name] a (Cases a)

instance Functor Cases where
  fmap f = go where
    go CNil = CNil
    go (CCons x xs rhs cs) = CCons x xs (f rhs) (go cs)
  {-# inline fmap #-}

instance Foldable Cases where
  foldr f z = go where
    go CNil = z
    go (CCons x xs a cs) = f a $! go cs
  {-# inline foldr #-}
  foldl' f z = go z where
    go acc CNil = acc
    go acc (CCons x xs a cs) = go (f acc a) cs
  {-# inline foldl' #-}

instance Traversable Cases where
  traverse f = go where
    go CNil = pure CNil
    go (CCons x xs a cs) = CCons x xs <$> f a <*> go cs
  {-# inline traverse #-}
  mapM f = go where
    go CNil = pure CNil
    go (CCons x xs a cs) = CCons x xs <$!> f a <*!> go cs
  {-# inline mapM #-}

instance Show a => Show (Cases a) where
  show = show . go where
    go CNil = []
    go (CCons x xs rhs cs) = (x, xs, rhs) : go cs

--------------------------------------------------------------------------------

uf :: Dbg => a
uf = undefined

impossible :: Dbg => a
impossible = error "impossible"
{-# inline impossible #-}

infixl 9 $$!
($$!) :: (a -> b) -> a -> b
($$!) f a = f $! a
{-# inline ($$!) #-}

infixl 4 <*!>
(<*!>) :: Monad m => m (a -> b) -> m a -> m b
(<*!>) mf ma = do
  f <- mf
  a <- ma
  pure $! f a
{-# inline (<*!>) #-}

--------------------------------------------------------------------------------

data ConvState = CSRigid | CSFlex | CSFull
  deriving (Eq, Show)

data QuoteOption
  = UnfoldAll     -- ^ Unfold top defs and metas.
  | UnfoldMetas   -- ^ Unfold metas only.
  | UnfoldNone    -- ^ No unfolding.
  | LiftVars Lvl  -- ^ Don't unfold anything, but raise vars to level 1.
                  --   Used for creating types for fresh metas (where we have to
                  --   move a type from an arbitrary context to a fully Lvl1 context).
  deriving Show

data Icit = Impl | Expl
  deriving (Eq, Show)

icit :: Icit -> a -> a -> a
icit Impl x y = x
icit Expl x y = y
{-# inline icit #-}

newtype Ix = Ix Int
  deriving (Eq, Ord, Show, Num) via Int

newtype Lvl = Lvl Int
  deriving (Eq, Ord, Show, Num, Bits) via Int

newtype MetaVar = MetaVar Int
  deriving (Eq, Ord, Show, Num) via Int

lvlToIx :: Lvl -> Lvl -> Ix
lvlToIx (Lvl envl) (Lvl l) = Ix (envl - l - 1)
{-# inline lvlToIx #-}

-- names
--------------------------------------------------------------------------------

type RawName = B.ByteString

data Name
  = NName {-# unpack #-} RawName
  | NEmpty
  | NX
  deriving (Eq)

instance Show Name where
  show (NName x) = show x
  show NEmpty    = "_"
  show NX        = "x"

instance IsString Name where
  fromString = NName . fromString

-- snoc lists
--------------------------------------------------------------------------------

infixl 4 :>
pattern (:>) :: [a] -> a -> [a]
pattern xs :> x <- x:xs where (:>) xs ~x = x:xs
{-# complete (:>), [] #-}
