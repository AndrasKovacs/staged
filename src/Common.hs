
module Common (
    module Common
  , FlatParse.Stateful.Span(..)
  , FlatParse.Stateful.Pos(..)
  , module Data.Coerce
  , module Control.Monad
  , module Data.Kind
  ) where

import Data.Kind
import Control.Monad
import Data.Foldable
import GHC.Exts
import qualified Data.ByteString as B

import Data.Bits
import Data.Hashable
import FNV164
import FlatParse.Stateful
import Data.Coerce
import GHC.Stack

import qualified Debug.Trace as Trace

--------------------------------------------------------------------------------

trace :: String -> a -> a
trace = Trace.trace

traceShow :: Show a => a -> b -> b
traceShow = Trace.traceShow

traceM :: Applicative m => String -> m ()
traceM = Trace.traceM
-- traceM _ = pure ()

traceShowM :: (Show a, Applicative f) => a -> f ()
traceShowM a = Trace.traceShowM a
-- traceShowM a = pure ()

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

uf :: a
uf = undefined

-- type Dbg = () :: Constraint
type Dbg = HasCallStack

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

-- States for approximate scope/conversion checking
newtype ConvState = ConvState# Int deriving Eq via Int
pattern CSRigid :: ConvState
pattern CSRigid = ConvState# 0
pattern CSFlex :: ConvState
pattern CSFlex = ConvState# 1
pattern CSFull :: ConvState
pattern CSFull = ConvState# 2
{-# complete CSRigid, CSFlex, CSFull #-}

instance Show ConvState where
  show CSRigid = "Rigid"
  show CSFlex  = "Flex"
  show CSFull  = "Full"

-- Unfolding mode for quoting
newtype Unfolding = Unfolding# Int deriving (Eq, Num) via Int
pattern DoUnfold :: Unfolding
pattern DoUnfold   = Unfolding# 0
pattern DontUnfold :: Unfolding
pattern DontUnfold = Unfolding# 1
{-# complete DoUnfold, DontUnfold #-}

instance Show Unfolding where
  show DoUnfold   = "DoUnfold"
  show DontUnfold = "DontUnfold"

newtype Icit = Icit# Int deriving Eq

pattern Impl :: Icit
pattern Impl = Icit# 0

pattern Expl :: Icit
pattern Expl = Icit# 1
{-# complete Impl, Expl #-}

instance Show Icit where
  show Impl = "Impl"
  show Expl = "Expl"

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

newtype RawName = RawName {unRawName :: B.ByteString}
  deriving (Semigroup, Monoid, Eq) via B.ByteString

instance IsString RawName where
  fromString = RawName . packUTF8

instance Show RawName where
  show = unpackUTF8 . unRawName

instance Hashable RawName where
  hashWithSalt salt (RawName str) = fnv164 str salt
  {-# inline hashWithSalt #-}

data Name
  = NName {-# unpack #-} RawName
  | NEmpty
  | NX
  deriving (Eq)

instance Show Name where
  show (NName x) = show x
  show NEmpty = "_"
  show NX = "x"

instance IsString Name where
  fromString = NName . fromString

-- snoc lists
--------------------------------------------------------------------------------

infixl 4 :>
pattern (:>) :: [a] -> a -> [a]
pattern xs :> x <- x:xs where (:>) xs ~x = x:xs
{-# complete (:>), [] #-}
