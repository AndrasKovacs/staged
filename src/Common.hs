
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


-- Singletons
--------------------------------------------------------------------------------

data family Pi (a :: k)

data Some (k :: Type) :: Type where
  Some :: Pi (a :: k) -> Some k

class SingKind (k :: Type) where
  fromSing :: Pi (a :: k) -> k
  toSing   :: k -> Some k

instance (Eq k, SingKind k) => Eq (Pi (s :: k)) where
  x == y = fromSing x == fromSing y
  {-# inline (==) #-}

instance (Show k, SingKind k) => Show (Pi (s :: k)) where
  showsPrec n x = showsPrec n (fromSing x)
  {-# inline showsPrec #-}

--------------------------------------------------------------------------------

data Stage = S0 | S1 deriving (Eq, Show)

data instance Pi (a :: Stage) where
  SS0 :: Pi 'S0
  SS1 :: Pi 'S1

instance SingKind Stage where
  fromSing SS0 = S0
  fromSing SS1 = S1
  {-# inline fromSing #-}
  toSing   S0  = Some SS0
  toSing   S1  = Some SS1
  {-# inline toSing #-}

--------------------------------------------------------------------------------

type Dbg = () :: Constraint
-- type Dbg = HasCallStack

impossible :: Dbg => a
impossible = error "impossible"
{-# noinline impossible #-}

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

data ArgInfo
  = NoName Icit
  | Named {-# unpack #-} Span
  deriving (Show)

data CV = C | V | CVVar CVMetaVar
  deriving (Eq, Show)

data U :: Stage -> Type where
  U0 :: CV -> U S0
  U1 :: U S1

deriving instance Eq (U s)
deriving instance Show (U s)

newtype Ix = Ix Int
  deriving (Eq, Ord, Show, Num) via Int

newtype Lvl = Lvl Int
  deriving (Eq, Ord, Show, Num, Bits) via Int

newtype MetaVar = MetaVar Int
  deriving (Eq, Ord, Show, Num) via Int

newtype CVMetaVar = CVMetaVar Int
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

instance IsString Name where
  fromString = NName . fromString
