
{-# language LambdaCase, TemplateHaskell, BlockArguments, RankNTypes,
    MultiParamTypeClasses, FunctionalDependencies, TypeApplications, TypeFamilyDependencies,
    ScopedTypeVariables, UndecidableInstances, QuantifiedConstraints, DataKinds,
    TypeFamilies, CPP, PartialTypeSignatures, MagicHash, AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures -Wincomplete-patterns #-}

module GenNoCurry2 where

import Prelude hiding (filter, zip, zipWith, take, drop)
import qualified Prelude as P
import Control.Monad
import Language.Haskell.TH hiding (ListT, ListE, Type)
import Control.Monad.State.Strict
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Cont
import Data.Coerce
import Control.Applicative
import Data.Kind
import Up
import GHC.Exts

#include "Sugar.h"

--------------------------------------------------------------------------------

data Bot
data Pair a b = Pair !a !b

class Val a
instance Val Int
instance Val Bool
instance Val Bot
instance Val ()
instance Val a => Val (Identity a)
instance (Val a, Val b) => Val (Pair a b)
instance Val a => Val (Maybe a)
instance (Val a, Val b) => Val (Either a b)
instance Val a => Val [a]

printUp :: Up a -> IO ()
printUp a = do
  x <- unType <$> runQ (examineCode a)
  print $ ppr x

upFun :: Up (a -> b) -> Up a -> Up b
upFun f a = [|| $$f $$a ||]

downFun :: (Up a -> Up b) -> Up (a -> b)
downFun f = [|| \a -> $$(f [||a||]) ||]

upBool :: Up Bool -> Gen Bool
upBool b = Gen \k -> [|| case $$b of True -> $$(k True); False -> $$(k False) ||]

downBool :: Bool -> Up Bool
downBool True = [||True||]
downBool False = [||False||]

upPair :: Up (Pair a b) -> Gen (Pair (Up a) (Up b))
upPair ab = Gen \k -> [|| case $$ab of Pair a b -> $$(k (Pair [||a||] [||b||])) ||]

downPair :: Pair (Up a) (Up b) -> Up (Pair a b)
downPair (Pair a b) = [|| Pair $$a $$b ||]

proj1 (Pair a _) = a
proj2 (Pair _ b) = b

--------------------------------------------------------------------------------

newtype Gen a = Gen {unGen :: forall r. Val r => (a -> Up r) -> Up r}

runGen :: Val a => Gen (Up a) -> Up a
runGen (Gen f) = f id

runGenFun :: forall a b. Val a => Val b => Gen (Up a -> Up b) -> Up (a -> b)
runGenFun (Gen f) = downFun (\a -> f (\f -> f a))

instance Functor Gen where
  fmap f ma = Gen \k -> unGen ma \a -> k (f a)

instance Applicative Gen where
  pure a = Gen \k -> k a
  (<*>) gf ga = Gen \k -> unGen gf \f -> unGen ga \a -> k (f a)

instance Monad Gen where
  return = pure
  (>>=) ga f = Gen \k -> unGen ga \a -> unGen (f a) k

instance MonadFail Gen where
  fail = error

class Monad m => MonadGen m where
  liftGen :: Gen a -> m a

instance MonadGen Gen where
  liftGen = id

instance MonadGen m => MonadGen (StateT s m) where
  liftGen = lift . liftGen

instance MonadGen m => MonadGen (ReaderT r m) where
  liftGen = lift . liftGen

instance MonadGen m => MonadGen (ExceptT e m) where
  liftGen = lift . liftGen

instance MonadGen m => MonadGen (MaybeT m) where
  liftGen = lift . liftGen

gen :: MonadGen m => Up a -> m (Up a)
gen a = liftGen $ Gen \k -> [|| let x = $$a in $$(k [||x||]) ||]

genFun :: Val a => Val b => MonadGen m => (Up a -> Up b) -> m (Up (a -> b))
genFun f = gen (downFun f)

-- Generic sums
--------------------------------------------------------------------------------

data family Sing (a :: k)

data instance Sing (a :: Type) where
  SType :: Sing a

data instance Sing (a :: [k]) where
  SNil  :: Sing '[]
  SCons :: Sing x -> Sing xs -> Sing (x ': xs)

class                           SingI a           where sing :: Sing a
instance                        SingI (a :: Type) where sing = SType
instance                        SingI '[]         where sing = SNil
instance (SingI a, SingI as) => SingI (a ': as)   where sing = SCons sing sing

type family Append (xs :: [k]) (ys :: [k]) where
  Append '[]       ys = ys
  Append (x ': xs) ys = x ': Append xs ys

injectLeft :: forall xs ys. SumUp xs -> SumUp (Append xs ys)
injectLeft (Here x)  = Here x
injectLeft (There x) = There (injectLeft @_ @ys x)

injectRight :: forall xs ys. Sing xs -> SumUp ys -> SumUp (Append xs ys)
injectRight SNil         y = y
injectRight (SCons x xs) y = There (injectRight xs y)

uninject :: forall xs ys. Sing xs -> SumUp (Append xs ys) -> Either (SumUp xs) (SumUp ys)
uninject SNil         y         = Right y
uninject (SCons x xs) (Here y)  = Left (Here y)
uninject (SCons x xs) (There y) = case uninject @_ @ys xs y of
  Left y  -> Left (There y)
  Right y -> Right y

data SumUp (as :: [Type]) where
  Here  :: Up a -> SumUp (a ': as)
  There :: SumUp as -> SumUp (a ': as)

data ProdUp (as :: [Type]) where
  PNil  :: ProdUp '[]
  PCons :: Up a -> ProdUp as -> ProdUp (a ': as)

genProdUp :: ProdUp as -> Gen (ProdUp as)
genProdUp PNil         = pure PNil
genProdUp (PCons x xs) = PCons <$> gen x <*> genProdUp xs

type family Tabulate (as :: [Type]) (b :: Type) where
  Tabulate '[]       b = '[]
  Tabulate (a ': as) b = (a -> b) ': Tabulate as b

tabulate :: forall as r. Sing as -> (SumUp as -> Up r) -> ProdUp (Tabulate as r)
tabulate SNil         f = PNil
tabulate (SCons a as) f = PCons [|| \x -> $$(f (Here [||x||])) ||] (tabulate as (f . There))

index :: forall as r. ProdUp (Tabulate as r) -> SumUp as -> Up r
index PNil         ix        = case ix of
index (PCons f fs) (Here x)  = upFun f x
index (PCons _ fs) (There x) = index fs x

class SingI bs => IsSumUp a bs | a -> bs where
  rep     :: a -> SumUp bs
  unrep   :: SumUp bs -> a

instance IsSumUp Bot '[] where
  rep   = \case
  unrep = \case

instance IsSumUp () '[ () ] where
  rep _   = Here Up.tt
  unrep _ = ()

instance (IsSumUp a ar, IsSumUp b br, SingI cr, cr ~ Append ar br) => IsSumUp (Either a b) cr where

  rep (Left x)  = injectLeft @ar @br (rep x)
  rep (Right x) = injectRight @ar @br sing (rep x)

  unrep x = case uninject @ar @br sing x of
    Left x  -> Left (unrep x)
    Right x -> Right (unrep x)

instance (IsSumUp a ar) => IsSumUp (Maybe a) (() ': ar) where
  rep   Nothing   = Here Up.tt
  rep   (Just x)  = There (rep x)
  unrep (Here _)  = Nothing
  unrep (There x) = Just (unrep x)

type family ProdLeft (x :: Type) (xs :: [Type]) = r | r -> xs where
  ProdLeft x '[]       = '[]
  ProdLeft x (y ': ys) = Pair x y ': ProdLeft x ys

type family MProd (xs :: [Type]) (ys :: [Type]) where
  MProd '[]       ys = '[]
  MProd (x ': xs) ys = Append (ProdLeft x ys) (MProd xs ys)

prodleft :: forall x ys. Up x -> SumUp ys -> SumUp (ProdLeft x ys)
prodleft x (Here y)  = Here [|| Pair $$x $$y ||]
prodleft x (There y) = There (prodleft x y)

unprodleft :: forall x ys. Sing ys -> SumUp (ProdLeft x ys) -> (Up x, SumUp ys)
unprodleft SNil         a         = case a of
unprodleft (SCons y ys) (Here a)  = ([|| proj1 $$a ||], Here [|| proj2 $$a ||])
unprodleft (SCons y ys) (There a) = case unprodleft ys a of (x, y) -> (x, There y)

sProdLeft :: forall x ys. Sing ys -> Sing (ProdLeft x ys)
sProdLeft SNil                      = SNil
sProdLeft (SCons y (ys :: Sing zs)) = SCons SType (sProdLeft @x @zs ys)

mprod :: forall xs ys. Sing xs -> Sing ys -> SumUp xs -> SumUp ys -> SumUp (MProd xs ys)
mprod xs ys a b = case xs of
  SNil -> case a of
  SCons (x :: Sing x) (xs :: Sing zs) -> case a of
    Here a  -> injectLeft @(ProdLeft x ys) @(MProd zs ys) (prodleft a b)
    There a -> injectRight @(ProdLeft x ys) @(MProd zs ys) (sProdLeft @x ys) (mprod xs ys a b)

unmprod :: forall xs ys. Sing xs -> Sing ys -> SumUp (MProd xs ys) -> (SumUp xs, SumUp ys)
unmprod xs ys a = case xs of
  SNil -> case a of
  SCons (x :: Sing x) (xs :: Sing xs') ->
    case uninject @(ProdLeft x ys) @(MProd xs' ys) (sProdLeft @x ys) a of
      Left a  -> case unprodleft @x ys a of (a, b) -> (Here a, b)
      Right a -> case unmprod @xs' @ys xs ys a of (a, b) -> (There a, b)

instance (IsSumUp a ar, IsSumUp b br, SingI cr, cr ~ MProd ar br) => IsSumUp (Pair a b) cr where
  rep (Pair a b) = mprod @ar @br sing sing (rep a) (rep b)
  unrep x        = case unmprod @ar @br sing sing x of
                     (a, b) -> Pair (unrep a) (unrep b)

--------------------------------------------------------------------------------

class MonadJoin m where
  joinG :: IsSumUp a ar => m a -> m a

instance MonadJoin Gen where
  joinG ma = Gen \k -> runGen do
    conts <- genProdUp (tabulate sing (k . unrep))
    a <- ma
    pure $ index conts (rep a)

-- instance (Joinable e, MonadJoin m) => MonadJoin (ExceptT e m) where
--   joinG (ExceptT ma) = ExceptT $ joinG ma

-- instance (MonadJoin m, Joinable s) => MonadJoin (StateT s m) where
--   joinG (StateT ma) = StateT \s -> joinG (ma s)

-- instance MonadJoin m => MonadJoin (MaybeT m) where
--   joinG (MaybeT ma) = MaybeT $ joinG ma

-- instance (MonadJoin m) => MonadJoin (ReaderT r m) where
--   joinG (ReaderT ma) = ReaderT \r -> joinG (ma r)
