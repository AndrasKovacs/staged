
{-# language LambdaCase, TemplateHaskell, BlockArguments, RankNTypes,
   MultiParamTypeClasses, FunctionalDependencies, TypeApplications,
   ScopedTypeVariables, UndecidableInstances, TypeFamilies,
   TypeFamilyDependencies, PartialTypeSignatures, AllowAmbiguousTypes,
   DataKinds, PolyKinds, MagicHash #-}

module Lib where

import Control.Applicative
import Control.Monad
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Data.Coerce
import Data.Kind
import GHC.Exts
import Language.Haskell.TH hiding (ListT, ListE, Type)

import Prelude
import qualified Prelude as P

import Up

--------------------------------------------------------------------------------

class Val a
instance Val Int
instance Val Bool
instance Val Bot
instance Val ()
instance Val a => Val (Identity a)
instance (Val a, Val b) => Val (Pair a b)
instance (Val a, Val b) => Val (a, b)
instance Val a => Val (Maybe a)
instance (Val a, Val b) => Val (Either a b)
instance Val a => Val [a]

data Pair a b = Pair !a !b

proj1 (Pair a b) = a
proj2 (Pair a b) = b

class Improve0 a b | a -> b, b -> a where
  up0 :: Up a -> b
  down0 :: b -> Up a

instance Improve0 (Pair a b) (Pair (Up a) (Up b)) where
  up0 x = Pair [|| proj1 $$x ||] [|| proj2 $$x ||]
  down0  (Pair a b) = [|| Pair $$a $$b ||]

data Bot

instance Improve0 Bool (Gen Bool) where
  up0 x = Gen \k -> [|| if $$x then $$(k True) else $$(k False) ||]
  down0 x = unGen x \x -> if x then [||True||] else [||False||]

instance Improve0 (Maybe a) (Gen (Maybe (Up a))) where
  up0 x = Gen \k -> [|| case $$x of Nothing -> $$(k Nothing); Just a -> $$(k (Just [||a||])) ||]
  down0 x = unGen x \case Nothing -> [||Nothing||]; Just a -> [||Just $$a ||]

instance Improve0 (Either a b) (Gen (Either (Up a) (Up b))) where
  up0 x = Gen \k -> [|| case $$x of Left a -> $$(k (Left [||a||])); Right b -> $$(k (Right [||b||])) ||]
  down0 x = unGen x \case Left a -> [||Left $$a||]; Right b -> [||Right $$b||]

instance Improve0 () (Up ()) where
  up0 x = x
  down0 x = x

class ToList a b | a -> b where
  list :: a -> Up [b]

--------------------------------------------------------------------------------

newtype Gen a = Gen {unGen :: forall r. (a -> Up r) -> Up r}

runGen :: Gen (Up a) -> Up a
runGen (Gen f) = f id

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

gen :: MonadGen m => Up a -> m (Up a)  -- not required to be val!
gen a = liftGen $ Gen \k -> [|| let x = $$a in seq x $$(k [||x||]) ||]

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

sAppend :: Sing xs -> Sing ys -> Sing (Append xs ys)
sAppend SNil         ys = ys
sAppend (SCons x xs) ys = SCons x (sAppend xs ys)

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

genSumUp :: SumUp as -> Gen (SumUp as)
genSumUp (Here x)  = Here <$> gen x
genSumUp (There x) = There <$> genSumUp x

type family Tabulate (as :: [Type]) (b :: Type) where
  Tabulate '[]       b = '[]
  Tabulate (a ': as) b = (a -> b) ': Tabulate as b

tabulate :: forall as r. Sing as -> (SumUp as -> Up r) -> ProdUp (Tabulate as r)
tabulate SNil         f = PNil
tabulate (SCons a as) f = PCons [|| \x -> $$(f (Here [||x||])) ||] (tabulate as (f . There))

index :: forall as r. ProdUp (Tabulate as r) -> SumUp as -> Up r
index PNil         ix        = case ix of
index (PCons f fs) (Here x)  = up1 f x
index (PCons _ fs) (There x) = index fs x

class IsSumUp a where
  type Rep a :: [Type]
  singRep :: Sing (Rep a)
  rep     :: a -> SumUp (Rep a)
  unrep   :: SumUp (Rep a) -> a

instance IsSumUp Bot where
  type Rep Bot = '[]
  singRep = sing
  rep   = \case
  unrep = \case

instance SingI as => IsSumUp (SumUp as) where
  type Rep (SumUp as) = as
  singRep = sing
  rep     = id
  unrep   = id

instance Val a => IsSumUp (Up a) where
  type Rep (Up a) = '[a]
  rep x = Here x
  singRep = sing
  unrep (Here x) = x
  unrep (There x) = case x of

instance IsSumUp () where
  type Rep () = '[ () ]
  singRep = sing
  rep _   = Here Up.tt
  unrep _ = ()

instance (IsSumUp a, IsSumUp b) => IsSumUp (Either a b) where
  type Rep (Either a b) = Append (Rep a) (Rep b)

  singRep = sAppend (singRep @a) (singRep @b)

  rep (Left x)  = injectLeft @(Rep a) @(Rep b) (rep x)
  rep (Right x) = injectRight @(Rep a) @(Rep b) (singRep @a) (rep x)

  unrep x = case uninject @(Rep a) @(Rep b) (singRep @a) x of
    Left x  -> Left (unrep x)
    Right x -> Right (unrep x)

instance IsSumUp a => IsSumUp (Maybe a)  where
  type Rep (Maybe a) = (() ': Rep a)
  singRep = SCons SType (singRep @a)
  rep   Nothing   = Here Up.tt
  rep   (Just x)  = There (rep x)
  unrep (Here _)  = Nothing
  unrep (There x) = Just (unrep x)

type family ProdLeft (x :: Type) (xs :: [Type]) = r | r -> xs where
  ProdLeft x '[]       = '[]
  ProdLeft x (y ': ys) = Pair x y ': ProdLeft x ys

sProdLeft :: forall x ys. Sing ys -> Sing (ProdLeft x ys)
sProdLeft SNil                      = SNil
sProdLeft (SCons y (ys :: Sing zs)) = SCons SType (sProdLeft @x @zs ys)

type family MProd (xs :: [Type]) (ys :: [Type]) where
  MProd '[]       ys = '[]
  MProd (x ': xs) ys = Append (ProdLeft x ys) (MProd xs ys)

sMProd :: Sing xs -> Sing ys -> Sing (MProd xs ys)
sMProd SNil                     ys = SNil
sMProd (SCons (x :: Sing x) xs) ys = sAppend (sProdLeft @x ys) (sMProd xs ys)

prodleft :: forall x ys. Up x -> SumUp ys -> SumUp (ProdLeft x ys)
prodleft x (Here y)  = Here [|| let a = $$x; b = $$y in Pair a b ||]
prodleft x (There y) = There (prodleft x y)

unprodleft :: forall x ys. Sing ys -> SumUp (ProdLeft x ys) -> (Up x, SumUp ys)
unprodleft SNil         a         = case a of
unprodleft (SCons y ys) (Here a)  = ([|| proj1 $$a ||], Here [|| proj2 $$a ||])
unprodleft (SCons y ys) (There a) = case unprodleft ys a of (x, y) -> (x, There y)

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

instance (IsSumUp a, IsSumUp b) => IsSumUp (Pair a b) where
  type Rep (Pair a b) = MProd (Rep a) (Rep b)
  singRep        = sMProd (singRep @a) (singRep @b)
  rep (Pair a b) = mprod @(Rep a) @(Rep b) (singRep @a) (singRep @b) (rep a) (rep b)
  unrep x        = case unmprod @(Rep a) @(Rep b) (singRep @a) (singRep @b) x of
                     (a, b) -> Pair (unrep a) (unrep b)

instance (IsSumUp a, IsSumUp b) => IsSumUp (a, b) where
  type Rep (a, b) = MProd (Rep a) (Rep b)
  singRep         = sMProd (singRep @a) (singRep @b)
  rep (a, b)      = mprod @(Rep a) @(Rep b) (singRep @a) (singRep @b) (rep a) (rep b)
  unrep x         = case unmprod @(Rep a) @(Rep b) (singRep @a) (singRep @b) x of
                      (a, b) -> (unrep a, unrep b)

--------------------------------------------------------------------------------

class MonadJoin m where
  joinG :: IsSumUp a => m a -> m a

instance MonadJoin Gen where
  joinG ma = Gen \(k :: a -> Up r) -> runGen do
    conts <- genProdUp (tabulate (singRep @a) (k . unrep))
    a <- ma
    pure $ index conts (rep a)

instance (IsSumUp e, MonadJoin m) => MonadJoin (ExceptT e m) where
  joinG (ExceptT ma) = ExceptT $ joinG ma

instance (MonadJoin m, IsSumUp s) => MonadJoin (StateT s m) where
  joinG (StateT ma) = StateT \s -> joinG (ma s)

instance MonadJoin m => MonadJoin (MaybeT m) where
  joinG (MaybeT ma) = MaybeT $ joinG ma

instance (MonadJoin m) => MonadJoin (ReaderT r m) where
  joinG (ReaderT ma) = ReaderT \r -> joinG (ma r)

--------------------------------------------------------------------------------

class Split a b | a -> b, b -> a where
  split :: Up a -> Gen b

caseJoin :: (MonadGen m, MonadJoin m, IsSumUp c) => Split a b => Up a -> (b -> m c) -> m c
caseJoin a f = joinG (liftGen (split a) >>= f)

caseGen :: (MonadGen m) => Split a b => Up a -> (b -> m c) -> m c
caseGen a f = (liftGen (split a) >>= f)

instance Split Bool Bool where
  split x = Gen \k -> [|| case $$x of
    True -> $$(k True)
    _    -> $$(k False) ||]

instance Split [a] (Maybe (Up a, Up [a])) where
  split x = Gen \k -> [|| case $$x of
    []   -> $$(k Nothing)
    a:as -> $$(k (Just ([||a||], [||as||]))) ||]

instance Split (a, b) (Up a, Up b) where
  split x = Gen \k -> [|| case $$x of
    (a, b) -> $$(k ([||a||], [||b||])) ||]

instance Split (Pair a b) (Pair (Up a) (Up b)) where
  split x = Gen \k -> [|| case $$x of
    Pair a b -> $$(k (Pair [||a||] [||b||])) ||]

instance Split (Either a b) (Either (Up a) (Up b)) where
  split x = Gen \k -> [|| case $$x of
    Left a  -> $$(k (Left [||a||]))
    Right b -> $$(k (Right [||b||])) ||]

instance Split Bot Bot where
  split x = Gen \_ -> [|| case $$x of ||]

instance Split (Maybe a) (Maybe (Up a)) where
  split x = Gen \k -> [|| case $$x of
    Nothing -> $$(k Nothing)
    Just a  -> $$(k (Just [||a||])) ||]

--------------------------------------------------------------------------------

class MonadGen n => Improve m n | m -> n, n -> m where
  up   :: Up (m a) -> n (Up a)
  down :: n (Up a) -> Up (m a)

instance Improve Identity Gen where
  up x = Gen \k -> k [||runIdentity $$x||]
  down x = unGen x \a -> [||Identity $$a||]

instance (Improve m n) => Improve (StateT s m) (StateT (Up s) n) where

  up x = StateT \s ->
    do as <- up [|| runStateT $$x $$s ||]
       caseGen as pure

  down x = [|| StateT \s -> $$(down
    do (a, s) <- runStateT x [||s||]
       pure [||($$a, $$s)||]
       )||]

instance (Improve m n) => Improve (ExceptT e m) (ExceptT (Up e) n) where
  up x = ExceptT do
    ea <- up [||runExceptT $$x||]
    caseGen ea pure

  down (ExceptT x) =
    [|| ExceptT $$(down (x >>= \case
          Left e  -> pure [||Left $$e||]
          Right a -> pure [||Right $$a||]
                        )) ||]

instance Improve m n => Improve (MaybeT m) (MaybeT n) where
  up x = MaybeT do
    ma <- up [||runMaybeT $$x||]
    caseGen ma pure

  down (MaybeT x) =
    [|| MaybeT $$(down (x >>= \case
          Nothing -> pure [||Nothing||]
          Just a -> pure [||Just $$a||])) ||]

instance Improve m n => Improve (ReaderT r m) (ReaderT (Up r) n) where
  up   x = ReaderT \r -> up [||runReaderT $$x $$r||]
  down x = [|| ReaderT \r -> $$(down (runReaderT x [||r||])) ||]

--------------------------------------------------------------------------------

putG :: forall m s. (MonadGen m, MonadState (Up s) m) => Up s -> m (Up ())
putG s = do
  s <- gen s
  put s
  pure Up.tt

modifyG :: (MonadGen m, MonadState (Up s) m) => (Up s -> Up s) -> m (Up ())
modifyG f = do
  s <- get
  putG (f s)

localG :: (MonadGen m, MonadReader (Up r) m) => (Up r -> Up r) -> m a -> m a
localG f ma = do
  r  <- ask
  r' <- gen (f r)
  local (const r') ma

--------------------------------------------------------------------------------

newtype Push a = Push {unPush :: forall r. (a -> Up r -> Up r) -> Up r -> Up r}
  deriving Functor

instance Semigroup (Push a) where
  Push xs <> Push ys = Push \c n -> xs c (ys c n)

instance Monoid (Push a) where
  mempty = Push \c n -> n

instance Applicative Push where
  pure a = Push \c n -> c a n
  Push fs <*> Push as = Push \c -> fs \f -> as \a -> c (f a)

instance Monad Push where
  return = pure
  Push as >>= f = Push \c -> as \a -> unPush (f a) c

instance MonadGen Push where
  liftGen ga = Push \c n -> unGen ga \a -> c a n

class ToPush a b | a -> b where
  push :: a -> Push b

instance ToPush (Up [a]) (Up a) where
  push as = Push \c n -> [||let go [] = $$n; go (a:as) = $$(c [||a||] [||go as||]) in go $$as||]

instance ToList (Push (Up a)) a where
  list as = unPush as (\a as -> [||$$a : $$as||]) [||[]||]

class Filter f where
  filter :: (a -> Up Bool) -> f a -> f a

instance Filter Push where
  filter f as = do
    a <- as
    caseGen (f a) \case
      True -> pure a
      _    -> mempty

class Drop f where
  drop :: Up Int -> f a -> f a

instance Drop Push where
  drop n (Push as) = Push \c nil -> [||
    $$(as (\a hyp -> [||\i -> case i <=# 0# of
              1# -> $$(c a [|| $$hyp 0# ||])
              _  -> $$hyp (i -# 1#) ||])
          [|| \_ -> $$nil ||]) (case $$n of I# i -> i)  ||]

class Take f where
  take :: Up Int -> f a -> f a

instance Take Push where
  take n (Push as) = Push \c nil -> [||
    $$(as (\a hyp -> [||\i -> case i <=# 0# of
              1# -> $$nil
              _  -> $$(c a [|| $$hyp (i -# 1#) ||]) ||])
          [|| \_ -> $$nil ||]) (case $$n of I# i -> i)
    ||]

-- instance MonadJoin Push where
--   joinG (Push as) = _
