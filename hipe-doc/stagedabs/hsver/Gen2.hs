{-# LANGUAGE DataKinds #-}

{-# language LambdaCase, TemplateHaskell, BlockArguments, RankNTypes,
    MultiParamTypeClasses, FunctionalDependencies, TypeApplications,
    ScopedTypeVariables, UndecidableInstances, QuantifiedConstraints,
    TypeFamilies, CPP, PartialTypeSignatures, MagicHash, AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Gen2 where

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
import Data.Coerce
import Control.Applicative
import Data.Kind
import Up

import GHC.Exts

#include "Sugar.h"

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

--------------------------------------------------------------------------------

-- a finite product of Up-s
data ProdUp a where
  HUp   :: ProdUp (Up a)
  HPair :: ProdUp a -> ProdUp b -> ProdUp (a, b)

-- a finite product/sum of Up-s
data ProdSumUp a where
  JUp     :: ProdSumUp (Up a)
  JEither :: ProdSumUp a -> ProdSumUp b -> ProdSumUp (Either a b)
  JMaybe  :: ProdSumUp a -> ProdSumUp (Maybe a)
  JPair   :: ProdSumUp a -> ProdSumUp b -> ProdSumUp (a, b)
  JBool   :: ProdSumUp Bool

class IsProdSumUp a where
  prodSumUp :: ProdSumUp a
instance IsProdSumUp (Up a) where
  prodSumUp = JUp
instance (IsProdSumUp a, IsProdSumUp b) => IsProdSumUp (Either a b) where
  prodSumUp = JEither prodSumUp prodSumUp
instance (IsProdSumUp a, IsProdSumUp b) => IsProdSumUp (a, b) where
  prodSumUp = JPair prodSumUp prodSumUp
instance IsProdSumUp a => IsProdSumUp (Maybe a) where
  prodSumUp = JMaybe prodSumUp
instance IsProdSumUp Bool where
  prodSumUp = JBool

type family Arr a r where
  Arr (Up a)       (Up r) = Up (a -> r)
  Arr (a, b)       (Up r) = Arr a (Arr b (Up r))
  Arr (Either a b) (Up r) = (Arr a (Up r), Arr b (Up r))
  Arr (Maybe a)    (Up r) = (Arr (Up ()) (Up r), Arr a (Up r))
  Arr Bool         (Up r) = (Arr (Up ()) (Up r), Arr (Up ()) (Up r))
  Arr a            (b, c) = (Arr a b, Arr a c)

arrIsProdUp :: forall a r. ProdSumUp a -> ProdUp r -> ProdUp (Arr a r)
arrIsProdUp JUp           r@HUp = HUp
arrIsProdUp (JPair a b)   r@HUp = arrIsProdUp a (arrIsProdUp b r)
arrIsProdUp (JEither a b) r@HUp = HPair (arrIsProdUp a r) (arrIsProdUp b r)
arrIsProdUp (JMaybe a)    r@HUp = HPair (arrIsProdUp JUp r) (arrIsProdUp a r)
arrIsProdUp JBool         r@HUp = HPair (arrIsProdUp JUp r) (arrIsProdUp JUp r)
arrIsProdUp a     r@(HPair b c) = HPair (arrIsProdUp a b) (arrIsProdUp a c)

weaken :: ProdUp a -> ProdSumUp a
weaken HUp         = JUp
weaken (HPair a b) = JPair (weaken a) (weaken b)

handle :: forall a r. ProdSumUp a -> ProdUp r -> (a -> r) -> Arr a r
handle JUp           r@HUp f = [|| \a -> $$(f [||a||]) ||]
handle (JPair a b)   r@HUp f = handle a (arrIsProdUp b r) \x -> handle b r \y -> f (x, y)
handle (JEither a b) r@HUp f = (handle a r (f . Left), handle b r (f . Right))
handle (JMaybe a)    r@HUp f = (handle JUp r (const (f Nothing)), handle a r (f . Just))
handle JBool         r@HUp f = (handle JUp r (const (f True)), handle JUp r (const (f False)))
handle a     r@(HPair b c) f = (handle a b (fst . f), handle a c (snd . f))

unhandle :: forall a r. ProdSumUp a -> ProdUp r -> Arr a r -> (a -> r)
unhandle JUp           r@HUp f = \x -> [|| $$f $$x ||]
unhandle (JPair a b)   r@HUp f = \(x, y) -> unhandle b r (unhandle a (arrIsProdUp b r) f x) y
unhandle (JEither a b) r@HUp f = either (unhandle a r (fst f)) (unhandle b r (snd f))
unhandle (JMaybe a)    r@HUp f = maybe (unhandle JUp r (fst f) tt) (unhandle a r (snd f))
unhandle JBool         r@HUp f = \case True -> unhandle JUp r (fst f) tt; False -> unhandle JUp r (snd f) tt
unhandle a     r@(HPair b c) f = \x -> (unhandle a b (fst f) x, unhandle a c (snd f) x)

letG :: forall a m. (IsProdSumUp a, MonadGen m) => a -> m a
letG a = liftGen (letG' prodSumUp a) where

letG' :: forall a. ProdSumUp a -> a -> Gen a
letG' JUp           x = Gen \k -> [|| let y = $$x in seq y $$(k [||y||]) ||]
letG' (JEither a b) x = either ((Left <$>). letG' a) ((Right <$>). letG' b) x
letG' (JMaybe a)    x = maybe (pure Nothing) ((Just <$>) . letG' a) x -- we don't join up the error branch!
letG' (JPair a b)   x = (,) <$> letG' a (fst x) <*> letG' b (snd x)
letG' JBool         x = pure x

class MonadJoin m where
  joinG :: IsProdSumUp a => m a -> m a

instance MonadJoin Gen where
  joinG ma = Gen \(k :: a -> Up r) ->
    let pa = prodSumUp :: ProdSumUp a
        pr = HUp :: ProdUp (Up r)
        pk = arrIsProdUp pa pr
    in unGen (letG' (weaken pk) (handle pa pr k)) \k -> unGen ma (unhandle pa pr k)

instance (IsProdSumUp e, MonadJoin m) => MonadJoin (ExceptT e m) where
  joinG (ExceptT ma) = ExceptT $ joinG ma

instance (MonadJoin m, IsProdSumUp s) => MonadJoin (StateT s m) where
  joinG (StateT ma) = StateT \s -> joinG (ma s)

instance MonadJoin m => MonadJoin (MaybeT m) where
  joinG (MaybeT ma) = MaybeT $ joinG ma

instance (MonadJoin m) => MonadJoin (ReaderT r m) where
  joinG (ReaderT ma) = ReaderT \r -> joinG (ma r)


--------------------------------------------------------------------------------

class Split a b | a -> b, b -> a where
  split :: Up a -> Gen b

caseG :: (MonadGen m, MonadJoin m, IsProdSumUp c) => Split a b => Up a -> (b -> m c) -> m c
caseG a f = joinG (liftGen (split a) >>= f)

caseG' :: (MonadGen m) => Split a b => Up a -> (b -> m c) -> m c
caseG' a f = (liftGen (split a) >>= f)

instance Split Bool Bool where
  split x = Gen \k -> [|| case $$x of
    True -> $$(k True)
    _    -> $$(k False) ||]

instance Split [a] (Maybe (Up a, Up [a])) where
  split x = Gen \k -> [|| case $$x of
    []   -> $$(k Nothing)
    a:as -> $$(k (Just (Q(a), Q(as)))) ||]

instance Split (a, b) (Up a, Up b) where
  split x = Gen \k -> [|| case $$x of
    (a, b) -> $$(k (Q(a), Q(b))) ||]

instance Split (Either a b) (Either (Up a) (Up b)) where
  split x = Gen \k -> [|| case $$x of
    Left a  -> $$(k (Left Q(a)))
    Right b -> $$(k (Right Q(b))) ||]

--------------------------------------------------------------------------------

class MonadGen n => Improve m n | m -> n, n -> m where
  up   :: Up (m a) -> n (Up a)
  down :: n (Up a) -> Up (m a)

instance Improve Identity Gen where
  up x = Gen \k -> k Q(runIdentity $$x)
  down x = unGen x \a -> Q(Identity $$a)

instance Improve m n => Improve (StateT s m) (StateT (Up s) n) where

  up x = StateT \s ->
    do as <- up [|| runStateT $$x $$s ||]
       caseG' as pure

  down x = [|| StateT \s -> $$(down
    do (a, s) <- runStateT x Q(s)
       pure Q(($$a, $$s))
       )||]

instance Improve m n => Improve (ExceptT e m) (ExceptT (Up e) n) where
  up x = ExceptT do
    ea <- up Q(runExceptT $$x)
    caseG' ea pure

  down (ExceptT x) =
    [|| ExceptT $$(down (x >>= \case
          Left e  -> pure Q(Left $$e)
          Right a -> pure Q(Right $$a)
                        )) ||]

instance Improve m n => Improve (ReaderT r m) (ReaderT (Up r) n) where
  up   x = ReaderT \r -> up Q(runReaderT $$x $$r)
  down x = [|| ReaderT \r -> $$(down (runReaderT x Q(r))) ||]

--------------------------------------------------------------------------------

putG :: (MonadGen m, IsProdSumUp s, MonadState s m) => s -> m (Up ())
putG s = do
  s <- letG s
  put s
  pure Q(())

modifyG :: (MonadGen m, IsProdSumUp s, MonadState s m) => (s -> s) -> m (Up ())
modifyG f = do
  s <- get
  putG (f s)

localG :: (MonadGen m, IsProdSumUp r, MonadReader r m) => (r -> r) -> m a -> m a
localG f ma = do
  r  <- ask
  r' <- letG (f r)
  local (const r') ma

-- Push
--------------------------------------------------------------------------------

newtype Push a = Push {unPush :: forall r. (a -> Up r -> Up r) -> Up r -> Up r}
  deriving Functor

instance Semigroup (Push  a) where
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
  push as = Push \c n -> Q(let go [] = $$n; go (a:as) = $$(c Q(a) Q(go as)) in go $$as)

class ToList a b | a -> b where
  list :: a -> Up [b]

instance ToList (Push (Up a)) a where
  list as = unPush as (\a as -> Q($$a : $$as)) Q([])

class Filter f where
  filter :: (a -> Up Bool) -> f a -> f a

instance Filter Push where
  filter f as = do
    a <- as
    caseG' (f a) \case
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

instance Improve [] Push where
  up   = push
  down = list

traversePush :: Improve m n => (Up a -> n (Up b)) -> Push (Up a) -> Up (m [b])
traversePush f as =
  unPush as (\a bs -> down do {bs <- up bs; b <- f a; pure [|| $$b : $$bs ||]}) (down $ pure [||[]||])

--------------------------------------------------------------------------------

data Pull a = forall s. Pull (Up s) (Up s -> MaybeT Gen (Up s, a))

data Pair a b = Pair !a !b

instance Split (Pair a b) (Pair (Up a) (Up b)) where
  split ab = Gen \k -> [|| case $$ab of Pair a b -> $$(k (Pair Q(a) Q(b))) ||]

instance Functor Pull where
  fmap f (Pull s step) = Pull s (fmap (fmap (fmap f)) step)

instance Applicative Pull where
  pure a = Pull Up.tt \s -> pure (s, a)
  Pull s step <*> Pull s' step' =
    Pull [||Pair $$s $$s'||] \ss' ->
      caseG' ss' \(Pair s s') -> do
        (s, f)  <- step s
        (s', a) <- step' s'
        pure ([|| Pair $$s $$s' ||], f a)

range :: Up Int -> Up Int -> Pull (Up Int)
range x y = Pull [|| Pair $$x $$y ||] \xy ->
  caseG xy \(Pair x y) ->
  caseG (x Up.>= y) \case
    True -> empty
    _    -> pure ([|| Pair ($$x + 1) $$y ||], x)

instance ToPush (Pull a) a where
  push (Pull s step) = Push \c n ->
    [|| let go s = seq s $$(unGen (runMaybeT (step [||s||])) \case
                           Nothing     -> n
                           Just (s, a) -> c a [||go $$s||]) in
        go $$s ||]

tee :: Functor f => f a -> f (a, a)
tee = fmap (\a -> (a, a))

--------------------------------------------------------------------------------

switchState :: Monad m => (s -> m s') -> (s' -> m s) -> StateT s' m a -> StateT s m a
switchState f g (StateT ma) = StateT \s -> do
  s <- f s
  (a, s) <- ma s
  s <- g s
  pure (a, s)

upBool :: MonadGen m => Up Bool -> m Bool
upBool b = liftGen $ Gen \k -> [|| if $$b then $$(k True) else $$(k False) ||]

downBool :: Bool -> Up Bool
downBool True  = [||True||]
downBool False = [||False||]
