

{-# language LambdaCase, TemplateHaskell, BlockArguments, RankNTypes,
    MultiParamTypeClasses, FunctionalDependencies, TypeApplications,
    ScopedTypeVariables, UndecidableInstances, QuantifiedConstraints,
    TypeFamilies, CPP, PartialTypeSignatures, MagicHash, AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Gen where

import Prelude hiding (filter, zip, zipWith, take, drop)
import qualified Prelude as P
import Control.Monad
import Language.Haskell.TH hiding (ListT, ListE)
import Control.Monad.State.Strict
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Data.Coerce
import Control.Applicative
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

letG :: MonadGen m => Up a -> m (Up a)
letG a = liftGen $ Gen $ \k -> [|| let x = $$a in seq x $$(k Q(x)) ||]

letGLazy :: MonadGen m => Up a -> m (Up a)
letGLazy a = liftGen $ Gen $ \k -> [|| let x = $$a in $$(k Q(x)) ||]

--------------------------------------------------------------------------------

class Split a b | a -> b, b -> a where
  split :: Up a -> Gen b

caseG :: MonadGen m => Split a b => Up a -> (b -> m c) -> m c
caseG a f = liftGen (split a) >>= f

-- letG, joinG
-- -- create a join point
-- joinG :: MonadGen m => ((forall b. Up a -> m b) -> m (Up a)) -> m (Up a)
-- joinG f = liftGen $ Gen \k -> _

-- joinG :: ((forall b. Up a -> Gen b) -> Gen (Up a)) -> Gen (Up a)
-- joinG f = Gen \k -> [|| let cont r = k in _ ||]

-- joinG :: Gen (Up a) -> Gen (Up a)
-- joinG ga = Gen \k -> [|| let cont a = $$(k [||a||]) in $$(unGen ga \a -> [|| cont $$a ||]) ||]

-- joinG' :: MonadGen m => m (Up a) -> m (Up a)
-- joinG' ga = do
--   a <- ga
--   a <- liftGen $ Gen \k -> [|| let cont a = $$(k Q(a)) in cont $$a ||]
--   _

-- I want to have joinG for MonadCont!!

type M s = StateT s Gen

-- Not quite the best joinG
joinG' :: Improve m n => n (Up a) -> n (Up a)
joinG' ma = up =<< letG (down ma)

  -- StateT \s -> Gen \k ->
  --   [|| let cont a s = $$(k (Q(a), Q(s))) in $$(unGen (runStateT ma s) \(a, s) -> [|| cont $$a $$s ||]) ||]

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
       caseG as pure

  down x = [|| StateT \s -> $$(down
    do (a, s) <- runStateT x Q(s)
       pure Q(($$a, $$s))
       )||]

instance Improve m n => Improve (ExceptT e m) (ExceptT (Up e) n) where
  up x = ExceptT do
    ea <- up Q(runExceptT $$x)
    caseG ea pure

  down (ExceptT x) =
    [|| ExceptT $$(down (x >>= \case
          Left e  -> pure Q(Left $$e)
          Right a -> pure Q(Right $$a)
                        )) ||]

instance Improve m n => Improve (ReaderT r m) (ReaderT (Up r) n) where
  up   x = ReaderT \r -> up Q(runReaderT $$x $$r)
  down x = [|| ReaderT \r -> $$(down (runReaderT x Q(r))) ||]

--------------------------------------------------------------------------------

putG :: (MonadGen m, MonadState (Up s) m) => Up s -> m (Up ())
putG s = do
  s <- letG s
  put s
  pure Q(())

modifyG :: (MonadGen m, MonadState (Up s) m) => (Up s -> Up s) -> m (Up ())
modifyG f = do
  s <- get
  putG (f s)

localG :: (MonadGen m, MonadReader (Up r) m) => (Up r -> Up r) -> m a -> m a
localG f ma = do
  r  <- ask
  r' <- letGLazy (f r)
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
    caseG (f a) \case
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
traversePush f as = unPush as (\a bs -> down do {bs <- up bs; b <- f a; pure [|| $$b : $$bs ||]}) (down $ pure [||[]||])

-- foo :: Push a -> Up (IO ())
-- foo (Push as) =

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
      caseG ss' \(Pair s s') -> do
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

-- CSE for Applicative:
-- f <$> x <*> x --> join f <$> x
-- f <$> x
-- join f <$> x

--------------------------------------------------------------------------------

-- class MonadJoinG m where
--   joinG :: m a -> m a

-- data Foo s a = M (s -> (Maybe (a, Foo s a), s))

-- newtype LS s a = LS {unLS ::
--   forall pt res. ((s -> Up res) -> Up pt) -> (s -> Up res) -> (a -> s -> Up pt -> Up res) -> Up pt}
--   deriving Functor


-- con : (Maybe (a, List a)) -> List a

--     () -> List a
--     a -> List a -> List a

newtype ListE e a = ListE {unListE :: forall r. (a -> Up r -> Up r) -> (e -> Up r) -> Up r -> Up r}
  deriving Functor

instance Applicative (ListE e) where
  pure a = ListE \c e n -> c a n
  (<*>) = ap

instance Monad (ListE e) where
  return = pure
  ListE as >>= f = ListE \c e n -> as (\a bs -> unListE (f a) c e bs) e n

instance Semigroup (ListE e a) where
  ListE xs <> ListE ys = ListE \c e n -> xs c e (ys c e n)

instance Monoid (ListE e a) where
  mempty = ListE \c e n -> n

earlyExit :: e -> ListE e a
earlyExit e = ListE \c t n -> t e

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving (Prelude.Eq, Show)

listTree :: Up (Tree a) -> ListE e (Up a)
listTree t = ListE \c e n -> [||
  let go (Leaf a)   acc = $$(c Q(a) Q(acc))
      go (Node l r) acc = go l (go r acc)
  in go $$t $$n
  ||]

instance ToPush (Up (Tree a)) (Up a) where
  push t = Push \c n -> [||
    let go (Leaf a)   acc = $$(c Q(a) Q(acc))
        go (Node l r) acc = go l (go r acc)
    in go $$t $$n
    ||]

instance MonadGen (ListE e) where
  liftGen ga = ListE \c e n -> unGen ga \a -> c a n

runListE :: ListE (Up e) (Up a) -> Up (Either e [a])
runListE as = unListE as (\a r ->
   [|| case $$r of Left e -> Left e; Right as -> Right ($$a : as)||]) (\e -> [|| Left $$e ||]) [|| Right [] ||]

-- instance Semigroup (LS s a) where
--   LS xs <> LS ys = LS \con n j -> xs (\hyp -> con hyp) (\s -> ys _ n _) _

-- instance Monoid (LS s a) where
--   mempty = LS \con n j -> con \s -> (n, s)

-- module _ (S A : Set) where
--   data PT : Set
--   data Res : Set

--   data PT where
--     con : (S → Res × S) → PT

--   data Res where
--     nothing : Res
--     just    : A → PT → Res

--   append : PT → PT → PT
--   append (con xs) (con ys) =
--     con λ s → case xs s of λ { (nothing   , s) → ys s
--                              ; (just x xs , s) → (just x (append xs (con ys))) , s}


-- PushT (State

-- newtype PushT m a = PushT {unPushT :: forall pt res. (m (Up res) -> Up pt) -> Up res -> (a -> Up pt -> Up res) -> Up pt}
--   deriving Functor

-- instance Monad m => Semigroup (PushT m a) where
--   PushT xs <> PushT ys = PushT \(con :: m (Up res) -> Up pt) n j ->
--     xs @pt con _ undefined

-- ListT m a = m (Maybe (a, ListT m a))
-- Up (Foo s a) =

-- Foo     : Set
-- Res     : Set
-- mkFoo   : (s -> Res) -> Foo
-- nothing : s -> Res
-- just    : a -> s -> Foo -> Res

-- forall foo res. ((s -> (res, s)) -> foo) -> res -> (a -> foo -> res) -> foo

-- forall pt res. ((s -> res) -> pt) -> (s -> res) -> (a -> s -> pt -> res) -> pt

-- forall pt res. ((s -> Up res) -> Up pt) -> (a -> Up res) -> (a -> s -> Up pt -> Up res) -> Up pt


-- newtype ListT m a = ListT (m (Maybe (a, ListT m a)))

-- Up (forall foo res. ((s -> (res, s)) -> foo) -> res -> (a -> foo -> res) -> foo)

              -- ((s -> (Up res, s)) -> Up foo) -> Up res -> (a -> Up foo -> Up res) -> Up foo


{-

1. How best to join up monadic computation? class MonadJoin?
   - up/down is fine for Reader & State, but not for Except!
     Problem is that it converts back to Left/Right instead of
     using two continuations!

Push transformer:

  - In general, PushT m does not have appending!!! So it's not a monad, either.
  - It's a monad if "m a" contains finite copies of "a".
  - For example, PushT (Either e) a works for "early exit".
    (However, running it requires dynamic Either, or exceptional returns)

-}

--------------------------------------------------------------------------------

-- "dynamic continuation" form
class DynCont a where
  type DC a
  to  :: a -> DC a
  fro :: DC a -> a

instance DynCont (Up a -> Up b) where
  type DC (Up a -> Up b) = Up (a -> b)
  to f    = [|| \a -> $$(f [||a||]) ||]
  fro f a = [|| $$f $$a ||]

instance (DynCont (a -> c), DynCont (b -> c)) => DynCont (Either a b -> c)  where
  type DC (Either a b -> c) = (DC (a -> c), DC (b -> c))
  to f = (to (f . Left), to (f . Right))
  fro (f, g) (Left a)  = fro f a
  fro (f, g) (Right b) = fro g b

instance (DynCont (Up () -> b), DynCont (a -> b)) => DynCont (Maybe a -> b) where
  type DC (Maybe a -> b) = (DC (Up () -> b), DC (a -> b))
  to f = (to @(Up () -> b) (const (f Nothing)), to (f . Just))
  fro (f, g) Nothing  = fro f tt
  fro (f, g) (Just a) = fro g a

instance (DynCont (b -> c), DynCont (a -> DC (b -> c))) => DynCont ((a, b) -> c) where
  type DC ((a, b) -> c) = DC (a -> DC (b -> c))
  to f = to \a -> to \b -> f (a, b)
  fro f (a, b) = fro (fro f a) b

--------------------------------------------------------------------------------

class DeepLet a where
  deepLet :: a -> Gen a

instance DeepLet (Up a) where
  deepLet = letG

instance (DeepLet a, DeepLet b) => DeepLet (a, b) where
  deepLet (a, b) = (,) <$> deepLet a <*> deepLet b

instance (DeepLet a, DeepLet b) => DeepLet (Either a b) where
  deepLet (Left a)  = Left <$> deepLet a
  deepLet (Right b) = Right <$> deepLet b

instance DeepLet a => DeepLet (Maybe a) where
  deepLet = traverse deepLet

--------------------------------------------------------------------------------

-- TODO, cleanup, also allow State s for joinable s
--  Joinable should be generated by: Up, finite sums and products
--  todo: look at Cont monad

class    (DynCont (a -> Up r), DeepLet (DC (a -> Up r))) => DynCont' a r
instance (DynCont (a -> Up r), DeepLet (DC (a -> Up r))) => DynCont' a r
type Joinable a = forall r. DynCont' a r

class MonadJoin m where
  joinG :: Joinable a => m a -> m a

instance MonadJoin Gen where
  joinG ma = Gen \k -> unGen (deepLet (to k)) \k -> unGen ma (fro k)

instance (Joinable e, MonadJoin m) => MonadJoin (ExceptT e m) where
  joinG (ExceptT ma) = ExceptT $ joinG ma

instance (MonadJoin m) => MonadJoin (StateT (Up s) m) where
  joinG (StateT ma) = StateT \s -> joinG (ma s)

instance MonadJoin m => MonadJoin (MaybeT m) where
  joinG (MaybeT ma) = MaybeT $ joinG ma

instance (Joinable r, MonadJoin m) => MonadJoin (ReaderT r m) where
  joinG (ReaderT ma) = ReaderT \r -> joinG (ma r)
