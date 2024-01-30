{-# LANGUAGE TypeFamilyDependencies #-}

{-# language LambdaCase, TemplateHaskell, BlockArguments, RankNTypes,
    MultiParamTypeClasses, FunctionalDependencies, TypeApplications,
    ScopedTypeVariables, UndecidableInstances, QuantifiedConstraints, DataKinds,
    TypeFamilies, CPP, PartialTypeSignatures, MagicHash, AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module GenNoCurry where

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

-- printUp :: Up a -> IO ()
-- printUp a = do
--   x <- unType <$> runQ (examineCode a)
--   print $ ppr x

newtype Gen a = Gen {
  unGen :: forall acc r. Val acc => Val r => (a -> Up acc -> Up r) -> Up acc -> Up r}

upFun :: Up (a -> b) -> Up a -> Up b
upFun f a = [|| $$f $$a ||]

downFun :: (Up a -> Up b) -> Up (a -> b)
downFun f = [|| \a -> $$(f [||a||]) ||]

-- Creating a Gen while simply forwarding accumulation
valGen :: forall a. (forall r. Val r => (a -> Up r) -> Up r) -> Gen a
valGen f = Gen \k acc -> f (\a -> k a acc)

runGen :: Val a => Gen (Up a) -> Up a
runGen (Gen f) = f (\x _ -> x) Up.tt

runGenFun :: forall a b. Val a => Val b => Gen (Up a -> Up b) -> Up (a -> b)
runGenFun (Gen f) = downFun $ f @a @b id

upBool :: Up Bool -> Gen Bool
upBool b = valGen \k -> [|| case $$b of True -> $$(k True); False -> $$(k False) ||]

downBool :: Bool -> Up Bool
downBool True = [||True||]
downBool False = [||False||]

gen :: Up a -> Gen (Up a)
gen a = valGen \k -> [|| let x = $$a in $$(k [||x||]) ||]

upPair :: Up (Pair a b) -> Gen (Pair (Up a) (Up b))
upPair ab = valGen \k -> [|| case $$ab of Pair a b -> $$(k (Pair [||a||] [||b||])) ||]

downPair :: Pair (Up a) (Up b) -> Up (Pair a b)
downPair (Pair a b) = [|| Pair $$a $$b ||]

foo = runGenFun do
  b <- upBool [||True||]
  if b then pure $ upFun [|| \(x :: Int) -> x + 10 ||]
       else pure $ upFun [|| \(x :: Int) -> x * 10 ||]

bar = runGenFun do
  b <- upBool [||True||]
  if b then pure $ \(x ::Up Int) -> [|| $$x + 10 ||]
       else pure $ \(x ::Up Int) -> [|| $$x * 10 ||]

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

class Split a b | a -> b, b -> a where
  split :: Up a -> Gen b

-- -- caseG :: (MonadGen m, MonadJoin m, Joinable c) => Split a b => Up a -> (b -> m c) -> m c
-- -- caseG a f = joinG (liftGen (split a) >>= f)

caseG' :: (MonadGen m) => Split a b => Up a -> (b -> m c) -> m c
caseG' a f = (liftGen (split a) >>= f)

instance Split Bool Bool where
  split = upBool

-- instance Split [a] (Maybe (Up a, Up [a])) where
--   split x = Gen \k -> [|| case $$x of
--     []   -> $$(k Nothing)
--     a:as -> $$(k (Just (Q(a), Q(as)))) ||]

-- instance Split (a, b) (Up a, Up b) where
--   split x = Gen \k -> [|| case $$x of
--     (a, b) -> $$(k (Q(a), Q(b))) ||]

-- instance Split (Either a b) (Either (Up a) (Up b)) where
--   split x = Gen \k -> [|| case $$x of
--     Left a  -> $$(k (Left Q(a)))
--     Right b -> $$(k (Right Q(b))) ||]

-- instance Split Bot Bot where
--   split x = Gen \_ -> [|| case $$x of ||]

-- --------------------------------------------------------------------------------

-- class MonadGen n => Improve m n | m -> n, n -> m where
--   up   :: Up (m a) -> n (Up a)
--   down :: n (Up a) -> Up (m a)

-- instance Improve Identity Gen where
--   up x = Gen \k -> k Q(runIdentity $$x)
--   down x = unGen x \a -> Q(Identity $$a)

-- instance Improve m n => Improve (StateT s m) (StateT (Up s) n) where

--   up x = StateT \s ->
--     do as <- up [|| runStateT $$x $$s ||]
--        caseG' as pure

--   down x = [|| StateT \s -> $$(down
--     do (a, s) <- runStateT x Q(s)
--        pure Q(($$a, $$s))
--        )||]

-- instance Improve m n => Improve (ExceptT e m) (ExceptT (Up e) n) where
--   up x = ExceptT do
--     ea <- up Q(runExceptT $$x)
--     caseG' ea pure

--   down (ExceptT x) =
--     [|| ExceptT $$(down (x >>= \case
--           Left e  -> pure Q(Left $$e)
--           Right a -> pure Q(Right $$a)
--                         )) ||]

-- instance Improve m n => Improve (ReaderT r m) (ReaderT (Up r) n) where
--   up   x = ReaderT \r -> up Q(runReaderT $$x $$r)
--   down x = [|| ReaderT \r -> $$(down (runReaderT x Q(r))) ||]

-- --------------------------------------------------------------------------------

-- -- putG :: (MonadGen m, Joinable s, MonadState s m) => s -> m (Up ())
-- -- putG s = do
-- --   s <- letG s
-- --   put s
-- --   pure Q(())

-- -- modifyG :: (MonadGen m, Joinable s, MonadState s m) => (s -> s) -> m (Up ())
-- -- modifyG f = do
-- --   s <- get
-- --   putG (f s)

-- -- localG :: (MonadGen m, Joinable r, MonadReader r m) => (r -> r) -> m a -> m a
-- -- localG f ma = do
-- --   r  <- ask
-- --   r' <- letG (f r)
-- --   local (const r') ma

-- -- Push
-- --------------------------------------------------------------------------------

newtype Push a = Push {
  unPush :: forall acc r. (Val acc, Val r) =>
        (a -> (Up acc -> Up r) -> Up acc -> Up r)
        -> (Up acc -> Up r) -> Up acc -> Up r}

foldrPush :: forall a b. Val b => (Up a -> Up b -> Up b) -> Up b -> Push (Up a) -> Up b
foldrPush f b (Push as) = as @() @b (\a b acc -> f a (b acc)) (\_ -> b) Up.tt

foldlPush :: forall a b. Val b => (Up b -> Up a -> Up b) -> Up b -> Push (Up a) -> Up b
foldlPush f b (Push as) = as @b @b (\a b acc -> b (f acc a)) (\acc -> acc) b

instance Semigroup (Push  a) where
  Push xs <> Push ys = Push \c n -> xs c (ys c n)

instance Monoid (Push a) where
  mempty = Push \c n -> n

instance Functor Push where
  fmap f (Push as) = Push \c -> as (c . f)

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
  push as = Push \c n acc -> [||
     let go (Pair [] acc)     = $$(n [||acc||])
         go (Pair (a:as) acc) = $$(c [||a||] (\acc -> [||go (Pair as $$acc)||]) [||acc||])
     in go (Pair $$as $$acc)||]

class ToList a b | a -> b where
  list :: a -> Up [b]

instance Val a => ToList (Push (Up a)) a where
  list = foldrPush (\a as -> [||$$a : $$as||]) [||[]||]

sumPush :: Push (Up Int) -> Up Int
sumPush = foldlPush (+) 0

class Filter f where
  filter :: (a -> Up Bool) -> f a -> f a

instance Filter Push where
  filter f as = do
    a <- as
    caseG' (f a) \case
      True -> pure a
      _    -> mempty

-- -- class Drop f where
-- --   drop :: Up Int -> f a -> f a

-- -- instance Drop Push where
-- --   drop n (Push as) = Push \c nil -> [||
-- --     $$(as (\a hyp -> [||\i -> case i <=# 0# of
-- --               1# -> $$(c a [|| $$hyp 0# ||])
-- --               _  -> $$hyp (i -# 1#) ||])
-- --           [|| \_ -> $$nil ||]) (case $$n of I# i -> i)  ||]

-- -- class Take f where
-- --   take :: Up Int -> f a -> f a

-- -- instance Take Push where
-- --   take n (Push as) = Push \c nil -> [||
-- --     $$(as (\a hyp -> [||\i -> case i <=# 0# of
-- --               1# -> $$nil
-- --               _  -> $$(c a [|| $$hyp (i -# 1#) ||]) ||])
-- --           [|| \_ -> $$nil ||]) (case $$n of I# i -> i)
-- --     ||]

-- -- instance Improve [] Push where
-- --   up   = push
-- --   down = list

-- -- traversePush :: Improve m n => (Up a -> n (Up b)) -> Push (Up a) -> Up (m [b])
-- -- traversePush f as =
-- --   unPush as (\a bs -> down do {bs <- up bs; b <- f a; pure [|| $$b : $$bs ||]}) (down $ pure [||[]||])


-- Traditional pull
--------------------------------------------------------------------------------

-- -- -- data Pull a = forall s. Pull (Up s) (Up s -> MaybeT Gen (Up s, a))

-- -- -- data Pair a b = Pair !a !b

-- -- -- instance Split (Pair a b) (Pair (Up a) (Up b)) where
-- -- --   split ab = Gen \k -> [|| case $$ab of Pair a b -> $$(k (Pair Q(a) Q(b))) ||]

-- -- -- instance Functor Pull where
-- -- --   fmap f (Pull s step) = Pull s (fmap (fmap (fmap f)) step)

-- -- -- instance Applicative Pull where
-- -- --   pure a = Pull Up.tt \s -> pure (s, a)
-- -- --   Pull s step <*> Pull s' step' =
-- -- --     Pull [||Pair $$s $$s'||] \ss' ->
-- -- --       caseG' ss' \(Pair s s') -> do
-- -- --         (s, f)  <- step s
-- -- --         (s', a) <- step' s'
-- -- --         pure ([|| Pair $$s $$s' ||], f a)

-- -- -- range :: Up Int -> Up Int -> Pull (Up Int)
-- -- -- range x y = Pull [|| Pair $$x $$y ||] \xy ->
-- -- --   caseG xy \(Pair x y) ->
-- -- --   caseG (x Up.>= y) \case
-- -- --     True -> empty
-- -- --     _    -> pure ([|| Pair ($$x + 1) $$y ||], x)

-- -- -- instance ToPush (Pull a) a where
-- -- --   push (Pull s step) = Push \c n ->
-- -- --     [|| let go s = seq s $$(unGen (runMaybeT (step [||s||])) \case
-- -- --                            Nothing     -> n
-- -- --                            Just (s, a) -> c a [||go $$s||]) in
-- -- --         go $$s ||]

-- -- -- tee :: Functor f => f a -> f (a, a)
-- -- -- tee = fmap (\a -> (a, a))

-- -- --------------------------------------------------------------------------------

-- -- switchState :: Monad m => (s -> m s') -> (s' -> m s) -> StateT s' m a -> StateT s m a
-- -- switchState f g (StateT ma) = StateT \s -> do
-- --   s <- f s
-- --   (a, s) <- ma s
-- --   s <- g s
-- --   pure (a, s)

-- -- upBool :: MonadGen m => Up Bool -> m Bool
-- -- upBool b = liftGen $ Gen \k -> [|| if $$b then $$(k True) else $$(k False) ||]

-- -- downBool :: Bool -> Up Bool
-- -- downBool True  = [||True||]
-- -- downBool False = [||False||]

-- -- --------------------------------------------------------------------------------

-- -- instance MonadGen m => MonadGen (ContT r m) where
-- --   liftGen = lift . liftGen

-- -- instance (MonadJoin m, Joinable r) => MonadJoin (ContT r m) where
-- --   joinG x = ContT \k -> runContT x \a -> joinG (k a)

-- -- instance Improve m n => Improve (ContT r m) (ContT (Up r) n) where
-- --   up x   = ContT \k -> up [|| runContT $$x \a -> $$(down (k [||a||])) ||]
-- --   down x = [|| ContT \k -> $$(down $ runContT x \a -> up [|| k $$a ||]) ||]


-- -- --------------------------------------------------------------------------------

-- -- data Pull a where
-- --   Pull :: ProdSumUp s -> s -> (s -> MaybeT Gen (s, a)) -> Pull a

-- -- instance Functor Pull where
-- --   fmap f (Pull ps s step) = Pull ps s $ fmap (fmap (fmap f)) step

-- -- instance Applicative Pull where
-- --   pure a = Pull JUp Up.tt \s -> pure (s, a)
-- --   Pull ps s step <*> Pull ps' s' step' =
-- --     Pull (JPair ps ps') (s, s') \(s, s') -> do
-- --       (s, f) <- step s
-- --       (s', a) <- step' s'
-- --       pure ((s, s'), f a)

-- -- instance Semigroup (Pull a) where
-- --   Pull ps s step <> Pull ps' s' step' =
-- --     Pull (JEither ps ps') (Left s) \case
-- --       Left s   -> do {(s, a) <- step s; pure (Left s, a)}
-- --       Right s' -> do {(s', a) <- step' s'; pure (Right s', a)}

-- -- instance Monoid (Pull a) where
-- --   mempty = Pull JUp Up.tt \_ -> empty


-- -- --------------------------------------------------------------------------------

-- -- pp :: Either (Up Int) (Up Bool)
-- --   -> (Either (Up Int) (Up Bool) -> MaybeT Gen (Either (Up Int) (Up Bool), a)) -> Push a
-- -- pp s step = Push \cons nil ->
-- --   [|| let f n = $$(unGen (runMaybeT (step (Left [||n||]))) \case
-- --                       Nothing     -> nil
-- --                       Just (s, a) -> case s of
-- --                         Left s   -> cons a [|| f $$s ||]
-- --                         Right s' -> cons a [|| g $$s' ||]
-- --                       )
-- --           g n = $$(unGen (runMaybeT (step (Right [||n||]))) \case
-- --                       Nothing     -> nil
-- --                       Just (s, a) -> case s of
-- --                         Left s   -> cons a [|| f $$s ||]
-- --                         Right s' -> cons a [|| g $$s' ||]
-- --                       )
-- --       in $$(either (\n -> [|| f $$n ||]) (\n -> [|| g $$n ||]) s)
-- --    ||]


-- -- -- pp :: Pull a -> Push a
-- -- -- pp (Pull ps s step) = Push \c n -> _


-- -- -- (a + b) -> Gen ((a + b)*x + 1)

-- -- -- a -> Gen ((a + b)*x + 1)
-- -- -- b -> Gen ((a + b)*x + 1)

-- -- -- FinRep a => Gen a           forall r. (a -> Up r) -> Up r
-- -- --                             forall r. Arr a (Up r) -> Up r


-- -- -- Gen (Either (Up a) (Up b)) -> Either (Up a) (Up b)

-- -- --        forall r. ((s * a) + 1 -> Up r) -> Up r
-- -- --        forall r. (s * a -> Up r, 1 -> Up r) -> Up r
-- -- --        forall r.
