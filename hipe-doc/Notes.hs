{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}

{-# language LambdaCase, TemplateHaskell, BlockArguments, RankNTypes,
    MultiParamTypeClasses, FunctionalDependencies, TypeApplications,
    ScopedTypeVariables, UndecidableInstances #-}

module Gen where

import Language.Haskell.TH
import Control.Monad.State.Strict
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Trans

type Up = CodeQ

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

class Improve a b where
  up   :: Up a -> b
  down :: b -> Up a

instance MonadGen m => MonadGen (StateT s m) where
  liftGen = lift . liftGen

instance MonadGen m => MonadGen (ReaderT r m) where
  liftGen = lift . liftGen

instance MonadGen m => MonadGen (ExceptT e m) where
  liftGen = lift . liftGen

gen :: MonadGen m => Up a -> m (Up a)
gen a = liftGen $ Gen $ \k -> [|| let x = $$a in seq x $$(k [||x||]) ||]

genLazy :: MonadGen m => Up a -> m (Up a)
genLazy a = liftGen $ Gen $ \k -> [|| let x = $$a in $$(k [||x||]) ||]

instance Improve Bool (Gen Bool) where
  up x = Gen \k -> [|| if $$x then $$(k True) else $$(k False) ||]
  down x = unGen x \case True -> [||True||]; _ -> [||False||]

instance Improve (Identity a) (Gen (Up a)) where
  up x = Gen \k -> k [|| runIdentity $$x ||]
  down x = unGen x \a -> [|| Identity $$a ||]

type ImproveM m n = (forall a. Improve (m a) (n (Up a)), MonadGen n)

instance ImproveM m n =>
  Improve (StateT s m a) (StateT (Up s) n (Up a)) where

  up x = StateT \s ->
    do as <- up [|| runStateT $$x $$s ||]
       pure ([|| fst $$as ||], [|| snd $$as ||])

  down x = [|| StateT \s -> $$(down
    do (a, s) <- runStateT x [||s||]
       pure [|| ($$a, $$s) ||])
    ||]

instance ImproveM m n => Improve (ExceptT e m a) (ExceptT (Up e) n (Up a)) where
  up x = ExceptT do
    ea <- up [|| runExceptT $$x ||]
    liftGen $ Gen \k -> [|| case $$ea of
      Left e  -> $$(k (Left [||e||]))
      Right a -> $$(k (Right [||a||]))||]

  down (ExceptT x) =
    [|| ExceptT $$(down (x >>= \case
          Left e  -> pure [|| Left $$e ||]
          Right a -> pure [|| Right $$a ||]
                        )) ||]

instance ImproveM m n => Improve (ReaderT r m a) (ReaderT (Up r) n (Up a)) where
  up   x = ReaderT \r -> up [|| runReaderT $$x $$r ||]
  down x = [|| ReaderT \r -> $$(down (runReaderT x [|| r ||])) ||]

putG :: (MonadGen m, MonadState (Up s) m) => Up s -> m ()
putG s = do
  s <- genLazy s
  put s

modifyG :: (MonadGen m, MonadState (Up s) m) => (Up s -> Up s) -> m ()
modifyG f = do
  s <- get
  putG (f s)

localG :: (MonadGen m, MonadReader (Up r) m) => (Up r -> Up r) -> m a -> m a
localG f ma = do
  r  <- ask
  r' <- genLazy (f r)
  local (const r') ma






{-
instance MonadGen m => MonadGen (StateT s m) where
  liftGen = lift . liftGen

instance MonadGen m => MonadGen (ReaderT r m) where
  liftGen = lift . liftGen

instance MonadGen m => MonadGen (ExceptT e m) where
  liftGen = lift . liftGen

gen :: MonadGen m => Up a -> m (Up a)
gen a = liftGen $ Gen $ \k -> [|| let x = $$a in seq x $$(k [||x||]) ||]

genLazy :: MonadGen m => Up a -> m (Up a)
genLazy a = liftGen $ Gen $ \k -> [|| let x = $$a in $$(k [||x||]) ||]

class MonadGen n => Fuse m n | m -> n, n -> m where
  up   :: Up (m a) -> n (Up a)
  down :: n (Up a) -> Up (m a)

instance Fuse m n => Fuse (ExceptT e m) (ExceptT (Up e) n) where
  up x = ExceptT do
    ea <- up [|| runExceptT $$x ||]
    liftGen $ Gen \k -> [|| case $$ea of
      Left e  -> $$(k (Left [||e||]))
      Right a -> $$(k (Right [||a||]))||]

  down (ExceptT x) =
    [|| ExceptT $$(down (x >>= \case
          Left e  -> pure [|| Left $$e ||]
          Right a -> pure [|| Right $$a ||]
                        )) ||]

instance Fuse Identity Gen where
  up   x = pure [|| runIdentity $$x ||]
  down x = [|| Identity $$(runGen x) ||]

instance Fuse m n => Fuse (ReaderT r m) (ReaderT (Up r) n) where
  up   x = ReaderT \r -> up [|| runReaderT $$x $$r ||]
  down x = [|| ReaderT \r -> $$(down (runReaderT x [|| r ||])) ||]

instance Fuse m n => Fuse (StateT s m) (StateT (Up s) n) where
  up x = StateT \s ->
    do as <- up [|| runStateT $$x $$s ||]
       pure ([|| fst $$as ||], [|| snd $$as ||])

  down x = [|| StateT \s -> $$(down
    do (a, s) <- runStateT x [||s||]
       pure [|| ($$a, $$s) ||])
    ||]

putG :: (MonadGen m, MonadState (Up s) m) => Up s -> m ()
putG s = do
  s <- genLazy s
  put s

modifyG :: (MonadGen m, MonadState (Up s) m) => (Up s -> Up s) -> m ()
modifyG f = do
  s <- get
  putG (f s)

localG :: (MonadGen m, MonadReader (Up r) m) => (Up r -> Up r) -> m a -> m a
localG f ma = do
  r  <- ask
  r' <- genLazy (f r)
  local (const r') ma
-}
