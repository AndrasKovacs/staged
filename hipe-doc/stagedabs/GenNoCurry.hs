

{-# language LambdaCase, TemplateHaskell, BlockArguments, RankNTypes,
    MultiParamTypeClasses, FunctionalDependencies, TypeApplications,
    ScopedTypeVariables, UndecidableInstances, QuantifiedConstraints,
    ImpredicativeTypes, TypeFamilies #-}

module GenNoCurry where

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

instance MonadGen m => MonadGen (StateT s m) where
  liftGen = lift . liftGen

instance MonadGen m => MonadGen (ReaderT r m) where
  liftGen = lift . liftGen

instance MonadGen m => MonadGen (ExceptT e m) where
  liftGen = lift . liftGen

gen :: MonadGen m => Up a -> m (Up a)
gen a = liftGen $ Gen $ \k -> [|| let x = $$a in $$(k [||x||]) ||]

class Improve m n where
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

-- instance Improve m n => Improve (ReaderT r m) (ReaderT (Up r) n) where
--   up   x = ReaderT \r -> up Q(runReaderT $$x $$r)
--   down x = [|| ReaderT \r -> $$(down (runReaderT x Q(r))) ||]


-- class Improve a b where
--   up   :: Up a -> b
--   down :: b -> Up a

-- instance Improve Bool (Gen Bool) where
--   up x = Gen \k -> [|| if $$x then $$(k True) else $$(k False) ||]
--   down x = unGen x \case True -> [||True||]; _ -> [||False||]

-- instance Improve (Identity a) (Gen (Up a)) where
--   up x = Gen \k -> k [|| runIdentity $$x ||]
--   down x = unGen x \a -> [|| Identity $$a ||]

-- type ImproveM m n = (forall a. Improve (m a) (n (Up a)), MonadGen n)

-- instance ImproveM m n =>
--   Improve (StateT s m a) (StateT (Up s) n (Up a)) where

--   up x = StateT \s ->
--     do (as :: Up (a, s)) <- up [|| runStateT $$x $$s ||]
--        caseUp as pure

--   down x = [|| StateT \s -> $$(down
--     do (a, s) <- runStateT x [||s||]
--        pure [|| ($$a, $$s) ||])
--     ||]

-- instance ImproveM m n => Improve (ExceptT e m a) (ExceptT (Up e) n (Up a)) where
--   up x = ExceptT do
--     (ea :: Up (Either e a)) <- up [|| runExceptT $$x ||]
--     caseUp ea pure

--   down (ExceptT x) =
--     [|| ExceptT $$(down (x >>= \case
--           Left e  -> pure [|| Left $$e ||]
--           Right a -> pure [|| Right $$a ||]
--                         )) ||]

-- instance ImproveM m n => Improve (ReaderT r m a) (ReaderT (Up r) n (Up a)) where
--   up   x = ReaderT \r -> up [|| runReaderT $$x $$r ||]
--   down x = [|| ReaderT \r -> $$(down (runReaderT x [|| r ||])) ||]

-- downM :: forall n m a. ImproveM m n => n (Up a) -> Up (m a)
-- downM x = down x

-- upM :: forall m n a. ImproveM m n => Up (m a) -> n (Up a)
-- upM x = up x

-- putG :: (MonadGen m, MonadState (Up s) m) => Up s -> m (Up ())
-- putG s = do
--   s <- gen s
--   put s
--   pure [||()||]

-- modifyG :: (MonadGen m, MonadState (Up s) m) => (Up s -> Up s) -> m (Up ())
-- modifyG f = do
--   s <- get
--   putG (f s)

-- localG :: (MonadGen m, MonadReader (Up r) m) => (Up r -> Up r) -> m a -> m a
-- localG f ma = do
--   r  <- ask
--   r' <- gen (f r)
--   local (const r') ma

-- class Split a b | a -> b where
--   split :: Up a -> Gen b

-- instance Split Bool Bool where
--   split = up

-- instance Split [a] (Maybe (Up a, Up [a])) where
--   split x = Gen \k -> [|| case $$x of
--     []   -> $$(k Nothing)
--     a:as -> $$(k (Just ([||a||], [||as||]))) ||]

-- instance Split (a, b) (Up a, Up b) where
--   split x = Gen \k -> [|| case $$x of (a, b) -> $$(k ([||a||], [||b||])) ||]

-- instance Split (Either a b) (Either (Up a) (Up b)) where
--   split x = Gen \k -> [|| case $$x of Left a -> $$(k (Left [||a||])); Right b -> $$(k (Right [||b||])) ||]

-- caseUp :: MonadGen m => Split a b => Up a -> (b -> m c) -> m c
-- caseUp a f = f =<< liftGen (split a)

-- upstate :: forall s s' m a. Improve s s' => Monad m =>
--   StateT s' m a -> StateT (Up s) m a
-- upstate (StateT f) = StateT \s -> do
--   (a, s) <- f (up s)
--   pure (a, down s)

-- --------------------------------------------------------------------------------
