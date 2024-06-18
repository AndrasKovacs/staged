
module CFTT.Gen where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State.Strict hiding (modify')
import Control.Monad.Trans.Maybe
import CFTT.Up

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

gen :: MonadGen m => Up a -> m (Up a)
gen a = liftGen $ Gen \k -> [|| let x = $$a in seq x $$(k [||x||]) ||]

genRec :: MonadGen m => (Up a -> Up a) -> m (Up a)
genRec a = liftGen $ Gen \k -> [|| let x = $$(a [||x||]) in $$(k [||x||]) ||]

put' :: (MonadState (Up s) m, MonadGen m) => Up s -> m (Up ())
put' s = do
  s <- gen s
  put s
  pure [||()||]

modify' :: (MonadState (Up s) m, MonadGen m) => (Up s -> Up s) -> m (Up ())
modify' f = do
  s <- get
  put' (f s)

local' :: (MonadReader (Up r) m, MonadGen m) => (Up r -> Up r) -> m a -> m a
local' f ma = do
  r <- ask
  r <- gen (f r)
  local (const r) ma
