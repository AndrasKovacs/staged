
module CFTT.MonadTailCall where

import Data.Kind
import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State.Strict hiding (modify')
import Control.Monad.Trans.Maybe

import CFTT.Up
import CFTT.Gen
import CFTT.SOP

data TailCall (f :: Type -> Type)(a :: Type) where
  Call' ∷ ∀ a b f. (Elₚ b → Up (f a)) → Elₚ b → TailCall f (Up a)
  Ret'  ∷ ∀ a f. a → TailCall f a

class Monad m => MonadTC f m | f -> m, m -> f where
  downTC ∷ ∀ a. m (TailCall f (Up a)) → Up (f a)

ret ∷ MonadTC f m => a -> m (TailCall f a)
ret a = pure $ Ret' a

tailcall1 ∷ MonadTC f m => Up (b -> f a) -> Up b -> m (TailCall f (Up a))
tailcall1 f x = pure $ Call' (\(Cons b Nil) -> [|| $$f $$b ||]) (Cons x Nil)

tailcall2 ∷ MonadTC f m => Up (b -> c -> f a) -> Up b -> Up c -> m (TailCall f (Up a))
tailcall2 f x y =
  pure $ Call' (\(Cons b (Cons c Nil)) -> [|| $$f $$b $$c ||]) (Cons x (Cons y Nil))

tailcall3 ∷ MonadTC f m => Up (b -> c -> d -> f a) -> Up b -> Up c -> Up d -> m (TailCall f (Up a))
tailcall3 f x y z =
  pure $ Call' (\(Cons b (Cons c (Cons d Nil))) -> [|| $$f $$b $$c $$d ||]) (Cons x (Cons y (Cons z Nil)))

instance MonadTC Identity Gen where
  downTC m = [|| Identity $$(unGen m \case
                                Call' f as -> [|| runIdentity $$(f as) ||]
                                Ret' a     -> a) ||]

instance MonadTC f m => MonadTC (ReaderT r f) (ReaderT (Up r) m) where
  downTC m = [|| ReaderT \r -> $$(downTC $ flip runReaderT [||r||] $ m >>= \case
                                     Call' f as -> do
                                       r <- ask
                                       pure $ Call' (\(Cons r x) -> [|| runReaderT $$(f x) $$r ||])
                                                    (Cons r as)
                                     Ret' a ->
                                       pure $ Ret' a
    )||]

instance MonadTC f m => MonadTC (MaybeT f) (MaybeT m) where
  downTC m = [|| MaybeT $$(downTC $ runMaybeT m >>= \case
    Just (Call' f as) -> pure $ Call' (\x -> [|| runMaybeT $$(f x) ||]) as
    Just (Ret' a)     -> pure $ Ret' [||Just $$a||]
    Nothing           -> pure $ Ret' [||Nothing||]
    )||]

instance MonadTC f m => MonadTC (StateT s f) (StateT (Up s) m) where
  downTC m = [|| StateT \s -> $$(downTC $ flip runStateT [||s||] m >>= \case
      (Call' f as, s) -> pure $ Call' (\(Cons s as) -> [|| runStateT $$(f as) $$s ||]) (Cons s as)
      (Ret' a    , s) -> pure $ Ret' [|| ($$a, $$s) ||]
     )||]

instance MonadTC f m => MonadTC (ExceptT e f) (ExceptT (Up e) m) where
  downTC m = [|| ExceptT $$(downTC $ runExceptT m >>= \case
      Left e             -> pure $ Ret' [||Left $$e||]
      Right (Call' f as) -> pure $ Call' (\x -> [|| runExceptT $$(f x) ||]) as
      Right (Ret' a)     -> pure $ Ret' [|| Right $$a ||]
     )||]
