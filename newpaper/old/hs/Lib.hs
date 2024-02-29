
{-# language LambdaCase, TemplateHaskell, BlockArguments, RankNTypes,
   MultiParamTypeClasses, FunctionalDependencies, TypeApplications,
   ScopedTypeVariables, UndecidableInstances, TypeFamilies,
   TypeFamilyDependencies, PartialTypeSignatures, AllowAmbiguousTypes,
   DataKinds, PolyKinds, MagicHash, ViewPatterns #-}

{-# options_ghc -Wincomplete-patterns #-}

module Lib where

import Control.Applicative
import Control.Monad hiding (join)
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State.Strict hiding (modify')
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Data.Coerce
import Data.Kind
import GHC.Exts
import Language.Haskell.TH hiding (ListT, ListE, Type, BindS)
import Data.Typeable

import Prelude
import qualified Prelude as P

import Up
import SOP

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

gen :: MonadGen m => Up a -> m (Up a)
gen a = liftGen $ Gen \k -> [|| let x = $$a in seq x $$(k [||x||]) ||]

genRec :: MonadGen m => (Up a -> Up a) -> m (Up a)
genRec a = liftGen $ Gen \k -> [|| let x = $$(a [||x||]) in $$(k [||x||]) ||]

genP :: MonadGen m => P as -> m (P as)
genP Nil         = pure Nil
genP (Cons a as) = Cons <$> gen a <*> genP as

-- Binding-time improvement for base types
--------------------------------------------------------------------------------

class Improve0 a b | a -> b, b -> a where
  up0 :: Up a -> b
  down0 :: b -> Up a

instance Improve0 (Pair a b) (Pair (Up a) (Up b)) where
  up0 x = Pair [|| proj1 $$x ||] [|| proj2 $$x ||]
  down0  (Pair a b) = [|| Pair $$a $$b ||]

instance Improve0 Bool (Gen Bool) where
  up0 x = Gen \k -> [|| if $$x then $$(k True) else $$(k False) ||]
  down0 x = unGen x \x -> if x then [||True||] else [||False||]

instance Improve0 (Maybe a) (Gen (Maybe (Up a))) where
  up0 x = Gen \k -> [|| case $$x of Nothing -> $$(k Nothing); Just a -> $$(k (Just [||a||])) ||]
  down0 x = unGen x \case Nothing -> [||Nothing||]; Just a -> [||Just $$a ||]

instance Improve0 (Either a b) (Gen (Either (Up a) (Up b))) where
  up0 x = Gen \k -> [|| case $$x of Left a -> $$(k (Left [||a||])); Right b -> $$(k (Right [||b||])) ||]
  down0 x = unGen x \case Left a -> [||Left $$a||]; Right b -> [||Right $$b||]

--------------------------------------------------------------------------------

class MonadJoin m where
  join :: IsSOP a => m a -> m a

instance MonadJoin Gen where
  join ma = Gen \(k :: a -> Up r) -> runGen do
    conts <- genP (tabulate (singRep @a) (k . decode))
    a <- ma
    pure $ index conts (encode a)

instance (IsSOP e, MonadJoin m) => MonadJoin (ExceptT e m) where
  join (ExceptT ma) = ExceptT $ join ma

instance (MonadJoin m, IsSOP s) => MonadJoin (StateT s m) where
  join (StateT ma) = StateT (join . ma)

instance MonadJoin m => MonadJoin (MaybeT m) where
  join (MaybeT ma) = MaybeT $ join ma

instance (MonadJoin m) => MonadJoin (ReaderT r m) where
  join (ReaderT ma) = ReaderT (join . ma)

--------------------------------------------------------------------------------

class Split a b | a -> b, b -> a where
  split :: Up a -> Gen b

case' :: (MonadGen m) => Split a b => Up a -> (b -> m c) -> m c
case' a f = liftGen (split a) >>= f

case'' :: (MonadJoin m, MonadGen m, IsSOP c) => Split a b => Up a -> (b -> m c) -> m c
case'' a f = join $ case' a f

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
       case' as pure

  down x = [|| StateT \s -> $$(down
    do (a, s) <- runStateT x [||s||]
       pure [||($$a, $$s)||]
       )||]

instance (Improve m n) => Improve (ExceptT e m) (ExceptT (Up e) n) where
  up x = ExceptT do
    ea <- up [||runExceptT $$x||]
    case' ea pure

  down (ExceptT x) =
    [|| ExceptT $$(down (x >>= \case
          Left e  -> pure [||Left $$e||]
          Right a -> pure [||Right $$a||]
                        )) ||]

instance Improve m n => Improve (MaybeT m) (MaybeT n) where
  up x = MaybeT do
    ma <- up [||runMaybeT $$x||]
    case' ma pure

  down (MaybeT x) =
    [|| MaybeT $$(down (x >>= \case
          Nothing -> pure [||Nothing||]
          Just a -> pure [||Just $$a||])) ||]

instance Improve m n => Improve (ReaderT r m) (ReaderT (Up r) n) where
  up   x = ReaderT \r -> up [||runReaderT $$x $$r||]
  down x = [|| ReaderT \r -> $$(down (runReaderT x [||r||])) ||]

--------------------------------------------------------------------------------

put' :: forall m s. (MonadGen m, MonadState (Up s) m) => Up s -> m (Up ())
put' s = do
  s <- gen s
  put s
  pure Up.tt

modify' :: (MonadGen m, MonadState (Up s) m) => (Up s -> Up s) -> m (Up ())
modify' f = do
  s <- get
  put' (f s)

local' :: (MonadGen m, MonadReader (Up r) m) => (Up r -> Up r) -> m a -> m a
local' f ma = do
  r  <- ask
  r' <- gen (f r)
  local (const r') ma


--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

data Step s a = Stop | Yield a s | Skip s deriving Functor

(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip fmap

data Pull a where
  Pull :: forall s a. IsSOP s => s -> (s -> Gen (Step s a)) -> Pull a

instance Semigroup (Pull a) where
  Pull @s seed step <> Pull @s' seed' step' =
    Pull @(Either s s') (Left seed) \case
      Left s   -> step s <&> \case
        Stop       -> Skip $ Right seed'
        Skip s     -> Skip $ Left s
        Yield a s  -> Yield a $ Left s
      Right s' -> step' s' <&> \case
        Stop       -> Stop
        Skip s'    -> Skip $ Right s'
        Yield a s' -> Yield a $ Right s'

instance Monoid (Pull a) where
  mempty = Pull () \_ -> pure Stop

single :: a -> Pull a
single a = Pull True $ \case
  True  -> pure $ Yield a False
  False -> pure Stop

bindGen :: Pull a -> (a -> Gen b) -> Pull b
bindGen (Pull seed step) f = Pull seed \s -> step s >>= \case
  Stop      -> pure $ Stop
  Skip s    -> pure $ Skip s
  Yield a s -> (`Yield` s) <$> f a

genPull :: Up a -> Pull (Up a)
genPull a = bindGen (single a) gen

instance Functor Pull where
  fmap f (Pull seed step) = Pull seed (fmap (fmap (fmap f)) step)

instance Applicative Pull where
  pure a = Pull () \_ -> pure $ Yield a ()

  Pull seed step <*> Pull seed' step' =
    Pull (seed, seed') \(s, s') -> step s >>= \case
      Stop      -> pure Stop
      Skip s    -> pure $ Skip (s, s')
      Yield f s -> step' s' >>= \case
        Stop       -> pure Stop
        Skip s'    -> pure $ Skip (s, s')
        Yield a s' -> pure $ Yield (f a) (s, s')

--------------------------------------------------------------------------------

undefinedP :: Sing as -> P as
undefinedP SNil         = Nil
undefinedP (SCons a as) = Cons [||undefined||] (undefinedP as)

data Pull' as b where
  Pull' :: forall s as b. IsSOP s => (P as -> s) -> (P as -> s -> Gen (Step s b)) -> Pull' as b

unravel :: Sing as -> (P as -> Pull b) -> Pull' as b
unravel as f = case f (undefinedP as) of
  Pull @s seed step ->
    Pull' @s (\xs -> case f xs of Pull seed _ -> unsafeCoerce# seed)
             (\xs -> case f xs of Pull _ step -> unsafeCoerce# step)

singleP :: Up a -> P '[ a ]
singleP a = Cons a Nil

unSingleP :: P '[ a ] -> Up a
unSingleP (Cons a Nil) = a

unravel1 :: (Up a -> Pull b) -> Pull' '[ a ] b
unravel1 f = unravel sing (f . unSingleP)

unravel2 :: ((Up a, Up b) -> Pull c) -> Pull' '[a, b] c
unravel2 f = unravel sing (\(Cons a (Cons b Nil)) -> f (a, b))

bind :: forall a b. Pull (Up a) -> (Up a -> Pull b) -> Pull b
bind (Pull @s seed step) (unravel1 -> Pull' @s' seed' step') =
  Pull @(s, Maybe (Up a, s')) (seed, Nothing) \case
    (s, Nothing) -> step s <&> \case
      Stop       -> Stop
      Skip s     -> Skip (s, Nothing)
      Yield a s  -> Skip (s, Just (a, seed' (singleP a)))
    (s, Just (a, s')) -> step' (singleP a) s' <&> \case
      Stop       -> Skip (s, Nothing)
      Skip s'    -> Skip (s, Just (a, s'))
      Yield b s' -> Yield b (s, Just (a, s'))

--------------------------------------------------------------------------------

class CasePull a b | a -> b, b -> a where
  casePull :: Up a -> (b -> Pull c) -> Pull c

instance CasePull (a, b) (Up a, Up b) where
  casePull x (unravel2 -> Pull' @s seed' step') =
    Pull @(Maybe (Up a, Up b, s)) Nothing \case
      Nothing -> case' x \case
        (a, b) -> _



instance CasePull Bool Bool where
  casePull b f = case (f True, f False) of
    (Pull @s seed step, Pull @s' seed' step') ->
      Pull @(Maybe (Either s s')) Nothing \case
        Nothing -> case' b \case True  -> pure $ Skip (Just (Left seed))
                                 False -> pure $ Skip (Just (Right seed'))
        Just (Left s)   -> step s >>= \case
          Stop      -> pure Stop
          Skip s    -> pure $ Skip (Just (Left s))
          Yield c s -> pure $ Yield c (Just (Left s))
        Just (Right s') -> step' s' >>= \case
          Stop      -> pure Stop
          Skip s    -> pure $ Skip (Just (Right s))
          Yield c s -> pure $ Yield c (Just (Right s))

instance CasePull (Either a b) (Either (Up a) (Up b)) where
  casePull x f = case (unravel1 (f . Left), unravel1 (f . Right)) of
    (Pull' @s seed step, Pull' @s' seed' step') ->
      Pull @(Maybe (Either (Up a, s) (Up b, s'))) Nothing \case
        Nothing -> case' x \case
          Left a    -> pure $ Skip (Just (Left  (a, seed  (singleP a))))
          Right b   -> pure $ Skip (Just (Right (b, seed' (singleP b))))
        Just (Left (a, s)) -> step (singleP a) s >>= \case
          Stop      -> pure Stop
          Skip s    -> pure $ Skip (Just (Left (a, s)))
          Yield c s -> pure $ Yield c (Just (Left (a, s)))
        Just (Right (b, s)) -> step' (singleP b) s >>= \case
          Stop      -> pure Stop
          Skip s    -> pure $ Skip (Just (Right (b, s)))
          Yield c s -> pure $ Yield c (Just (Right (b, s)))

instance CasePull (Maybe a) (Maybe (Up a)) where
  casePull x f = case (f Nothing, unravel1 (f . Just)) of
    (Pull @s seed step, Pull' @s' seed' step') ->
      Pull @(Maybe (Either s (Up a, s'))) Nothing \case
        Nothing -> case' x \case
          Nothing   -> pure $ Skip (Just (Left  seed))
          Just a    -> pure $ Skip (Just (Right (a, seed' (singleP a))))
        Just (Left s) -> step s >>= \case
          Stop      -> pure Stop
          Skip s    -> pure $ Skip (Just (Left s))
          Yield c s -> pure $ Yield c (Just (Left s))
        Just (Right (a, s)) -> step' (singleP a) s >>= \case
          Stop      -> pure Stop
          Skip s    -> pure $ Skip (Just (Right (a, s)))
          Yield c s -> pure $ Yield c (Just (Right (a, s)))

instance CasePull [a] (Maybe (Up a, Up [a])) where
  casePull x f = case (f Nothing, unravel2 (f . Just)) of
    (Pull @s seed step, Pull' @s' seed' step') ->
      Pull @(Maybe (Either s (Up a, Up [a], s'))) Nothing \case
        Nothing -> case' x \case
          Nothing      -> pure $ Skip (Just (Left  seed))
          Just (a, as) -> pure $ Skip (Just (Right (a, as, seed' (Cons a (Cons as Nil)))))
        Just (Left s) -> step s >>= \case
          Stop      -> pure Stop
          Skip s    -> pure $ Skip (Just (Left s))
          Yield c s -> pure $ Yield c (Just (Left s))
        Just (Right (a, as, s)) -> step' (Cons a (Cons as Nil)) s >>= \case
          Stop      -> pure Stop
          Skip s    -> pure $ Skip (Just (Right (a, as, s)))
          Yield c s -> pure $ Yield c (Just (Right (a, as, s)))


-- foldr: try to mimic mutual letrec with computation product types!
--------------------------------------------------------------------------------
-- (s -> Gen (Step s (Up a)))
-- (S ass -> Gen (Step (S ass) (Up a)))
-- P_i (P as_i -> Gen (Step (S ass) (Up a)))      -- Big product!

-- type of fun:
-- (a_i -> b)
-- (a_i -> b)
-- (a_i -> b)

-- let go : Prod i (as_i -> b) =
--       (\as_i -> runGen (fs_i) (pack as_i) >>= \case
--            Stop      -> pure bstart
--            Skip s    -> go s
--            Yield a s -> pure $ f a (call go s)


-- foldrPull :: (Up a -> Up b -> Up b) -> Up b -> Pull (Up a) -> Up b
-- foldrPull f b (Pull @s seed step) = _
