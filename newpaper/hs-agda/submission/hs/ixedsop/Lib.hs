
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

data Nat = Zero | Suc Nat

data instance Sing (n :: Nat) where
  SZero :: Sing 'Zero
  SSuc  :: Sing n -> Sing ('Suc n)

instance SingI Zero               where sing = SZero
instance SingI n => SingI (Suc n) where sing = SSuc sing

data Vec (a :: Type) :: Nat -> Type where
  VNil  :: Vec a Zero
  VCons :: a -> Vec a n -> Vec a (Suc n)

data instance Sing (v :: Vec a n) where
  SVNil :: Sing VNil
  SVCons :: Sing (x :: a) -> Sing (v :: Vec a n) -> Sing (VCons x v)

instance SingI VNil where
  sing = SVNil
instance (SingI (x :: a), SingI (v :: Vec a n)) => SingI (VCons x v) where
  sing = SVCons sing sing

data Fin (n :: Nat) where
  FZero :: Fin (Suc n)
  FSuc  :: Fin n -> Fin (Suc n)

type Ix :: forall (a :: Type)(n :: Nat)(as :: Vec a n)(i :: Fin n) -> a
type family Ix a n as i where
  Ix a (Suc n) (VCons x xs) FZero    = x
  Ix a (Suc n) (VCons x xs) (FSuc i) = Ix a n xs i

data instance Sing (i :: Fin n) where
  SFZero :: Sing FZero
  SFSuc  :: Sing (i :: Fin n) -> Sing (FSuc i)

instance SingI FZero where sing = SFZero
instance SingI (i :: Fin n) => SingI (FSuc i) where sing = SFSuc sing

data S' (v :: Vec [Type] n) where
  S' :: Sing (i :: Fin n) -> P (Ix [Type] n v i) -> S' v

data BindS (ass :: Vec [Type] n) (bss :: Vec [Type] n) where
  BindS :: Sing (i :: Fin n) -> P (Ix [Type] n ass i) -> P (Ix [Type] n bss i) -> BindS ass bss

instance IsSOP (BindS ass bss) where

data BindStuff n ass b where
  BindStuff :: forall n (ass :: Vec [Type] n) (bss :: Vec [Type] n) b.
       (forall (i :: Fin n). Sing i -> P (Ix [Type] n ass i) -> P (Ix [Type] n bss i))
    -> (forall (i :: Fin n). Sing i -> P (Ix [Type] n ass i) -> P (Ix [Type] n bss i)
         -> Gen (Step (P (Ix [Type] n bss i)) b))
    -> BindStuff n ass b

undefinedP :: Sing as -> P as
undefinedP SNil         = Nil
undefinedP (SCons a as) = Cons [||undefined||] (undefinedP as)

makeBindStuff :: forall n (ass :: Vec [Type] n) b. Sing ass -> (S' ass -> Pull b) -> BindStuff n ass b
makeBindStuff SVNil f =
  BindStuff @Zero @VNil @VNil (\i -> case i of) (\i -> case i of)
makeBindStuff (SVCons as ass) f = case makeBindStuff ass (\(S' i a) -> f (S' (SFSuc i) a)) of
  BindStuff @_ @_ @bss @_ seed step -> case f (S' SFZero (undefinedP undefined)) of
    Pull @s _ _ ->  _
      -- BindStuff @_ @_ @(VCons (Rep s)

-- goBind :: forall n (ass :: Vec [Type] n) (bss :: Vec [Type] n) b.
--      Pull (S' ass)
--   -> (forall (i :: Fin n). Sing i -> P (Ix [Type] n ass i) -> P (Ix [Type] n bss i))
--   -> (forall (i :: Fin n). Sing i -> P (Ix [Type] n ass i) -> P (Ix [Type] n bss i)
--        -> Gen (Step (P (Ix [Type] n bss i)) b))
--   -> Pull b
-- goBind (Pull @s seed step) seed' step' =
--   Pull @(s, Maybe (BindS ass bss)) (seed, Nothing) \(s, s') -> case s' of
--     Nothing -> step s >>= \case
--       Stop             -> pure Stop
--       Skip s           -> pure $ Skip (s, Nothing)
--       Yield (S' n a) s -> pure $ Skip (s, Just (BindS n a (seed' n a)))
--     Just (BindS n a s') -> step' n a s' >>= \case
--       Stop       -> pure $ Skip (s, Nothing)
--       Skip s'    -> pure $ Skip (s, Just (BindS n a s'))
--       Yield b s' -> pure $ Yield b $ (s, Just (BindS n a s'))

-- goBind :: forall ass bss b.
--         Pull (S' ass) -> (forall n. Sing n -> Ix n ass -> Ix n bss)
--                       -> (forall n. Sing n -> Ix n ass -> Ix n bss -> Gen (Step (Ix n bss) b))
--                       -> Pull b
-- goBind (Pull @s seed step) seed' step' =
--   Pull @(s, Maybe (BindS ass bss)) (seed, Nothing) \(s, s') -> case s' of
--     Nothing -> step s >>= \case
--       Stop             -> pure Stop
--       Skip s           -> pure $ Skip (s, Nothing)
--       Yield (S' n a) s -> pure $ Skip (s, Just (BindS n a (seed' n a)))
--     Just (BindS n a s') -> step' n a s' >>= \case
--       Stop       -> pure $ Skip (s, Nothing)
--       Skip s'    -> pure $ Skip (s, Just (BindS n a s'))
--       Yield b s' -> pure $ Yield b $ (s, Just (BindS n a s'))








-- type family Ix (xs :: [[Type]]) (n :: Fin (Len xs)) :: Type where
--   Ix (as ': ass) FZero = P as

-- type family Ix (n :: Nat) (xs :: [[Type]]) :: Type where
--   Ix Zero    (as ': ass) = P as
--   Ix (Suc n) (as ': ass) = Ix n ass

-- data S' (ass :: [[Type]]) where
--   S' :: forall n ass. Sing n -> Ix n ass -> S' ass


-- data BindS ass bss where
--   BindS :: forall ass bss n. Sing n -> Ix n ass -> Ix n bss -> BindS ass bss

-- instance IsSOP (BindS ass bss) where


-- data BindStuff ass b where
--   BindStuff :: forall ass bss b.
--                           (forall n. Sing n -> Ix n ass -> Ix n bss)
--                        -> (forall n. Sing n -> Ix n ass -> Ix n bss -> Gen (Step (Ix n bss) b))
--                        -> BindStuff ass b

-- stuff :: Sing ass -> (S' ass -> Pull b) -> BindStuff ass b
-- stuff SNil           f =
--   BindStuff @'[] @'[] (\n a -> undefined) (\n a s' -> undefined)
-- stuff (SCons as ass) f = case stuff ass ( of
--   foo -> _

-- goBind :: forall ass bss b.
--         Pull (S' ass) -> (forall n. Sing n -> Ix n ass -> Ix n bss)
--                       -> (forall n. Sing n -> Ix n ass -> Ix n bss -> Gen (Step (Ix n bss) b))
--                       -> Pull b
-- goBind (Pull @s seed step) seed' step' =
--   Pull @(s, Maybe (BindS ass bss)) (seed, Nothing) \(s, s') -> case s' of
--     Nothing -> step s >>= \case
--       Stop             -> pure Stop
--       Skip s           -> pure $ Skip (s, Nothing)
--       Yield (S' n a) s -> pure $ Skip (s, Just (BindS n a (seed' n a)))
--     Just (BindS n a s') -> step' n a s' >>= \case
--       Stop       -> pure $ Skip (s, Nothing)
--       Skip s'    -> pure $ Skip (s, Just (BindS n a s'))
--       Yield b s' -> pure $ Yield b $ (s, Just (BindS n a s'))


-- bind :: Pull (

-- data Pull' as b where
--   Pull' :: forall s as b. IsSOP s => (P as -> s) -> (P as -> s -> Gen (Step s b)) -> Pull' as b

-- undefinedP :: Sing as -> P as
-- undefinedP SNil         = Nil
-- undefinedP (SCons a as) = Cons [||undefined||] (undefinedP as)

-- untangleP :: Sing as -> (P as -> Pull b) -> Pull' as b
-- untangleP as f = case f (undefinedP as) of
--   Pull @s _ _ -> Pull' @s (\as -> case f as of Pull seed _ -> unsafeCoerce# seed)
--                           (\as -> case f as of Pull _ step -> unsafeCoerce# step)

-- type family Untangle (ass :: [[Type]]) (b :: Type) :: Type where
--   Untangle '[]         b = ()
--   Untangle (as ': ass) b = (Pull' as b, Untangle ass b)

-- untangle :: Sing ass -> (S ass -> Pull b) -> Untangle ass b
-- untangle SNil           f = ()
-- untangle (SCons as ass) f = (untangleP as (f . Here), untangle ass (f . There))

-- (S ass -> Pull b)

-- makeSigma :: forall ass b. Sing ass -> (S ass -> Pull b) -> MakeSigma ass b
-- makeSigma = _

-- -> Sum (0 to i) (ass[i], s'[i])

-- From (Sing ass) (f :: S ass -> Pull b)

-- Compute an existential type (Sigma a f1)
-- Such






-- bind' :: forall ass b. SingI ass => Pull (S ass) -> (S ass -> Pull b) -> Pull b
-- bind' (Pull @s seed step) f =
--   let f' = untangle sing f in
--   Pull @(s, Maybe (S ass, ())) _ _


-- bind :: forall a b. IsSOP a => Pull a -> (a -> Pull b) -> Pull b
-- bind as f =
--   let f' = untangle (singRep @a) (f . decode)
--   in _

-- bundle :: (Up a -> Pull b) -> Pull' a b
-- bundle f = case f [||undefined||] of
--   Pull @s seed step ->
--     Pull' @s (\a -> case f a of Pull seed _ -> unsafeCoerce# seed)
--              (\a -> case f a of Pull _ step -> unsafeCoerce# step)

-- bind :: forall a b. Pull (Up a) -> (Up a -> Pull b) -> Pull b
-- bind (Pull @s seed step) (bundle -> Pull' @s' seed' step') =
--   Pull @(s, Maybe (Up a, s')) (seed, Nothing) \case
--     (s, Nothing) -> step s <&> \case
--       Stop       -> Stop
--       Skip s     -> Skip (s, Nothing)
--       Yield a s  -> Skip (s, Just (a, seed' a))
--     (s, Just (a, s')) -> step' a s' <&> \case
--       Stop       -> Skip (s, Nothing)
--       Skip s'    -> Skip (s, Just (a, s'))
--       Yield b s' -> Yield b (s, Just (a, s'))

-- bind :: forall ass b. Pull (S ass) -> (S ass -> Pull b) -> Pull b
-- bind = _



-- eitherPull :: Up (Either a b) -> (Up a -> Pull c) -> (Up b -> Pull c) -> Pull c
-- eitherPull ab (bundle -> Pull' @s seed step) (bundle -> Pull' @s' seed' step') =
--   Pull

-- foldrPull :: (Up a -> Up b -> Up b) -> Up b -> Pull (Up a) -> Up b
-- foldrPull f b (Pull seed step) = _
