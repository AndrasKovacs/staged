
{-# language LambdaCase, TemplateHaskell, BlockArguments, RankNTypes,
    MultiParamTypeClasses, FunctionalDependencies, TypeApplications, TypeFamilyDependencies,
    ScopedTypeVariables, UndecidableInstances, QuantifiedConstraints, DataKinds,
    TypeFamilies, CPP, PartialTypeSignatures, MagicHash, AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures -Wincomplete-patterns #-}

module GenNoCurry4 where

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
import Data.Proxy
import Unsafe.Coerce

#include "Sugar.h"

{-

Object lang:
  - currying
  - let body only value
  - case body only value

how to translate:
  - let-bind immediate beta-redex functions
  - eta-expand function let-defs to arity
  - lambda-lift non-tail-called functions
  - leave only-tail-called functions as join points


Haskell code quality:
  - Haskell kinda does trip up on inner products, so it would
    be better to use SOP instead of just generic sums there.
    For the paper version the generic sum is just simpler though.

Monadic control flow:

  - general monadic local let:
      is fine for Reader/State (no overhead)
      but requires re-packing for Except

  - monadic continuation-join always works without re-packing (Joinable)

  - monadic tail-recursion?
    For example, in Maybe I can write a tail-recursive "find" which can be inlined to return to
    static locations, but can this be generalized to other monads?

  - If a let-bound action is non-tail called from multiple points, it is not possible
    to have fused return (return address is not statically known)

  - there is a difference between let-binding continuation and computation?
    can I somehow do multiple join points to choose from?


Pull streams:

  - Applicative
  - Monoid
  - Not monad
  - Not even Selective, because the a/b in Either a b can't be relayed through
    internal state.

  - What's the best we can do?
  - Can we enrich Pull with some Gen to have let-insertion etc?

  - Mutual letrec can be simulated with nested letrec, but at the cost
    of significant code size increase

      let f = fbody
          g = gbody
          h = hbody

    becomes

      let f =
        let g =
          let h = hbody
          gbody
        let h =
          let g = gbody
          hbody
        fbody

Question:
- Pull streams apparently don't explode on case-bind
- Push streams do!
  Can I add "join points" to push streams?

-}

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
instance (Val a, Val b) => Val (a, b)
instance Val a => Val (Maybe a)
instance (Val a, Val b) => Val (Either a b)
instance Val a => Val [a]

-- printUp :: Up a -> IO ()
-- printUp a = do
--   x <- unType <$> runQ (examineCode a)
--   print $ ppr x

upFun :: Up (a -> b) -> Up a -> Up b
upFun f a = [|| let x = $$a in seq x ($$f x) ||]

downFun :: (Up a -> Up b) -> Up (a -> b)
downFun f = [|| \a -> seq a $$(f [||a||]) ||]

upBool :: Up Bool -> Gen Bool
upBool b = Gen \k -> [|| case $$b of True -> $$(k True); False -> $$(k False) ||]

downBool :: Bool -> Up Bool
downBool True = [||True||]
downBool False = [||False||]

upPair :: Up (Pair a b) -> Gen (Pair (Up a) (Up b))
upPair ab = Gen \k -> [|| case $$ab of Pair a b -> $$(k (Pair [||a||] [||b||])) ||]

downPair :: Up a -> Up b -> Up (Pair a b)
downPair a b = [|| Pair $$a $$b ||]

proj1 (Pair a _) = a; {-# inline proj1 #-}
proj2 (Pair _ b) = b; {-# inline proj2 #-}

--------------------------------------------------------------------------------

newtype Gen a = Gen {unGen :: forall r. Val r => (a -> Up r) -> Up r}

runGen :: forall a. Val a => Gen (Up a) -> Up a
runGen (Gen f) = f id

runGen1 :: forall a b. Val a => Val b => Gen (Up a -> Up b) -> Up (a -> b)
runGen1 (Gen f) = downFun \a -> f \f -> f a

runGen2 :: forall a b c. Val a => Val b => Val c => Gen (Up a -> Up b -> Up c) -> Up (a -> b -> c)
runGen2 (Gen f) = downFun \a -> downFun \b -> f \f -> f a b

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

-- Generic sums
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
index (PCons f fs) (Here x)  = upFun f x
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

caseG :: (MonadGen m, MonadJoin m, IsSumUp c) => Split a b => Up a -> (b -> m c) -> m c
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

instance Split (Pair a b) (Pair (Up a) (Up b)) where
  split x = Gen \k -> [|| case $$x of
    Pair a b -> $$(k (Pair Q(a) Q(b))) ||]

instance Split (Either a b) (Either (Up a) (Up b)) where
  split x = Gen \k -> [|| case $$x of
    Left a  -> $$(k (Left Q(a)))
    Right b -> $$(k (Right Q(b))) ||]

instance Split Bot Bot where
  split x = Gen \_ -> [|| case $$x of ||]

instance Split (Maybe a) (Maybe (Up a)) where
  split x = Gen \k -> [|| case $$x of
    Nothing -> $$(k Nothing)
    Just a  -> $$(k (Just [||a||])) ||]
--------------------------------------------------------------------------------

class MonadGen n => Improve m n | m -> n, n -> m where
  up   :: Val a => Up (m a) -> n (Up a)
  down :: Val a => n (Up a) -> Up (m a)

instance Improve Identity Gen where
  up x = Gen \k -> k Q(runIdentity $$x)
  down x = unGen x \a -> Q(Identity $$a)

instance (Improve m n, Val s) => Improve (StateT s m) (StateT (Up s) n) where

  up x = StateT \s ->
    do as <- up [|| runStateT $$x $$s ||]
       caseG' as pure

  down x = [|| StateT \s -> $$(down
    do (a, s) <- runStateT x Q(s)
       pure Q(($$a, $$s))
       )||]

instance (Improve m n, Val e) => Improve (ExceptT e m) (ExceptT (Up e) n) where
  up x = ExceptT do
    ea <- up Q(runExceptT $$x)
    caseG' ea pure

  down (ExceptT x) =
    [|| ExceptT $$(down (x >>= \case
          Left e  -> pure Q(Left $$e)
          Right a -> pure Q(Right $$a)
                        )) ||]

instance Improve m n => Improve (MaybeT m) (MaybeT n) where
  up x = MaybeT do
    ma <- up Q(runMaybeT $$x)
    caseG' ma pure

  down (MaybeT x) =
    [|| MaybeT $$(down (x >>= \case
          Nothing -> pure Q(Nothing)
          Just a -> pure Q(Just $$a))) ||]

instance Improve m n => Improve (ReaderT r m) (ReaderT (Up r) n) where
  up   x = ReaderT \r -> up Q(runReaderT $$x $$r)
  down x = [|| ReaderT \r -> $$(down (runReaderT x Q(r))) ||]

--------------------------------------------------------------------------------

putG :: forall m s. (MonadGen m, MonadState (Up s) m) => Up s -> m (Up ())
putG s = do
  s <- gen s
  put s
  pure Q(())

modifyG :: (MonadGen m, MonadState (Up s) m) => (Up s -> Up s) -> m (Up ())
modifyG f = do
  s <- get
  putG (f s)

localG :: (MonadGen m, MonadReader (Up r) m) => (Up r -> Up r) -> m a -> m a
localG f ma = do
  r  <- ask
  r' <- gen (f r)
  local (const r') ma

-- replicateM_0 :: Improve m n => Up Int -> m a -> m a
-- replicateM_0 n ma = Imp
--   [|| let go n = _
--       in go $$n ||]

--------------------------------------------------------------------------------

class Applicative f => Selective f where
  branch :: f (Either a b) -> f (a -> c) -> f (b -> c) -> f c

branchDefault :: Monad m => m (Either a b) -> m (a -> c) -> m (b -> c) -> m c
branchDefault m l r = either (\a -> ($ a) <$> l) (\b -> ($ b) <$> r) =<< m

newtype Push a = Push {
  unPush :: forall acc r. (Val acc, Val r) =>
        (a -> (Up acc -> Up r) -> Up acc -> Up r)
        -> (Up acc -> Up r) -> Up acc -> Up r}

foldrPush :: forall a b. Val b => (Up a -> Up b -> Up b) -> Up b -> Push (Up a) -> Up b
foldrPush f b (Push as) = as @() @b (\a b acc -> f a (b acc)) (\_ -> b) Up.tt

foldlPush :: forall a b. Val b => (Up b -> Up a -> Up b) -> Up b -> Push (Up a) -> Up b
foldlPush f b (Push as) = as @b @b (\a b acc -> b (f acc a)) (\acc -> acc) b

-- No elimination directly to a monad!! Damned accumulator.
-- Unrestricted elim is indeed convenient.
foldrMPush :: forall a b m n. Improve m n => Val b =>
                 (Up a -> Up b -> m (Up b)) -> Up b -> Push (Up a) -> Up (n (Up b))
foldrMPush f b (Push as) = undefined

sumPush :: Push (Up Int) -> Up Int
sumPush = foldlPush ((+) :: Up Int -> Up Int -> Up Int) (0 :: Up Int)

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
  liftGen ga = Push \c n acc -> unGen ga \a -> c a n acc

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

instance Selective Push where
  branch = branchDefault

--------------------------------------------------------------------------------

data Step s a = Yield a s | Skip s | Done
  deriving Functor

instance Split (Step s a) (Step (Up s) (Up a)) where
  split x = Gen \k -> [|| case $$x of
    Yield a s -> $$(k (Yield [||a||] [||s||]))
    Skip s    -> $$(k (Skip [||s||]))
    Done      -> $$(k Done) ||]

data Pull a where
  Pull :: forall a s. Up s -> (Up s -> Gen (Step (Up s) a)) -> Pull a

instance Functor Pull where
  fmap f (Pull s step) = Pull s (fmap (fmap (fmap f)) step)

instance Applicative Pull where
  pure a = Pull Up.tt \_ -> pure (Yield a Up.tt)
  Pull s step <*> Pull s' step' =
    Pull (downPair s s') \ss' -> caseG' ss' \(Pair s s') ->
      step s >>= \case
        Done      -> pure Done
        Skip s    -> pure $ Skip $ downPair s s'
        Yield f s -> step' s' >>= \case
          Done       -> pure Done
          Skip s'    -> pure $ Skip $ downPair s s'
          Yield a s' -> pure $ Yield (f a) $ downPair s s'

instance Semigroup (Pull a) where
  Pull (s :: Up s) step <> Pull (s' :: Up s') step' =
   Pull @_ @(Either s s') [||Left $$s||] \s -> caseG' s \case
    Left s -> step s >>= \case
      Done      -> pure $ Skip [||Right $$s'||]
      Skip s    -> pure $ Skip [||Left $$s||]
      Yield a s -> pure $ Yield a [||Left $$s||]
    Right s' -> step' s' >>= \case
      Done       -> pure Done
      Skip s'    -> pure $ Skip [||Right $$s'||]
      Yield a s' -> pure $ Yield a [||Right $$s'||]

instance Monoid (Pull a) where
  mempty = Pull Up.tt \_ -> pure Done

data BindUp a s = Outer | Inner !a !s

instance Split (BindUp a s) (BindUp (Up a) (Up s)) where
  split x = Gen \k -> [|| case $$x of
    Outer    -> $$(k Outer)
    Inner a s -> $$(k (Inner Q(a) Q(s))) ||]

bindGen :: Pull a -> (a -> Gen b) -> Pull b
bindGen (Pull s step) f =
  Pull s \s -> step s >>= \case
    Done -> pure Done
    Skip s -> pure $ Skip s
    Yield a s -> f a >>= \b -> pure $ Yield b s

data SomeType where
  SomeType :: forall (a :: Type). Proxy a -> SomeType

stTy :: Pull a -> SomeType
stTy (Pull (s :: Up s) _) = SomeType (Proxy :: Proxy s)

bindPull :: forall a b. Pull (Up a) -> (Up a -> Pull b) -> Pull b
bindPull (Pull (s :: Up s) step) f = case stTy (f [||undefined||]) of
  SomeType (_ :: Proxy s') ->
    let f' :: Up a -> (Up s', Up s' -> Gen (Step (Up s') b))
        f' a = case f a of Pull s' step' -> (unsafeCoerce s', unsafeCoerce step')
    in
    Pull @b @(Pair s (BindUp a s'))
         (downPair s [||Outer||])
         \s -> caseG' s \(Pair s s') -> caseG' s' \case
           Outer -> step s >>= \case
             Done      -> pure Done
             Skip s    -> pure $ Skip $ downPair s [||Outer||]
             Yield a s -> pure $ Skip $ downPair s [|| Inner $$a $$(fst (f' a)) ||]
           Inner a s' -> snd (f' a) s' >>= \case
             Done       -> pure $ Skip $ downPair s [||Outer||]
             Skip s'    -> pure $ Skip $ downPair s [||Inner $$a $$s' ||]
             Yield b s' -> pure $ Yield b $ downPair s [||Inner $$a $$s' ||]

data IfPull b s s' = Init b | InL s | InR s'

instance Split (IfPull b s s') (IfPull (Up b) (Up s) (Up s')) where
  split x = Gen \k -> [|| case $$x of
    Init b -> $$(k (Init [||b||]))
    InL s  -> $$(k (InL [||s||]))
    InR s' -> $$(k (InR [||s'||])) ||]

instance ToPush (Pull (Up a)) (Up a) where
  push (Pull (s :: Up s) step) = Push \c n (acc :: Up acc) -> [||
    let go (acc :: acc) (s :: s) = $$(runGen do
             step [||s||] >>= \case
               Done      -> pure $ n [||acc||]
               Skip s    -> pure $ [|| go acc $$s ||]
               Yield a s -> pure $ c a (\acc -> [|| go $$acc $$s ||]) [||acc||]
             )
    in go $$acc $$s
   ||]

-- TODO: generic sum version

ifPull :: forall a. Up Bool -> Pull a -> Pull a -> Pull a
ifPull b (Pull (s :: Up s) step) (Pull (s' :: Up s') step') =
  Pull @a @(IfPull Bool s s') [||Init $$b||] \x -> caseG' x \case
    Init b -> caseG' b \case
      True  -> pure $ Skip [|| InL $$s ||]
      False -> pure $ Skip [|| InR $$s' ||]
    InL s  -> step s >>= \case
      Done      -> pure $ Done
      Skip s    -> pure $ Skip [||InL $$s||]
      Yield a s -> pure $ Yield a [||InL $$s||]
    InR s' -> step' s' >>= \case
      Done       -> pure $ Done
      Skip s'    -> pure $ Skip [||InR $$s'||]
      Yield a s' -> pure $ Yield a [||InR $$s'||]

range :: Up Int -> Up Int -> Pull (Up Int)
range x y = Pull (downPair x y) \s -> caseG' s \(Pair x y) ->
  caseG' (x Up.< y) \case
    True  -> pure $ Yield x $ downPair (x + 1) y
    False -> pure Done

single :: Up a -> Pull (Up a)
single a = Pull [||True||] \s -> caseG' s \case -- would be 2 meta-states
  True  -> gen a >>= \a -> pure $ Yield a [||False||]
  False -> pure $ Done
