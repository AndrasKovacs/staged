
module CFTT.Pull where

import Data.Typeable

import CFTT.Gen
import CFTT.SOP
import CFTT.Split
import CFTT.Join
import CFTT.Up

--------------------------------------------------------------------------------

impossible :: a
impossible = error "impossible"

data Step s a = Stop | Yield a s | Skip s deriving Functor

data Pull a where
  Pull :: forall s a. (IsSOP s, Typeable s) => Bool -> Gen s -> (s -> Gen (Step s a)) -> Pull a

--------------------------------------------------------------------------------

infixl 1 <&>
(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip fmap

repeat :: a -> Pull a
repeat a = Pull False (pure ()) \_ -> pure $ Yield a ()

instance Functor Pull where
  fmap f (Pull skips seed step) = Pull skips seed (fmap (fmap (fmap f)) step)

infixl 4 <:>
(<:>) :: ∀ a b. (IsSOP a, Typeable a) => Pull (a → b) → Pull a → Pull b
(<:>) (Pull @s False seed step) (Pull @s' False seed' step') =
  Pull False ((,) <$> seed <*> seed') \(s, s') -> step s >>= \case
    Stop      -> pure Stop
    Skip _    -> impossible
    Yield f s -> step' s' >>= \case
      Stop       -> pure Stop
      Skip _     -> impossible
      Yield a s' -> pure $ Yield (f a) (s, s')
(<:>) (Pull @s True seed step) (Pull @s' False seed' step') =
  Pull True ((,) <$> seed <*> seed') \(s, s') ->
    step s >>= \case
      Stop        → pure Stop
      (Skip s)    → pure $ Skip (s , s')
      (Yield f s) → step' s' >>= \case
        Stop         → pure Stop
        (Skip s')    → impossible
        (Yield a s') → pure $ Yield (f a) (s , s')
(<:>) (Pull @s False seed step) (Pull @s' True seed' step') =
  Pull True ((,) <$> seed <*> seed') \case
    (s, s') → step' s' >>= \case
      Stop         → pure Stop
      (Skip s')    → impossible
      (Yield a s') → step s >>= \case
        Stop        → pure Stop
        (Skip s)    → pure $ Skip (s, s')
        (Yield f s) → pure $ Yield (f a) (s , s')
(<:>) (Pull @s True seed step) (Pull @s' True seed' step') =
  Pull True ((,,) <$> seed <*> seed' <*> pure Nothing) \case
    (s , s' , Just a)  → step s >>= \case
      Stop        → pure Stop
      (Skip s)    → pure $ Skip (s , s' , Just a)
      (Yield f s) → pure $ Yield (f a) (s , s' , Nothing)
    (s , s' , nothing) → step' s' >>= \case
      Stop         → pure Stop
      (Skip s')    → pure $ Skip (s , s' , Nothing)
      (Yield a s') → pure $ Skip (s , s' , Just a)

--------------------------------------------------------------------------------

-- optimized definition for prepending an element
cons :: ∀ a. IsSOP a => a -> Pull a -> Pull a
cons a (Pull @s skips seed step) =
  Pull @(Maybe s) skips (pure Nothing) \case
    Nothing  → Yield a . Just <$> seed
    (Just s) → step s >>= \case
      Stop        → pure Stop
      (Skip s)    → pure $ Skip (Just s)
      (Yield a s) → pure $ Yield a (Just s)

instance Semigroup (Pull a) where
  Pull @s _ seed step <> Pull @s' _ seed' step' =
    Pull @(Either s s') True (Left <$> seed) \case
      Left s   -> step s >>= \case
        Stop       -> Skip . Right <$> seed'
        Skip s     -> pure $ Skip $ Left s
        Yield a s  -> pure $ Yield a $ Left s
      Right s' -> step' s' <&> \case
        Stop       -> Stop
        Skip s'    -> Skip $ Right s'
        Yield a s' -> Yield a $ Right s'

instance Monoid (Pull a) where
  mempty = Pull False (pure ()) \_ -> pure Stop

--------------------------------------------------------------------------------

single :: a -> Pull a
single a = Pull False (pure True) $ \case
  True  -> pure $ Yield a False
  False -> pure Stop

undefinedP :: Sing a -> Elₚ a
undefinedP SNil         = Nil
undefinedP (SCons a as) = Cons [||undefined||] (undefinedP as)

maybeCast :: forall a b. Typeable a => Typeable b => a -> b
maybeCast x = maybe (error "generativity violation") id (cast @a @b x)

data Pull' as b where
  Pull' :: forall s as b. (IsSOP s, Typeable s) => Bool -> (Elₚ as -> Gen s) -> (Elₚ as -> s -> Gen (Step s b)) -> Pull' as b

unravel :: Typeable b => Sing a -> (Elₚ a -> Pull b) -> Pull' a b
unravel a f = case f (undefinedP a) of
  Pull @s skip seed step ->
    Pull' @s skip
             (\xs -> case f xs of Pull @s' _ seed _ -> maybeCast seed)
             (\xs -> case f xs of Pull @s' _ _ step -> maybeCast step)

singleP :: Up a -> Elₚ '[ a ]
singleP a = Cons a Nil

unSingleP :: Elₚ '[ a ] -> Up a
unSingleP (Cons a Nil) = a

unravel1 :: Typeable b => (Up a -> Pull b) -> Pull' '[ a ] b
unravel1 f = unravel sing (f . unSingleP)

unravel2 :: Typeable c => ((Up a, Up b) -> Pull c) -> Pull' '[a, b] c
unravel2 f = unravel sing (\(Cons a (Cons b Nil)) -> f (a, b))

forEach :: forall a b. (Typeable a, Typeable b) => Pull (Up a) -> (Up a -> Pull b) -> Pull b
forEach (Pull @s skip seed step) (unravel1 -> Pull' @s' skip' seed' step') =
  Pull @(s, Maybe (Up a, s')) (skip || skip') ((,Nothing) <$> seed) \case
    (s, Nothing) -> step s >>= \case
      Stop       -> pure Stop
      Skip s     -> pure $ Skip (s, Nothing)
      Yield a s  -> do {s' <- seed' (singleP a); pure $ Skip (s, Just (a, s'))}
    (s, Just (a, s')) -> step' (singleP a) s' <&> \case
      Stop       -> Skip (s, Nothing)
      Skip s'    -> Skip (s, Just (a, s'))
      Yield b s' -> Yield b (s, Just (a, s'))

--------------------------------------------------------------------------------

class CasePull a b | a -> b, b -> a where
  casePull :: Typeable c => Up a -> (b -> Pull c) -> Pull c

genPull :: forall a b. (Typeable a, Typeable b) => Up a -> (Up a -> Pull b) -> Pull b
genPull a (unravel1 -> Pull' @s skip seed step) =
  Pull @(Up a, s) skip ((a,) <$> seed (Cons a Nil)) \(a, s) -> step (singleP a) s >>= \case
      Stop       -> pure Stop
      Skip s     -> pure $ Skip (a, s)
      Yield b s  -> pure $ Yield b (a, s)

instance CasePull Bool Bool where
  casePull b f = case (f True, f False) of
    (Pull @s st seed step, Pull @s' st' seed' step') ->
      Pull @(Either s s') (st || st')
        (caseM b \case True  -> Left  <$> seed
                       False -> Right <$> seed')
        \case
        Left s   -> step s >>= \case
          Stop      -> pure Stop
          Skip s    -> pure $ Skip $ Left s
          Yield c s -> pure $ Yield c $ Left s
        Right s' -> step' s' >>= \case
          Stop      -> pure Stop
          Skip s    -> pure $ Skip $ Right s
          Yield c s -> pure $ Yield c $ Right s

instance (Typeable a, Typeable b) => CasePull (Either a b) (Either (Up a) (Up b)) where
  casePull x f = case (unravel1 (f . Left), unravel1 (f . Right)) of
    (Pull' @s skip seed step, Pull' @s' skip' seed' step') ->
      Pull @(Either (Up a, s) (Up b, s')) (skip || skip')
           (caseM x \case Left a  -> do {s <- seed  (singleP a); pure $ Left (a, s)}
                          Right b -> do {s <- seed' (singleP b); pure $ Right (b, s)})
           \case
        (Left (a, s)) -> step (singleP a) s >>= \case
          Stop      -> pure Stop
          Skip s    -> pure $ Skip ( (Left (a, s)))
          Yield c s -> pure $ Yield c ( (Left (a, s)))
        (Right (b, s)) -> step' (singleP b) s >>= \case
          Stop      -> pure Stop
          Skip s    -> pure $ Skip ( (Right (b, s)))
          Yield c s -> pure $ Yield c ( (Right (b, s)))

instance Typeable a => CasePull (Maybe a) (Maybe (Up a)) where
  casePull x f = case (f Nothing, unravel1 (f . Just)) of
    (Pull @s skip seed step, Pull' @s' skip' seed' step') ->
      Pull @( (Either s (Up a, s'))) (skip || skip')
           (caseM x \case Nothing -> Left <$> seed
                          Just a  -> Right . (a,) <$> seed' (singleP a))
        \case
        (Left s) -> step s >>= \case
          Stop      -> pure Stop
          Skip s    -> pure $ Skip ( (Left s))
          Yield c s -> pure $ Yield c ( (Left s))
        (Right (a, s)) -> step' (singleP a) s >>= \case
          Stop      -> pure Stop
          Skip s    -> pure $ Skip ( (Right (a, s)))
          Yield c s -> pure $ Yield c ( (Right (a, s)))

instance Typeable a => CasePull [a] (Maybe (Up a, Up [a])) where
  casePull x f = case (f Nothing, unravel2 (f . Just)) of
    (Pull @s skip seed step, Pull' @s' skip' seed' step') ->
      Pull @( (Either s (Up a, Up [a], s'))) (skip || skip')
           (caseM x \case Nothing -> Left <$> seed
                          Just (a, as) -> Right . (a,as,) <$> seed' (Cons a (Cons as Nil)))
         \case
         (Left s) -> step s >>= \case
          Stop      -> pure Stop
          Skip s    -> pure $ Skip ( (Left s))
          Yield c s -> pure $ Yield c ( (Left s))
         (Right (a, as, s)) -> step' (Cons a (Cons as Nil)) s >>= \case
          Stop      -> pure Stop
          Skip s    -> pure $ Skip ( (Right (a, as, s)))
          Yield c s -> pure $ Yield c ( (Right (a, as, s)))
