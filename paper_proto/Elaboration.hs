
module Elaboration (check, infer, inferS) where

import Control.Exception
import Control.Monad

import qualified Data.Map.Strict as M

import Common
import Cxt
import Errors
import Evaluation
import Syntax
import Unification
import Value

import qualified Presyntax as P



-- Implicit insertion
--------------------------------------------------------------------------------

-- | Insert fresh implicit applications.
insert' :: Cxt -> IO (Tm, VTy, Stage) -> IO (Tm, VTy, Stage)
insert' cxt act = go =<< act where
  go (t, va, st) = case force va of
    VPi x Impl a b -> do
      m <- freshMeta cxt a st
      let mv = eval (env cxt) m
      go (App t m Impl, b $$ mv, st)
    va -> pure (t, va, st)

-- | Insert fresh implicit applications to a term which is not
--   an implicit lambda (i.e. neutral).
insert :: Cxt -> IO (Tm, VTy, Stage) -> IO (Tm, VTy, Stage)
insert cxt act = act >>= \case
  (t@(Lam _ Impl _ _), va, st) -> pure (t, va, st)
  (t                 , va, st) -> insert' cxt (pure (t, va, st))

-- | Variant of `insert` which gets the stage from outside.
insertS :: Cxt -> Stage -> IO (Tm, VTy) -> IO (Tm, VTy)
insertS cxt s act = do
  (t, a, s) <- insert cxt (do {(t, a) <- act; pure (t, a, s)})
  pure (t, a)

-- | Insert fresh implicit applications until we hit a Pi with
--   a particular binder name.
insertUntilName :: Cxt -> Name -> IO (Tm, VTy, Stage) -> IO (Tm, VTy, Stage)
insertUntilName cxt name act = go =<< act where
  go :: (Tm, VTy, Stage) -> IO (Tm, VTy, Stage)
  go (t, va, st) = case force va of
    va@(VPi x Impl a b) -> do
      if x == name then
        pure (t, va, st)
      else do
        m <- freshMeta cxt a st
        let mv = eval (env cxt) m
        go (App t m Impl, b $$ mv, st)
    _ ->
      throwIO $ Error cxt $ NoNamedImplicitArg name


-- Subtyping
--------------------------------------------------------------------------------

-- | Try to adjust (t : a) at stage s to (t' : a') at stage s',
--   without doing any subtyping coercion.
adjustStage' :: Cxt -> Tm -> VTy -> Stage -> Stage -> IO (Maybe (Tm, VTy))
adjustStage' cxt t a s s' = case compare s s' of
  EQ ->
    pure Nothing
  LT ->
    pure $ Just (Quote t, VLift a)
  GT -> case force a of
    VLift a ->
      pure $ Just (Splice t, a)
    a -> do
      m <- eval (env cxt) <$> freshMeta cxt (VU S0) S0
      unifyCatch cxt (VLift m) a
      pure $ Just (Splice t, m)

-- | Try to adjust (t : a) at stage s to (t' : a') at stage s',
--   without doing any subtyping coercion.
adjustStage :: Cxt -> Tm -> VTy -> Stage -> Stage -> IO (Tm, VTy)
adjustStage cxt t a s s' = maybe (t, a) id <$> adjustStage' cxt t a s s'

-- | Try to coerce (t : a) at stage s to a' type at stage s'.
coe :: Cxt -> Tm -> VTy -> Stage -> VTy -> Stage -> IO Tm
coe cxt t a s a' s' = maybe t id <$> go cxt t a s a' s' where

  justUnify :: Cxt -> Tm -> VTy -> Stage -> VTy -> Stage -> IO (Maybe Tm)
  justUnify cxt t a s a' s' = adjustStage' cxt t a s s' >>= \case
    Nothing     -> Nothing <$ unifyCatch cxt a a'
    Just (t, a) -> Just t  <$ unifyCatch cxt a a'

  go :: Cxt -> Tm -> VTy -> Stage -> VTy -> Stage -> IO (Maybe Tm)
  go cxt t a s a' s' = case (force a, force a') of

    (VPi x i a b, VPi x' i' a' b') -> do
      when (i /= i') $ throwIO UnifyError
      let cxt' = bind cxt x a' s'
      coev0 <- go cxt' (Var 0) a' s' a s
      case coev0 of
        Nothing -> do
          body <- go cxt' (App (Wk t) (Var 0) i)
                          (b  $$ eval (env cxt') (Var 0)) s
                          (b' $$ VVar (lvl cxt)) s'
          pure $ Lam x i (quote (lvl cxt) a') <$> body
        Just coev0 -> do
          body <- go cxt' (App (Wk t) coev0 i)
                          (b  $$ eval (env cxt') (Var 0)) s
                          (b' $$ VVar (lvl cxt)) s'
          case body of
            Nothing   -> pure $ Just $ Lam x i (quote (lvl cxt) a') (App (Wk t) coev0 i)
            Just body -> pure $ Just $ Lam x i (quote (lvl cxt) a') body

    (VU S0  , VU S1   ) -> pure $ Just $ Lift t
    (VLift a, VLift a') -> Nothing <$ unifyCatch cxt a a'

    (a@VFlex{}, a') -> justUnify cxt t a s a' s'
    (a, a'@VFlex{}) -> justUnify cxt t a s a' s'

    -- coercion avoidance is not perfect here!! We can
    (VLift a, a'      ) -> Just <$> coe cxt (Splice t) a S0 a' s'
    (a      , VLift a') -> Just . Quote <$> coe cxt t a s a' S0

    (a, a') -> justUnify cxt t a s a' s'



-- Check & Infer
--------------------------------------------------------------------------------

checkU :: Cxt -> P.Tm -> Stage -> IO Tm
checkU cxt t s = check cxt t (VU s) s

check :: Cxt -> P.Tm -> VTy -> Stage -> IO Tm
check cxt t a st = case (t, force a) of
  (P.Pos pos t, a) ->
    check (cxt {pos = coerce pos}) t a st

  -- If the icitness of the lambda matches the Pi type, check as usual
  (P.Lam x a i t, VPi x' i' a' b) | either (\x -> x == x' && i' == Impl) (==i') i -> do
    (a, va) <- case a of
      Just a -> do
        a <- checkU cxt a st
        let va = eval (env cxt) a
        unifyCatch cxt va a'
        pure (a, va)
      Nothing ->
        pure (quote (lvl cxt) a', a')

    Lam x i' a <$> check (bind cxt x va st) t (b $$ VVar (lvl cxt)) st

  -- Otherwise if Pi is implicit, insert a new implicit lambda
  (t, VPi x Impl a b) -> do
    Lam x Impl (quote (lvl cxt) a)
      <$> check (newBinder cxt x a st) t (b $$ VVar (lvl cxt)) st

  (P.Lift a, VU S1) -> do
    Lift <$> checkU cxt a S0

  (P.Pi x i a b, VU st) -> do
    a <- checkU cxt a st
    let ~va = eval (env cxt) a
    b <- checkU (bind cxt x va st) b st
    pure $ Pi x i a b

  (P.Quote t, VLift a) -> do
    Quote <$> check cxt t a S0

  -- (t, VLift a) -> do
  --   Quote <$> check cxt t a S0

  (P.Let st' x a t u, a') | st == st' -> do
    (a, va, t, vt) <- case a of
      Nothing -> do
        (t, a) <- inferS cxt t st
        pure (quote (lvl cxt) a, a, t, eval (env cxt) t)
      Just a -> do
        a <- checkU cxt a st
        let ~va = eval (env cxt) a
        t <- check cxt t va st
        pure (a, va, t, eval (env cxt) t)
    u <- check (define cxt x t vt a va st) u a' st
    pure (Let st' x a t u)

  (P.Hole, a) ->
    freshMeta cxt a st

  (t, a) -> do
    (t, ta, ts) <- insert cxt $ infer cxt t
    coe cxt t ta ts a st

inferU :: Cxt -> P.Tm -> IO (Tm, Stage)
inferU cxt t = do
  (t, a, s) <- infer cxt t
  unifyCatch cxt a (VU s)
  pure (t, s)

inferS :: Cxt -> P.Tm -> Stage -> IO (Tm, VTy)
inferS cxt t s = case t of
  P.Pos pos t ->
    inferS (cxt {pos = coerce pos}) t s

  -- P.Lam x ann (Right i) t -> do
  --   a <- case ann of
  --     Nothing -> freshMeta cxt (VU s) s
  --     Just a -> checkU cxt a s
  --   let ~va = eval (env cxt) a
  --   (t, b) <- inferS (bind cxt x va s) t s
  --   pure (Lam x i a t, VPi x i va (closeVal cxt b))

  P.Lam x Nothing (Right i) t -> do
    a <- freshMeta cxt (VU s) s
    let ~va = eval (env cxt) a
    (t, b) <- inferS (bind cxt x va s) t s
    pure (Lam x i a t, VPi x i va (closeVal cxt b))

  P.Lam x a Left{} t ->
    throwIO $ Error cxt $ InferNamedLam

  P.Hole -> do
    a <- eval (env cxt) <$> freshMeta cxt (VU s) s
    t <- freshMeta cxt a s
    pure (t, a)

  t -> do
    (t, a, s') <- infer cxt t
    adjustStage cxt t a s' s

infer :: Cxt -> P.Tm -> IO (Tm, VTy, Stage)
infer cxt = \case
  P.Pos pos t ->
    infer (cxt {pos = coerce pos}) t

  P.Var x ->
    case M.lookup x (srcNames cxt) of
      Just (x', a, st) -> pure (Var (lvl2Ix (lvl cxt) x'), a, st)
      Nothing          -> throwIO $ Error cxt $ NameNotInScope x

  P.Lam x (Just a) (Right i) t -> do
    (a, as) <- inferU cxt a
    let ~va = eval (env cxt) a
    let cxt' = bind cxt x va as
    (t, b) <- insertS cxt' as $ inferS cxt' t as
    pure (Lam x i a t, VPi x i va (closeVal cxt b), as)

  P.Lam x Nothing Right{} _ -> do
    throwIO $ Error cxt $ InferStage

  P.Lam x a Left{} t ->
    throwIO $ Error cxt $ InferNamedLam

  P.App t u i -> do

    -- choose implicit insertion
    (i, t, tty, st) <- case i of
      Left name -> do
        (t, tty, ts) <- insertUntilName cxt name $ infer cxt t
        pure (Impl, t, tty, ts)
      Right Impl -> do
        (t, tty, ts) <- infer cxt t
        pure (Impl, t, tty, ts)
      Right Expl -> do
        (t, tty, ts) <- insert' cxt $ infer cxt t
        pure (Expl, t, tty, ts)

    -- ensure that t has a Pi type
    (t, a, b) <- case force tty of
      VPi x i' a b -> do
        unless (i == i') $
          throwIO $ Error cxt $ IcitMismatch i i'
        pure (t, a, b)
      tty -> do
        a <- eval (env cxt) <$> freshMeta cxt (VU st) st
        b <- Closure (env cxt) <$> freshMeta (bind cxt "x" a st) (VU st) st
        t <- coe cxt t tty st (VPi "x" i a b) st
        pure (t, a, b)

    u <- check cxt u a st
    pure (App t u i, b $$ eval (env cxt) u, st)

  P.U s ->
    pure (U s, VU s, s)

  P.Pi x i a b -> do
    (a, s) <- inferU cxt a
    b <- checkU (bind cxt x (eval (env cxt) a) s) b s
    pure (Pi x i a b, VU s, s)

  P.Let st x a t u -> do
    (a, va, t, vt) <- case a of
      Nothing -> do
        (t, a) <- inferS cxt t st
        pure (quote (lvl cxt) a, a, t, eval (env cxt) t)
      Just a -> do
        a <- checkU cxt a st
        let ~va = eval (env cxt) a
        t <- check cxt t va st
        pure (a, va, t, eval (env cxt) t)
    (u, b) <- inferS (define cxt x t vt a va st) u st
    pure (Let st x a t u, b, st)

  P.Hole -> do
    throwIO $ Error cxt InferStage

  P.Lift a -> do
    a <- checkU cxt a S0
    pure (Lift a, VU S1, S1)

  P.Quote t -> do
    (t, a) <- inferS cxt t S0
    pure (Quote t, VLift a, S1)

  P.Splice t -> do
    (t, a) <- inferS cxt t S1
    (t, a) <- adjustStage cxt t a S1 S0
    pure (t, a, S0)
