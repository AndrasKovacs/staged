
-- module Evaluation (
--   ($$), ($$$), up, down, eval0, eval1, forceF1, forceF0,
--   forceFU1, forceFU0, app, inlineApp, quote0, quote1, diveCase) where

module Evaluation where

import           IO
import           Common
import qualified Syntax as S
import qualified ElabState as ES
import           Values

--------------------------------------------------------------------------------

var :: Env -> Ix -> Val
var (Snoc _ t)   0 = t
var (Snoc env _) x = var env (x - 1)
var _            _ = impossible

metaIO :: MetaVar -> IO Val
metaIO x = ES.readMeta x >>= \case
  ES.Unsolved{}   -> pure $ Blocked (Unsolved x) Id
  ES.Solved v _ _ -> pure $ Unfold (Solved x) Id v
{-# inline metaIO #-}

meta :: MetaVar -> Val
meta x = runIO $ metaIO x
{-# inline meta #-}

top1 :: Lvl -> Val
top1 x = runIO $ ES.readTop x >>= \case
  ES.TEDef _ _ _ v _ _ _ -> pure $ Unfold (Top1 x) Id v
  _                      -> impossible
{-# inline top1 #-}

infixl 2 $$
-- | Strict closure application.
($$) :: Close S.Tm -> Val -> Val
($$) (Close env t) u = eval1 (Snoc env u) t
{-# inline ($$) #-}

infixl 2 $$$
-- | Lazy closure application.
($$$) :: Close S.Tm -> Val -> Val
($$$) (Close env t) ~u = eval1 (Snoc env u) t
{-# inline ($$$) #-}

app1 :: Val -> Val -> Icit -> Val
app1 t u i = case t of
  Lam x i a t   -> t $$ u
  Blocked h sp  -> Blocked h (App1 sp u i)
  Unfold h sp t -> Unfold h (App1 sp u i) (app1 t u i)
  t             -> App t u i

app1Sp :: Val -> Spine -> Val
app1Sp t sp = case sp of
  Id            -> t
  App1 sp u i   -> app1 (app1Sp t sp) u i
  Field1 sp x n -> field1 (app1Sp t sp) x n

eval :: Env -> S.Tm -> U -> Val
eval env t un = case un of
  U0 _   -> eval0 env t
  U1     -> eval1 env t
  UVar x -> Blocked (Eval env t x) Id

up :: Val -> Val
up (Down t) = t
up t        = Up t

down :: Val -> Val
down (Up t) = t
down t      = Down t

ixFields :: Fields a -> Int -> a
ixFields (FCons x a fs) 0 = a
ixFields (FCons _ _ fs) n = ixFields fs (n - 1)
ixFields _              _ = impossible

field1 :: Val -> Name -> Int -> Val
field1 t x n = case t of
  RecCon ts     -> ixFields ts n
  Blocked h sp  -> Blocked h (Field1 sp x n)
  Unfold h sp t -> Unfold h (Field1 sp x n) (field1 t x n)
  t             -> Field t x n

weakLift :: U -> Val -> Val
weakLift (U0 _)   a = Lift a
weakLift U1       a = a
weakLift (UVar x) a = Blocked (WeakLift a x) Id

weakUp :: U -> Val -> Val
weakUp (U0 _)   t = up t
weakUp U1       t = t
weakUp (UVar x) t = Blocked (WeakUp t x) Id

inserted :: Val -> Env -> S.Locals -> Val
inserted t env ls = case (env, ls) of
  (!env,       S.Empty            ) -> t
  (Snoc env u, S.Define ls _ _ _ _) -> inserted t env ls
  (Snoc env u, S.Bind ls x a au   ) -> app1 (inserted t env ls) (weakUp au u) Expl
  _                                 -> impossible

eval0 :: Env -> S.Tm -> Val
eval0 env = \case
  S.Var x          -> var env x
  S.Top x          -> Top x
  S.Let x a t u    -> Let x (eval1 env a) (eval0 env t) (Close env u)
  S.Lam x Expl a t -> Lam x Expl (eval1 env a) (Close env t)
  S.App t u Expl   -> App (eval0 env t) (eval1 env u) Expl
  S.RecCon fs      -> RecCon (eval0 env <$> fs)
  S.Field t x n    -> Field (eval0 env t) x n
  S.DataCon x n    -> DataCon x n
  S.Case t cs      -> Case (eval0 env t) (Close env cs)
  S.Down t         -> down (eval1 env t)
  _                -> impossible

eval1 :: Env -> S.Tm -> Val
eval1 env = \case
  S.Var x         -> var env x
  S.Top x         -> top1 x
  S.Let x a t u   -> eval1 (Snoc env (eval1 env t)) u
  S.Pi x i a b    -> Pi x i (eval1 env a) (Close env b)
  S.Lam x i a t   -> Lam x i (eval1 env a) (Close env t)
  S.App t u i     -> app1 (eval1 env t) (eval1 env u) i
  S.U un          -> U un
  S.Rec as        -> Rec (eval1 env <$> as)
  S.RecCon ts     -> RecCon (eval1 env <$> ts)
  S.Field t x n   -> field1 (eval1 env t) x n
  S.TyCon x       -> TyCon x
  S.DataCon x n   -> DataCon x n
  S.Case t cs     -> impossible
  S.Lift un a     -> weakLift un (eval1 env a)
  S.Up un t       -> weakUp un (eval1 env t)
  S.Inserted x ls -> runIO do {t <- metaIO x; pure $! inserted t env ls}
  S.Meta x        -> meta x
  _               -> impossible

--------------------------------------------------------------------------------

forceCV :: CV -> CV
forceCV = \case
  CVVar x -> runIO $ ES.readCVMeta x >>= \case
               ES.CVSolved cv -> pure $! forceCV $! cv
               _              -> pure $! CVVar x
  cv -> cv

forceU :: U -> U
forceU = \case
  UVar x -> runIO $ ES.readUMeta x >>= \case
              ES.USolved un -> pure $! forceU $! un
              _             -> pure $! UVar x
  un     -> un

-- | Force Blocked only.
forceB :: Val -> Val
forceB = \case
  Blocked h sp -> forceB' h sp
  v            -> v

forceB' :: BlockedOn -> Spine -> Val
forceB' h sp = case h of
  Unsolved x -> runIO $ ES.readMeta x >>= \case
    ES.Solved v _ _ -> pure $! forceB (app1Sp v sp)
    _               -> pure $! Blocked (Unsolved x) sp
  Eval env t x -> runIO $ ES.readUMeta x >>= \case
    ES.USolved un -> pure $! forceB $! eval env t un
    _             -> pure $! Blocked (Eval env t x) sp
  WeakLift x a -> runIO $ ES.readUMeta x >>= \case
    ES.USolved un -> pure $! forceB $! weakLift un a
    _             -> pure $! Blocked (WeakLift x a) sp
  WeakUp x t -> runIO $ ES.readUMeta x >>= \case
    ES.USolved un -> pure $! forceB $! weakUp un $! forceB t
    _             -> pure $! Blocked (WeakUp x t) sp

-- | Force Blocked and Unfold.
forceBU :: Val -> Val
forceBU = \case
  Blocked h sp  -> forceBU' h sp
  Unfold h sp t -> forceBU t
  v             -> v

forceBU' :: BlockedOn -> Spine -> Val
forceBU' h sp = case h of
  Unsolved x -> runIO $ ES.readMeta x >>= \case
    ES.Solved v _ _ -> pure $! forceBU (app1Sp v sp)
    _               -> pure $! Blocked (Unsolved x) sp
  Eval env t x -> runIO $ ES.readUMeta x >>= \case
    ES.USolved un -> pure $! forceBU $! eval env t un
    _             -> pure $! Blocked (Eval env t x) sp
  WeakLift x a -> runIO $ ES.readUMeta x >>= \case
    ES.USolved un -> pure $! forceBU $! weakLift un a
    _             -> pure $! Blocked (WeakLift x a) sp
  WeakUp x t -> runIO $ ES.readUMeta x >>= \case
    ES.USolved un -> pure $! forceBU $! weakUp un $! forceBU t
    _             -> pure $! Blocked (WeakUp x t) sp

-- Quoting
--------------------------------------------------------------------------------

quoteSp :: Lvl -> Unfolding -> S.Tm -> Spine -> S.Tm
quoteSp l st h = \case
  Id            -> h
  App1 sp u i   -> S.App (quoteSp l st h sp) (quote1 l st u) i
  Field1 sp x n -> S.Field (quoteSp l st h sp) x n

quote1 :: Lvl -> Unfolding -> Val -> S.Tm
quote1 l st t = let
  go0     = quote0  l st; {-# inline go0 #-}
  go1     = quote1  l st; {-# inline go1 #-}
  goSp    = quoteSp l st; {-# inline goSp #-}
  force t = case st of DoUnfold -> forceBU t
                       _        -> forceB t

  goB :: BlockedOn -> S.Tm
  goB = \case
    Unsolved x   -> S.Meta x
    WeakLift x a -> S.Lift (UVar x) (go1 a)
    WeakUp x t   -> S.Up (UVar x) (go

-- data BlockedOn
--   = Unsolved MetaVar           -- unsolved (lvl1) meta
--   | Eval Env S.Tm UMetaVar     -- unknown eval       (at unknown lvl)
--   | WeakLift Ty UMetaVar       -- unknown type lift  (lvl 1)
--   | WeakUp Val UMetaVar        -- unknown term lift  (lvl 1)


  -- in case force t of
  --   Blocked h sp  -> goSp _ sp
  --   Unfold h sp t -> goSp _ sp







quote1 :: Lvl -> Unfolding -> Val -> S.Tm
quote1 = undefined
