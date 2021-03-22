
module Evaluation (ca1, ca1', ca0', up, down, eval0, eval1) where

import           IO
import           Common
import qualified Syntax as S
import qualified ElabState as ES
import           Values

--------------------------------------------------------------------------------

var :: Env -> Ix -> Val
var (Snoc _ v)   0 = v
var (Snoc env _) x = var env (x - 1)
var _ _            = impossible

meta :: MetaVar -> Val
meta x = runIO $ ES.readMeta x >>= \case
  ES.MEUnsolved{}    -> pure $ Flex x SNil
  ES.MESolved v  _ _ -> pure $ Unfold (UHMeta x) SNil v
{-# inline meta #-}

top :: Lvl -> Val
top x = runIO $ ES.readTop x >>= \case
  ES.TEDef v _ _ _ _ _ _ -> pure $ Unfold (UHTop x) SNil v
  _                      -> impossible
{-# inline top #-}

-- | Strict closure application.
ca1' :: Close S.Tm -> Val -> Val
ca1' (Close env t) u = eval1 (Snoc env u) t
{-# inline ca1' #-}

-- | Lazy closure application.
ca1 :: Close S.Tm -> Val -> Val
ca1 (Close env t) ~u = eval1 (Snoc env u) t
{-# inline ca1 #-}

-- | Strict closure application.
ca0' :: Close S.Tm -> Val -> Val
ca0' (Close env t) u = eval0 (Snoc env u) t
{-# inline ca0' #-}

app :: Val -> Val -> Icit -> Val
app t u i = case t of
  Lam x i a t     -> ca1' t u
  Rigid h sp      -> Rigid h (SApp sp u i)
  Flex h sp       -> Flex h (SApp sp u i)
  Unfold h sp t   -> Unfold h (SApp sp u i) (app t u i)
  _               -> impossible

down :: Val -> Val
down = \case Up t -> t; t -> Down t;
{-# inline down #-}

up :: Val -> Val
up = \case Down t -> t; t -> Up t;
{-# inline up #-}

appSp :: Val -> Spine -> Val
appSp v = \case
  SNil        -> v
  SApp sp u i -> app (appSp v sp) u i

-- | Force flex
forceF :: Val -> Val
forceF = \case
  Flex x sp -> runIO $ ES.readMeta x >>= \case
                 ES.MESolved v _ _ -> pure $! forceF (appSp v sp)
                 _                 -> pure $! Flex x sp
  v         -> v

-- | Force flex and unfolding
forceFU :: Val -> Val
forceFU = \case
  Flex x sp     -> runIO $ ES.readMeta x >>= \case
                     ES.MESolved v _ _ -> pure $! forceFU (appSp v sp)
                     _                 -> pure $! Flex x sp
  Unfold h sp t -> forceFU t
  v             -> v

evalRecCon :: Env -> [(Name, S.Tm)] -> [(Name, Val)]
evalRecCon env [] = []
evalRecCon env ((x, t):ts) =
  let t'  = eval0 env t
      ts' = evalRecCon env ts
  in (x, t'):ts'

eval0 :: Env -> S.Tm -> Val
eval0 env t = let
  go  = eval0 env; {-# inline go #-}
  go1 = eval1 env; {-# inline go1 #-}
  in case t of
    S.Var x       -> var env x
    S.Top x       -> Top x
    S.Let x a t u -> Let x (go1 a) (go t) (Close env u)
    S.DataCon x i -> DataCon x i
    S.Lam x i a t -> Lam x i (go1 a) (Close env t)
    S.App t u i   -> App (go t) (go u) i
    S.RecCon ts   -> RecCon (evalRecCon env ts)
    S.Field t x n -> Field (go t) x n
    S.Case t ts   -> Case (go t) (Close env ts)
    S.Down t      -> down (go1 t)
    _             -> impossible

evalRec :: Env -> [(Name, S.Tm)] -> [(Name, Val)]
evalRec env [] = []
evalRec env ((x, t):ts) =
  let t'  = eval1 env t
      ts' = evalRec env ts
  in (x, t'):ts'

inserted :: Val -> Env -> S.Locals -> Val
inserted t env          S.Empty             = t
inserted t (Snoc env u) (S.Define ls _ _ _) = inserted t env ls
inserted t (Snoc env u) (S.Bind ls x a   )  = app (inserted t env ls) u Expl
inserted _ _   _                            = impossible

eval1 :: Env -> S.Tm -> Val
eval1 env t = let
  go = eval1 env; {-# inline go #-}
  in case t of
    S.Var x         -> var env x
    S.Top x         -> top x
    S.Meta x        -> meta x
    S.Inserted x ls -> inserted (meta x) env ls
    S.Let x a t u   -> eval1 (Snoc env (go t)) u
    S.DataCon x i   -> Rigid (RHDataCon x i) SNil
    S.Lam x i a t   -> Lam x i (go a) (Close env t)
    S.App t u i     -> app (go t) (go u) i
    S.Fun a b       -> Fun (go a) (go b)
    S.Pi x i a b    -> Pi x i (go a) (Close env t)
    S.Ty u          -> Ty u
    S.Up t          -> up (eval0 env t)
    S.Lift a        -> Lift (go a)
    S.Rec fs        -> Rec (evalRec env fs)
    S.TyCon x       -> Rigid (RHTyCon x) SNil
    _               -> impossible
