
module Evaluation (
  ($$), ($$$), up, down, eval0, eval1, forceF1, forceF0,
  forceFU1, forceFU0, app, inlineApp, quote0, quote1, diveCase) where

import           IO
import           Common
import qualified Syntax as S
import qualified ElabState as ES
import           Values

--------------------------------------------------------------------------------

var1 :: Env -> Ix -> Val1
var1 (Snoc1 _ v)   0 = v
var1 (Snoc1 env _) x = var1 env (x - 1)
var1 (Snoc0 env _) x = var1 env (x - 1)
var1 _ _             = impossible

var0 :: Env -> Ix -> Lvl
var0 (Snoc0 _ v)   0 = v
var0 (Snoc0 env _) x = var0 env (x - 0)
var0 (Snoc1 env _) x = var0 env (x - 0)
var0 _ _             = impossible

metaIO :: MetaVar -> IO Val1
metaIO x = ES.readMeta x >>= \case
  ES.MEUnsolved{}  -> pure $ Flex x SNil
  ES.MESolved v  _ -> pure $ Unfold (UHMeta x) SNil v
{-# inline metaIO #-}

meta :: MetaVar -> Val1
meta x = runIO $ metaIO x
{-# inline meta #-}

top1 :: Lvl -> Val1
top1 x = runIO $ ES.readTop x >>= \case
  ES.TEDef1 _ _ v _ _ _ -> pure $ Unfold (UHTop x) SNil v
  _                     -> impossible
{-# inline top1 #-}

infixl 2 $$
-- | Strict closure application.
($$) :: Close S.Tm1 -> Val1 -> Val1
($$) (Close env t) u = eval1 (Snoc1 env u) t
{-# inline ($$) #-}

infixl 2 $$$
-- | Lazy closure application.
($$$) :: Close S.Tm1 -> Val1 -> Val1
($$$) (Close env t) ~u = eval1 (Snoc1 env u) t
{-# inline ($$$) #-}

-- | Going under a level 0 closure
dive :: Close S.Tm0 -> Lvl -> Val0
dive (Close env t) l = eval0 (Snoc0 env l) t
{-# inline dive #-}

app :: Val1 -> Val1 -> Icit -> Val1
app t u i = case t of
  Lam1 x i a t    -> t $$ u
  Rigid h sp      -> Rigid h (SApp sp u i)
  Flex h sp       -> Flex h (SApp sp u i)
  Unfold h sp t   -> Unfold h (SApp sp u i) (app t u i)
  _               -> impossible

inlineApp :: Val1 -> Val1 -> Icit -> Val1
inlineApp t u i = case t of
  Lam1 x i a t    -> t $$ u
  Rigid h sp      -> Rigid h (SApp sp u i)
  Flex h sp       -> Flex h (SApp sp u i)
  Unfold h sp t   -> Unfold h (SApp sp u i) (app t u i)
  _               -> impossible
{-# inline inlineApp #-}

appSp :: Val1 -> Spine -> Val1
appSp v = \case
  SNil        -> v
  SApp sp u i -> inlineApp (appSp v sp) u i

down :: Val1 -> Val0
down = \case Up t -> t; t -> Down t;
{-# inline down #-}

up :: Val0 -> Val1
up = \case Down t -> t; t -> Up t;
{-# inline up #-}

-- | Force flex
forceF1 :: Val1 -> Val1
forceF1 = \case
  Flex x sp -> runIO $ ES.readMeta x >>= \case
                 ES.MESolved v _ -> pure $! forceF1 (appSp v sp)
                 _               -> pure $! Flex x sp
  v           -> v

forceF0 :: Val0 -> Val0
forceF0 = \case
  Down t -> down (forceF1 t)
  t      -> t

-- | Force flex and unfolding
forceFU1 :: Val1 -> Val1
forceFU1 = \case
  Flex x sp     -> runIO $ ES.readMeta x >>= \case
                     ES.MESolved v _ -> pure $! forceFU1 (appSp v sp)
                     _               -> pure $! Flex x sp
  Unfold h sp t -> forceFU1 t
  v             -> v

forceFU0 :: Val0 -> Val0
forceFU0 = \case
  Down t -> down (forceFU1 t)
  t      -> t

eval0 :: Env -> S.Tm0 -> Val0
eval0 env t = let
  go  = eval0 env; {-# inline go #-}
  go1 = eval1 env; {-# inline go1 #-}
  in case t of
    S.Var0 x       -> Var0 (var0 env x)
    S.Top0 x       -> Top0 x
    S.Let0 x a t u -> Let0 x (go1 a) (go t) (Close env u)
    S.DataCon0 x i -> DataCon0 x i
    S.Lam0 x a t   -> Lam0 x (go1 a) (Close env t)
    S.App0 t u     -> App0 (go t) (go u)
    S.RecCon ts    -> RecCon (fmap go ts)
    S.Field t x n  -> Field (go t) x n
    S.Case t ts    -> Case (go t) (Close env ts)
    S.Down t       -> down (go1 t)

inserted :: Val1 -> Env -> S.Locals -> Val1
inserted t env           S.Empty             = t
inserted t (Snoc1 env u) (S.Define ls _ _ _) = inserted t env ls
inserted t (Snoc1 env u) (S.Bind ls x a    ) = inlineApp (inserted t env ls) u Expl
inserted _ _   _                             = impossible

eval1 :: Env -> S.Tm1 -> Val1
eval1 env t = let
  go = eval1 env; {-# inline go #-}
  in case t of
    S.Var1 x        -> var1 env x
    S.Top1 x        -> top1 x
    S.Meta x        -> meta x
    S.Inserted x ls -> runIO (metaIO x >>= \v -> pure $! inserted v env ls)
    S.Let1 x a t u  -> eval1 (Snoc1 env (go t)) u
    S.DataCon1 x i  -> Rigid (RHDataCon x i) SNil
    S.Lam1 x i a t  -> Lam1 x i (go a) (Close env t)
    S.App1 t u i    -> inlineApp (go t) (go u) i
    S.Fun a b       -> Fun (go a) (go b)
    S.Pi x i a b    -> Pi x i (go a) (Close env t)
    S.Ty u          -> Ty u
    S.Up t          -> up (eval0 env t)
    S.Lift a        -> Lift (go a)
    S.Rec fs        -> Rec (fmap go fs)
    S.TyCon x       -> Rigid (RHTyCon x) SNil


-- Quoting
--------------------------------------------------------------------------------


diveCase :: Env -> [Name] -> Lvl -> Env
diveCase env []     l = env
diveCase env (_:xs) l = diveCase (Snoc0 env l) xs (l + 1)

quoteSp :: Lvl -> Unfolding -> S.Tm1 -> Spine -> S.Tm1
quoteSp l st h sp = case sp of
  SNil        -> h
  SApp sp t i -> S.App1 (quoteSp l st h sp) (quote1 l st t) i

quote0 :: Lvl -> Unfolding -> Val0 -> S.Tm0
quote0 l st v = let
  go  = quote0 l st; {-# inline go #-}
  go1 = quote1 l st; {-# inline go1 #-}

  goClose :: Close S.Tm0 -> S.Tm0
  goClose t = quote0 (l + 1) st (dive t l)
  {-# inline goClose #-}

  goCases :: Close (Cases S.Tm0) -> Cases S.Tm0
  goCases (Close env cs) = goCs cs where
    goCs CNil              = CNil
    goCs (CCons x xs t cs) = CCons x xs (go (eval0 (diveCase env xs l) t)) (goCs cs)

  in case v of
    Var0 x       -> S.Var0 (lvlToIx l x)
    Top0 x       -> S.Top0 x
    App0 t u     -> S.App0 (go t) (go u)
    Let0 x a t u -> S.Let0 x (go1 a) (go t) (goClose u)
    Lam0 x a t   -> S.Lam0 x (go1 a) (goClose t)
    Down t       -> S.Down (quote1 l st t)
    DataCon0 x i -> S.DataCon0 x i
    RecCon fs    -> S.RecCon (fmap go fs)
    Case t cs    -> S.Case (go t) (goCases cs)
    Field t x n  -> S.Field (go t) x n

quote1 :: Lvl -> Unfolding -> Val1 -> S.Tm1
quote1 l st v = let
  go      = quote1 l st;  {-# inline go #-}
  goSp    = quoteSp l st; {-# inline goSp #-}
  force v = case st of DoUnfold -> forceFU1 v; _ -> forceF1 v

  goRH :: RigidHead -> S.Tm1
  goRH = \case
    RHVar x        -> S.Var1 (lvlToIx l x)
    RHDataCon x i  -> S.DataCon1 x i
    RHTyCon x      -> S.TyCon x

  goUH :: UnfoldHead -> S.Tm1
  goUH = \case
    UHMeta x -> S.Meta x
    UHTop x  -> S.Top1 x

  goClose :: Close S.Tm1 -> S.Tm1
  goClose t = quote1 (l + 1) st (t $$ Var1 l)
  {-# inline goClose #-}

  in case force v of
    Rigid h sp     -> goSp (goRH h) sp
    Flex x sp      -> goSp (S.Meta x) sp
    Unfold h sp t  -> goSp (goUH h) sp
    Pi x i a b     -> S.Pi x i (go a) (goClose b)
    Lam1 x i a t   -> S.Lam1 x i (go a) (goClose t)
    Fun a b        -> S.Fun (go a) (go b)
    Up t           -> S.Up (quote0 l st t)
    Lift a         -> S.Lift (go a)
    Rec as         -> S.Rec (fmap go as)
    Ty u           -> S.Ty u
