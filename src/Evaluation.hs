
module Evaluation (
    ($$), ($$$), dive, dive2, diveN, up, down, eval0, eval1
  , forceF0, forceFU0, forceF1, forceFU1
  , quote0, quote1, app1Sp, app1, nfNil0, nfNil1, field1
  )
  where

import           IO
import           Common
import qualified Syntax as S
import qualified ElabState as ES
import           Values

--------------------------------------------------------------------------------

var0 :: Dbg => Env -> Ix -> Lvl
var0 (Snoc0 _ l)   0 = l
var0 (Snoc1 env _) x = var0 env (x - 1)
var0 (Snoc0 env _) x = var0 env (x - 1)
var0 _             _ = impossible

var1 :: Dbg => Env -> Ix -> Val1
var1 (Snoc1 _ v)   0 = v
var1 (Snoc1 env _) x = var1 env (x - 1)
var1 (Snoc0 env _) x = var1 env (x - 1)
var1 env           x = impossible

metaIO :: Dbg => MetaVar -> IO Val1
metaIO x = ES.readMeta x >>= \case
  ES.Unsolved{}  -> pure $ Flex x Id
  ES.Solved v _  -> pure $ Unfold (Solved x) Id v
{-# inline metaIO #-}

meta :: Dbg => MetaVar -> Val1
meta x = runIO $ metaIO x
{-# inline meta #-}

top1 :: Dbg => Lvl -> Val1
top1 x = runIO $ ES.readTop x >>= \case
  ES.TEDef1 _ _ _ v _ _ -> pure $ Unfold (Top1 x) Id v
  _                     -> impossible
{-# inline top1 #-}

down :: Val1 -> Val0
down = \case Up t -> t; t -> Down t
{-# inline down #-}

up :: Val0 -> Val1
up = \case Down t -> t; t -> Up t
{-# inline up #-}

infixl 2 $$
-- | Strict closure application.
($$) :: Dbg => Close S.Tm1 -> Val1 -> Val1
($$) (Close env t) u = eval1 (Snoc1 env u) t
{-# inline ($$) #-}

infixl 2 $$$
-- | Lazy closure application.
($$$) :: Dbg => Close S.Tm1 -> Val1 -> Val1
($$$) (Close env t) ~u = eval1 (Snoc1 env u) t
{-# inline ($$$) #-}

-- | Go under 1 lvl0 binder.
dive :: Dbg => Close S.Tm0 -> Lvl -> Val0
dive (Close env t) l = eval0 (Snoc0 env l) t
{-# inline dive #-}

-- | Go under 2 lvl0 binders.
dive2 :: Dbg => Close S.Tm0 -> Lvl -> Val0
dive2 (Close env t) l = eval0 (env `Snoc0` l `Snoc0` (l + 1)) t
{-# inline dive2 #-}

-- | Go under N lvl0 binders.
diveN :: Dbg => Close S.Tm0 -> Lvl -> Int -> Val0
diveN (Close env t) l n = eval0 (go env l n) t where
  go acc l 0 = acc
  go acc l n = go (Snoc0 acc l) (l + 1) (n - 1)

app1 :: Dbg => Val1 -> Val1 -> Icit -> Val1
app1 t u i = case t of
  Lam1 x i a t   -> t $$ u
  Flex h sp      -> Flex h (App1 sp u i)
  Unfold h sp t  -> Unfold h (App1 sp u i) (app1 t u i)
  Rigid h sp     -> Rigid h (App1 sp u i)
  _              -> impossible

lookupField :: Dbg => Fields a -> Int -> a
lookupField (FCons x a fs) 0 = a
lookupField (FCons _ _ fs) n = lookupField fs (n - 1)
lookupField _              _ = impossible

field1 :: Dbg => Val1 -> Name -> Int -> Val1
field1 t x n = case t of
  RecCon1 ts    -> lookupField ts n
  Flex h sp     -> Flex h (Field1 sp x n)
  Unfold h sp t -> Unfold h (Field1 sp x n) (field1 t x n)
  Rigid h sp    -> Rigid h (Field1 sp x n)
  _             -> impossible

appPruning :: Dbg => Env -> Val1 -> S.Pruning -> Val1
appPruning env ~v pr = case (env, pr) of
  (Nil         , []               ) -> v
  (Snoc1 env u , pr :> S.PESkip   ) -> appPruning env v pr
  (Snoc0 env u , pr :> S.PEBind0  ) -> app1 (appPruning env v pr) (Up (Var0 u)) Expl
  (Snoc1 env u , pr :> S.PEBind1 i) -> app1 (appPruning env v pr) u i
  _                                 -> impossible

eval0 :: Dbg => Env -> S.Tm0 -> Val0
eval0 env = \case
  S.Var0 x       -> Var0 (var0 env x)
  S.Top0 x       -> Top0 x
  S.Let0 x a t u -> Let0 x (eval1 env a) (eval0 env t) (Close env u)
  S.Lam0 x a t   -> Lam0 x (eval1 env a) (Close env t)
  S.App0 t u     -> App0 (eval0 env t) (eval0 env u)
  S.RecCon0 fs   -> RecCon0 (eval0 env <$> fs)
  S.Field0 t x n -> Field0 (eval0 env t) x n
  S.Case t cs    -> Case (eval0 env t) (Close env cs)
  S.Down t       -> down (eval1 env t)
  S.Add t u      -> Add (eval0 env t) (eval0 env u)
  S.Mul t u      -> Mul (eval0 env t) (eval0 env u)
  S.Sub t u      -> Sub (eval0 env t) (eval0 env u)
  S.IntLit n     -> IntLit n
  S.Wk10 t       -> eval0 (wk1Env env) t

eval1 :: Dbg => Env -> S.Tm1 -> Val1
eval1 env = \case
  S.Var1 x          -> var1 env x
  S.Top1 x          -> top1 x
  S.Let1 x a t u    -> eval1 (Snoc1 env (eval1 env t)) u
  S.Pi x i a b      -> Pi x i (eval1 env a) (Close env b)
  S.Lam1 x i a t    -> Lam1 x i (eval1 env a) (Close env t)
  S.App1 t u i      -> app1 (eval1 env t) (eval1 env u) i
  S.Fun a b bcv     -> Fun (eval1 env a) (eval1 env b) (eval1 env bcv)
  S.U0 cv           -> U0 (eval1 env cv)
  S.Rec0 as         -> Rec0 (eval1 env <$> as)
  S.Rec1 as         -> Rec1 (Close env as)
  S.RecCon1 ts      -> RecCon1 (eval1 env <$> ts)
  S.Field1 t x n    -> field1 (eval1 env t) x n
  S.TyCon x         -> Rigid (RHTyCon x) Id
  S.DataCon  x n    -> Rigid (RHDataCon x n) Id
  S.Lift cv a       -> Lift (eval1 env cv) (eval1 env a)
  S.Up t            -> up (eval0 env t)
  S.AppPruning t pr -> appPruning env (eval1 env t) pr
  S.Meta x          -> meta x
  S.Wk11 t          -> eval1 (wk1Env env) t
  S.Wk01 t          -> eval1 (wk0Env env) t
  S.Int             -> Int
  S.U1              -> U1
  S.CV              -> CV
  S.Comp            -> Comp
  S.Val             -> Val

--------------------------------------------------------------------------------

app1Sp :: Val1 -> Spine -> Val1
app1Sp t = \case
  Id            -> t
  App1 sp u i   -> app1 (app1Sp t sp) u i
  Field1 sp x n -> field1 (app1Sp t sp) x n

-- | Force Flex only.
forceF1 :: Val1 -> Val1
forceF1 = \case
  Flex x sp -> runIO $ ES.readMeta x >>= \case
    ES.Solved v _ -> pure $! forceF1 $! app1Sp v sp
    _             -> pure $! Flex x sp
  v         -> v

-- | Force Flex only.
forceF0 :: Val0 -> Val0
forceF0 = \case
  Down t -> case forceF1 t of
              Up t -> forceF0 t
              t    -> Down t
  v      -> v

-- | Force Flex and Unfold.
forceFU1 :: Val1 -> Val1
forceFU1 = \case
  Flex x sp    -> runIO $ ES.readMeta x >>= \case
                    ES.Solved v _ -> pure $! forceFU1 $! app1Sp v sp
                    _             -> pure $! Flex x sp
  Unfold _ _ t -> forceFU1 t
  v            -> v

-- | Force Flex and Unfold.
forceFU0 :: Val0 -> Val0
forceFU0 = \case
  Down t -> case forceFU1 t of
              Up t -> forceFU0 t
              t    -> Down t
  v      -> v

--------------------------------------------------------------------------------

quoteSp :: Dbg => Lvl -> QuoteOption -> S.Tm1 -> Spine -> S.Tm1
quoteSp l st h = \case
  Id            -> h
  App1 sp u i   -> S.App1 (quoteSp l st h sp) (quote1 l st u) i
  Field1 sp x n -> S.Field1 (quoteSp l st h sp) x n

quote1 :: Dbg => Lvl -> QuoteOption -> Val1 -> S.Tm1
quote1 l st t = let
  go0  = quote0 l st; {-# inline go0 #-}
  go1  = quote1 l st; {-# inline go1 #-}
  goSp = quoteSp l st; {-# inline goSp #-}

  force t = case st of UnfoldAll  -> forceFU1 t
                       UnfoldFlex -> forceF1 t
                       _          -> t
  {-# inline force #-}

  goUH :: UnfoldHead -> S.Tm1
  goUH = \case Solved x -> S.Meta x; Top1 x -> S.Top1 x
  {-# inline goUH #-}

  goRH :: Lvl -> RigidHead -> S.Tm1
  goRH l = \case
    RHVar1 x     -> S.Var1 (lvlToIx l x)
    RHTyCon x      -> S.TyCon x
    RHDataCon x ix -> S.DataCon x ix
  {-# inline goRH #-}

  goClose1 :: Close S.Tm1 -> S.Tm1
  goClose1 t = quote1 (l + 1) st (t $$ Var1 l)
  {-# inline goClose1 #-}

  goRec1 :: Close (Fields S.Ty) -> Fields S.Ty
  goRec1 (Close env as) = go env l as where
    go env l FNil           = FNil
    go env l (FCons x a as) =
      FCons x (quote1 l st (eval1 env a)) (go (Snoc1 env (Var1 l)) (l + 1) as)

  in case force t of
    Unfold h sp _ -> goSp (goUH h) sp
    Flex x sp     -> goSp (S.Meta x) sp
    Rigid h sp    -> goSp (goRH l h) sp
    Var1 x        -> S.Var1 (lvlToIx l x)
    Lift cv t     -> S.Lift (go1 cv) (go1 t)
    Up t          -> S.Up (go0 t)
    Pi x i a b    -> S.Pi x i (go1 a) (goClose1 b)
    Lam1 x i a t  -> S.Lam1 x i (go1 a) (goClose1 t)
    Fun a b bcv   -> S.Fun (go1 a) (go1 b) (go1 bcv)
    Rec0 as       -> S.Rec0 (go1 <$> as)
    Rec1 as       -> S.Rec1 (goRec1 as)
    RecCon1 ts    -> S.RecCon1 (go1 <$> ts)
    Int           -> S.Int
    U1            -> S.U1
    U0 cv         -> S.U0 (go1 cv)
    Comp          -> S.Comp
    Val           -> S.Val
    CV            -> S.CV

quoteCases :: Dbg => Lvl -> QuoteOption -> Close (Cases S.Tm0) -> Cases S.Tm0
quoteCases l st (Close env cs) = case cs of
  CNil -> CNil
  CCons c xs t cs ->
    let n = length xs
    in CCons c xs (quote0 (l + Lvl n) st (diveN (Close env t) l n))
                  (quoteCases l st (Close env cs))

quote0 :: Dbg => Lvl -> QuoteOption -> Val0 -> S.Tm0
quote0 l st t = let
  go0  = quote0 l st; {-# inline go0 #-}
  go1  = quote1 l st; {-# inline go1 #-}
  goSp = quoteSp l st; {-# inline goSp #-}

  force t = case st of UnfoldAll  -> forceFU0 t
                       UnfoldFlex -> forceF0 t
                       _          -> t
  {-# inline force #-}

  goClose0 :: Close S.Tm0 -> S.Tm0
  goClose0 t = quote0 (l + 1) st (dive t l)
  {-# inline goClose0 #-}

  goCases :: Close (Cases S.Tm0) -> Cases S.Tm0
  goCases = quoteCases l st; {-# inline goCases #-}

  goFix :: Close S.Tm0 -> S.Tm0
  goFix (Close env t) =
    quote0 (l + 2) st (eval0 (env `Snoc0` l `Snoc0` (l + 1)) t)
  {-# inline goFix #-}

  in case force t of
    Var0 x        -> case st of
                       LiftVars lvl | x < lvl -> S.Down (S.Var1 (lvlToIx l x))
                       _                      -> S.Var0 (lvlToIx l x)
    Top0 x        -> S.Top0 x
    Let0 x a t u  -> S.Let0 x (go1 a) (go0 t) (goClose0 u)
    Down t        -> S.Down (go1 t)
    Case t ts     -> S.Case (go0 t) (goCases ts)
    Lam0 x a t    -> S.Lam0 x (go1 a) (goClose0 t)
    App0 t u      -> S.App0 (go0 t) (go0 u)
    RecCon0 ts    -> S.RecCon0 (go0 <$> ts)
    Field0 t x n  -> S.Field0 (go0 t) x n
    Add t u       -> S.Add (go0 t) (go0 u)
    Sub t u       -> S.Sub (go0 t) (go0 u)
    Mul t u       -> S.Mul (go0 t) (go0 u)
    IntLit n      -> S.IntLit n

--------------------------------------------------------------------------------

nfNil0 :: S.Tm0 -> S.Tm0
nfNil0 t = quote0 0 UnfoldAll (eval0 Nil t)

nfNil1 :: S.Tm1 -> S.Tm1
nfNil1 t = quote1 0 UnfoldAll (eval1 Nil t)
