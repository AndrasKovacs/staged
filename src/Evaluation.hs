
module Evaluation (
  ($$), ($$$), dive, diveN, up, down, eval0, eval1, forceCV, eval,
  forceF, forceFU, quote, app1Sp, app1, nfNil, field1) where

import           IO
import           Common
import qualified Syntax as S
import qualified ElabState as ES
import           Values

--------------------------------------------------------------------------------

var0 :: Env -> Ix -> Lvl
var0 (Snoc0 _ l)   0 = l
var0 (Snoc1 env _) x = var0 env (x - 1)
var0 (Snoc0 env _) x = var0 env (x - 1)
var0 _             _ = impossible

var1 :: Env -> Ix -> Val1
var1 (Snoc1 _ v)   0 = v
var1 (Snoc1 env _) x = var1 env (x - 1)
var1 (Snoc0 env _) x = var1 env (x - 1)
var1 _             _ = impossible

metaIO :: MetaVar -> IO Val1
metaIO x = ES.readMeta x >>= \case
  ES.Unsolved{}  -> pure $ Flex x SId
  ES.Solved v _  -> pure $ Unfold (Solved x) SId v
{-# inline metaIO #-}

meta :: MetaVar -> Val1
meta x = runIO $ metaIO x
{-# inline meta #-}

top1 :: Lvl -> Val1
top1 x = runIO $ ES.readTop x >>= \case
  ES.TEDef _ _ _ v U1 _ _ -> pure $ Unfold (Top1 x) SId v
  _                       -> impossible
{-# inline top1 #-}

down :: Val1 -> Val0
down = \case Up t -> t; t -> Down t
{-# inline down #-}

up :: Val0 -> Val1
up = \case Down t -> t; t -> Up t
{-# inline up #-}

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

dive :: Close S.Tm0 -> Lvl -> Val0
dive (Close env t) l = eval0 (Snoc0 env l) t
{-# inline dive #-}

diveN :: Close S.Tm0 -> Lvl -> Int -> Val0
diveN (Close env t) l n = eval0 (go env l n) t where
  go acc l 0 = acc
  go acc l n = go (Snoc0 acc l) (l + 1) (n - 1)

app1 :: Val1 -> Val1 -> Icit -> Val1
app1 t u i = case t of
  Lam1 x i a t   -> t $$ u
  Flex h sp   -> Flex h (SApp1 sp u i)
  Unfold h sp t  -> Unfold h (SApp1 sp u i) (app1 t u i)
  t              -> App1 t u i

ixFields :: Fields a -> Int -> a
ixFields (FCons x a fs) 0 = a
ixFields (FCons _ _ fs) n = ixFields fs (n - 1)
ixFields _              _ = impossible

field1 :: Val1 -> Name -> Int -> Val1
field1 t x n = case t of
  RecCon ts     -> ixFields ts n
  Flex h sp  -> Flex h (SField1 sp x n)
  Unfold h sp t -> Unfold h (SField1 sp x n) (field1 t x n)
  t             -> Field t x n

inserted :: Val1 -> Env -> S.Locals -> Val1
inserted t env ls = case (env, ls) of
  (!env,        S.Empty           ) -> t
  (Snoc1 env u, S.Define ls _ _ _ ) -> inserted t env ls
  (Snoc0 env u, S.Bind ls _ _ U0{}) -> app1 (inserted t env ls) (Up (Var u)) Expl
  (Snoc1 env u, S.Bind ls _ _ U1  ) -> app1 (inserted t env ls) u Expl
  _                                 -> impossible

eval0 :: Env -> S.Tm0 -> Val0
eval0 env = \case
  S.Var x       -> Var (var0 env x)
  S.Top x       -> Top0 x
  S.Let x a t u -> Let x (eval1 env a) (eval0 env t) (Close env u)
  S.Lam0 x a t  -> Lam0 x (eval1 env a) (Close env t)
  S.App0 t u    -> App0 (eval0 env t) (eval0 env u)
  S.RecCon fs   -> RecCon (eval0 env <$> fs)
  S.Field t x n -> Field (eval0 env t) x n
  S.DataCon x n -> DataCon x n
  S.Case t cs   -> Case (eval0 env t) (Close env cs)
  S.Fix x y t   -> Fix x y (Close env t)
  S.Down t      -> down (eval1 env t)

eval1 :: Env -> S.Tm1 -> Val1
eval1 env = \case
  S.Var x         -> var1 env x
  S.Top x         -> top1 x
  S.Let x a t u   -> eval1 (Snoc1 env (eval1 env t)) u
  S.Pi x i a b    -> Pi x i (eval1 env a) (Close env b)
  S.Lam1 x i a t  -> Lam1 x i (eval1 env a) (Close env t)
  S.App1 t u i    -> app1 (eval1 env t) (eval1 env u) i
  S.Fun a b       -> Fun (eval1 env a) (eval1 env b)
  S.U u           -> U u
  S.Rec as        -> Rec (eval1 env <$> as)
  S.RecCon ts     -> RecCon (eval1 env <$> ts)
  S.Field t x n   -> field1 (eval1 env t) x n
  S.TyCon x       -> TyCon x
  S.DataCon x n   -> DataCon x n
  S.Lift a        -> Lift (eval1 env a)
  S.Up t          -> up (eval0 env t)
  S.Inserted x ls -> runIO do {t <- metaIO x; pure $! inserted t env ls}
  S.Meta x        -> meta x

eval :: Env -> S.Tm s -> U s -> Val s
eval env t = \case
  U0 _ -> eval0 env t
  U1   -> eval1 env t
{-# inline eval #-}

--------------------------------------------------------------------------------

app1Sp :: Val1 -> Spine -> Val1
app1Sp t = \case
  SId            -> t
  SApp1 sp u i   -> app1 (app1Sp t sp) u i
  SField1 sp x n -> field1 (app1Sp t sp) x n

forceCV :: CV -> CV
forceCV = \case
  CVVar x -> runIO $ ES.readCVMeta x >>= \case
               ES.CVSolved cv -> pure $! forceCV $! cv
               _              -> pure $! CVVar x
  cv -> cv

-- | Force Flex only.
forceF :: Val s -> Val s
forceF = \case
  Flex x sp -> runIO $ ES.readMeta x >>= \case
                    ES.Solved v _ -> pure $! forceF $! app1Sp v sp
                    _             -> pure $! Flex x sp
  Up t         -> case forceF t of
                    Down t -> forceF t
                    t      -> Up t
  Down t       -> case forceF t of
                    Up t   -> forceF t
                    t      -> Down t
  v            -> v

-- | Force Flex and Unfold.
forceFU :: Val s -> Val s
forceFU = \case
  Flex x sp -> runIO $ ES.readMeta x >>= \case
                    ES.Solved v _ -> pure $! forceFU $! app1Sp v sp
                    _             -> pure $! Flex x sp
  Unfold _ _ t -> forceFU t
  Up t         -> case forceFU t of
                    Down t -> forceFU t
                    t      -> Up t
  Down t       -> case forceFU t of
                    Up t   -> forceFU t
                    t      -> Down t
  v            -> v

quoteSp :: Lvl -> Unfolding -> S.Tm1 -> Spine -> S.Tm1
quoteSp l st h = \case
  SId            -> h
  SApp1 sp u i   -> S.App1 (quoteSp l st h sp) (quote l st u) i
  SField1 sp x n -> S.Field (quoteSp l st h sp) x n

quote :: forall s. Lvl -> Unfolding -> Val s -> S.Tm s
quote l st t = let

  go :: forall s. Val s -> S.Tm s
  go = quote l st; {-# inline go #-}

  goSp    = quoteSp l st; {-# inline goSp #-}
  force t = case st of DoUnfold -> forceFU t
                       _        -> forceF t
  {-# inline force #-}

  goUH :: UnfoldHead -> S.Tm1
  goUH = \case Solved x -> S.Meta x; Top1 x -> S.Top x
  {-# inline goUH #-}

  goClose0 :: Close S.Tm0 -> S.Tm0
  goClose0 t = quote (l + 1) st (dive t l)
  {-# inline goClose0 #-}

  goClose1 :: Close S.Tm1 -> S.Tm1
  goClose1 t = quote (l + 1) st (t $$ Var l)
  {-# inline goClose1 #-}

  goCases :: Close (Cases S.Tm0) -> Cases S.Tm0
  goCases (Close env cs) = go' cs where
    go' :: Cases S.Tm0 -> Cases S.Tm0
    go' CNil =
      CNil
    go' (CCons c xs t cs) =
      let n = length xs
      in CCons c xs (quote (l + Lvl n) st (diveN (Close env t) l n)) (go' cs)

  goFix :: Close S.Tm0 -> S.Tm0
  goFix (Close env t) =
    quote (l + 2) st (eval0 (env `Snoc0` l `Snoc0` (l + 1)) t)
  {-# inline goFix #-}

  in case force t of
    Unfold h sp _ -> goSp (goUH h) sp
    Flex x sp  -> goSp (S.Meta x) sp
    Var x         -> S.Var (lvlToIx l x)
    Top0 x        -> S.Top x
    Let x a t u   -> S.Let x (go a) (go t) (goClose0 u)
    Lift t        -> S.Lift (go t)
    Up t          -> S.Up (go t)
    Down t        -> S.Down (go t)
    TyCon x       -> S.TyCon x
    DataCon x n   -> S.DataCon x n
    Case t ts     -> S.Case (go t) (goCases ts)
    Fix x y t     -> S.Fix x y (goFix t)
    Pi x i a b    -> S.Pi x i (go a) (goClose1 b)
    Lam1 x i a t  -> S.Lam1 x i (go a) (goClose1 t)
    App1 t u i    -> S.App1 (go t) (go u) i
    Fun a b       -> S.Fun (go a) (go b)
    Lam0 x a t    -> S.Lam0 x (go a) (goClose0 t)
    App0 t u      -> S.App0 (go t) (go u)
    Rec fs        -> S.Rec (go <$> fs)
    RecCon ts     -> S.RecCon (go <$> ts)
    Field t x n   -> S.Field (go t) x n
    U u           -> S.U u

nfNil :: U s -> S.Tm s -> S.Tm s
nfNil u t = quote 0 DoUnfold (eval Nil t u)
