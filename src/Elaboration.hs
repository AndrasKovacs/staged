{-# options_ghc -Wno-unused-imports #-}

module Elaboration where

import Control.Monad

import Data.IORef

import qualified Data.ByteString     as B
import qualified Data.HashMap.Strict as HM
import qualified FlatParse.Stateful  as FP

import qualified Syntax              as S
import qualified Values              as V
import qualified Presyntax           as P
import qualified Evaluation          as Eval

import Common
import Cxt
import ElabState
import InCxt
import Exceptions


-- TODO: fix context decoration of errors

--------------------------------------------------------------------------------

elabError :: Cxt -> P.Tm -> Ex -> IO a
elabError cxt tgt err = throwIO $ ElabError (_locals cxt) tgt err
{-# inline elabError #-}

-- Converting spans to names
--------------------------------------------------------------------------------

unsafeSlice :: RawName -> Span -> RawName
unsafeSlice x span = coerce $ FP.unsafeSlice (coerce x) span
{-# inline unsafeSlice #-}

spanToRawName :: Cxt -> Span -> RawName
spanToRawName cxt span = unsafeSlice (_src cxt) span
{-# inline spanToRawName #-}

spanToName :: Cxt -> Span -> Name
spanToName cxt = NName . spanToRawName cxt
{-# inline spanToName #-}

bindToName :: Cxt -> P.Bind -> Name
bindToName cxt (P.Bind x) = NName (spanToRawName cxt x)
bindToName cxt P.DontBind = NEmpty
{-# inline bindToName #-}


-- Implicit insertions
--------------------------------------------------------------------------------

insert' :: Cxt -> IO (S.Tm1, V.Ty) -> IO (S.Tm1, V.Ty)
insert' cxt inf = do {(t, a) <- inf; go t a} where
  go :: S.Tm1 -> V.Ty -> IO (S.Tm1, V.Ty)
  go t topA = case forceFU1 topA of
    V.Pi x Impl a b -> do
      m <- freshMeta cxt (quote1 cxt a)
      go (S.App1 t m Impl) (b $$$ eval1 cxt m)
    topA ->
      pure (t, topA)

insertTmU' :: Cxt -> IO TmU -> IO TmU
insertTmU' cxt inf = inf >>= \case
  Tm0 t a cv -> pure $! Tm0 t a cv
  Tm1 t a    -> do {(t, a) <- insert' cxt (pure (t, a)); pure (Tm1 t a)}

insertUntilName :: Cxt -> P.Tm -> RawName -> IO (S.Tm1, V.Ty) -> IO (S.Tm1, V.Ty)
insertUntilName cxt topT topX inf = do {(t, a) <- inf; go t a} where
  go :: S.Tm1 -> V.Ty -> IO (S.Tm1, V.Ty)
  go t topA = case forceFU1 topA of
    V.Pi x Impl a b -> do
      if NName topX == x then
        pure (t, topA)
      else do
        m <- freshMeta cxt (quote1 cxt a)
        go (S.App1 t m Impl) (b $$$ eval1 cxt m)
    _ ->
      elabError cxt topT $ NoSuchArgument topX

insert :: Cxt -> IO (S.Tm1, V.Ty) -> IO (S.Tm1, V.Ty)
insert cxt inf = inf >>= \case
  res@(S.Lam1 _ Impl a t, va) -> pure res
  res                         -> insert' cxt (pure res)
{-# inline insert #-}

-- coercion
--------------------------------------------------------------------------------

-- Subtyping in U0 (implicit)
subtype0 :: Cxt -> V.Ty -> CV -> V.Ty -> CV -> IO ()
subtype0 cxt a cv a' cv' = do
  case (forceCV cv, forceCV cv') of
    (V, C)    -> pure ()
    (cv, cv') -> unifyCV cv cv'
  unify1 cxt a a'

-- Coercion in U1 (explicit)
coe1 :: Cxt -> S.Tm1 -> V.Ty -> V.Ty -> IO S.Tm1
coe1 cxt t a a' = maybe t id <$> go cxt t a a' where

  -- ugly helper for record structural rule
  goRec1 :: Cxt -> S.Tm1 -> Int -> V.Close (Fields S.Tm1)
                -> V.Close (Fields S.Tm1) -> IO (Fields (Bool, S.Tm1))
  goRec1 cxt t ix (V.Close e as) (V.Close e' as') = case (as, as') of
    (FNil, FNil) -> pure FNil
    (FCons x a as, FCons x' a' as') -> do
      when (x /= x') $ throwIO $ FieldNameMismatch x x'
      let va   = Eval.eval1 e a
          va'  = Eval.eval1 e' a'
          t1   = S.Field1 t x ix
          ~vt1 = eval1 cxt t1
      go cxt t1  va va' >>= \case
        Nothing ->
          (FCons x (False, t1)) <$!>
             goRec1 cxt t (ix + 1) (V.Close (V.Snoc1 e  vt1) as)
                                   (V.Close (V.Snoc1 e' vt1) as')
        Just coet1 ->
          (FCons x (True, coet1)) <$!>
             goRec1 cxt t (ix + 1) (V.Close (V.Snoc1 e  vt1)               as)
                                   (V.Close (V.Snoc1 e' (eval1 cxt coet1)) as')
    _ ->
      throwIO CantUnify

  -- lifting helpers
  --------------------------------------------------------------------------------

  -- compute ^(a -> b) to ^a -> ^b
  liftFun :: Cxt -> V.Ty -> V.Ty -> V.Ty
  liftFun cxt a b =
    let qb = Eval.quote1 (_lvl cxt + 1) DontUnfold b
    in V.Pi NEmpty Expl (V.Lift V a) (V.Close V.Nil (S.Lift C qb))

  --  ^(a -> b) -> ^a -> ^b
  upFun :: Cxt -> V.Ty -> S.Tm1 -> S.Tm1
  upFun cxt a t =
    S.Lam1 NEmpty Expl (S.Lift V (quote1 cxt a)) (S.Up (S.App0 (S.Down (S.Wk1 t)) (S.Down (S.Var1 0))))

  --  (^a -> ^b) -> ^(a -> b)
  downFun :: Cxt -> V.Ty -> S.Tm1 -> S.Tm1
  downFun cxt a t =
    S.Up (S.Lam0 NEmpty (quote1 cxt a) (S.Down (S.App1 (S.Wk1 t) (S.Var1 0) Expl)))

  -- compute ^(Rec0 as) to Rec1 (map ^ as)
  liftRec0 :: Cxt -> Fields V.Ty -> V.Ty
  liftRec0 cxt as = V.Rec1 (V.Close V.Nil (go (_lvl cxt) as)) where
    go :: Lvl -> Fields V.Ty -> Fields S.Tm1
    go l FNil           = FNil
    go l (FCons x a as) =
      FCons x (S.Lift V (Eval.quote1 l DontUnfold a)) (go (l + 1) as)

  --  ^(Rec as) -> Rec (map ^ as)
  upRecCon0 :: S.Tm1 -> Fields a -> S.Tm1
  upRecCon0 t as = S.RecCon1 (go 0 as) where
    go ix FNil           = FNil
    go ix (FCons x _ as) = FCons x (S.Up (S.Field0 (S.Down t) x ix)) (go (ix + 1) as)

  -- Rec (map ^ as) -> ^(Rec as)
  downRecCon0 :: Fields a -> S.Tm1 -> S.Tm1
  downRecCon0 as t = S.Up (S.RecCon0 (go 0 as)) where
    go ix FNil           = FNil
    go ix (FCons x _ as) = FCons x (S.Down (S.Field1 t x ix)) (go (ix + 1) as)


  go :: Cxt -> S.Tm1 -> V.Ty -> V.Ty -> IO (Maybe S.Tm1)
  go cxt t a a' = case (forceFU1 a, forceFU1 a') of

    -- structural rules
    ------------------------------------------------------------

    (V.Lift cv a, V.Lift cv' a') -> do
      subtype0 cxt a cv a' cv'
      pure Nothing

    (V.Pi x i a b, V.Pi x' i' a' b') -> do
      when (i /= i') $ throwIO $ IcitMismatch i i'
      let cxt' = bind1' x (quote1 cxt a') a' cxt
      coev0 <- go cxt' (S.Var1 0) a' a
      case coev0 of
        Nothing ->
          (S.Lam1 x i (quote1 cxt a') <$!>) <$!>
            go cxt' (S.App1 (S.Wk1 t) (S.Var1 0) i)
                    (b  $$$ eval1 cxt' (S.Var1 0))
                    (b' $$$ V.Var1 (_lvl cxt))

        Just coev0 ->
          (Just . S.Lam1 x i (quote1 cxt a')) <$!>
            coe1 cxt' (S.App1 (S.Wk1 t) coev0 i) (b  $$$ eval1 cxt' coev0)
                                                 (b' $$$ V.Var1 (_lvl cxt))

    (V.Rec1 as, V.Rec1 as') -> do
      fs <- goRec1 cxt t 0 as as'
      if any fst fs
        then pure $! Just $! S.RecCon1 (snd <$> fs)
        else pure Nothing


    -- TODO: Pi-Flex, Flex-Pi   (refine other side)
    --       Rec-Flex, Flex-Rec (refine other side)

    -- lift preservation
    ------------------------------------------------------------

    (V.Lift _ (V.Fun a b), a') ->
      Just <$!> coe1 cxt (upFun cxt a t) (liftFun cxt a b) a'

    (a, V.Lift cv' (V.Fun a' b')) ->
      Just . downFun cxt a' <$!> coe1 cxt t a (liftFun cxt a' b')

    (V.Lift _ (V.Rec0 as), a') ->
      Just <$!> coe1 cxt (upRecCon0 t as) (liftRec0 cxt as) a'

    (a, V.Lift _ (V.Rec0 as)) ->
      Just . downRecCon0 as <$!> coe1 cxt t a (liftRec0 cxt as)

    (a, a') ->
      Nothing <$ unify1 cxt a a'


tyAnnot :: Cxt -> Maybe P.Tm -> U -> IO S.Ty
tyAnnot cxt ma un =
  maybe (freshMeta cxt (S.U un)) (\a -> check1 cxt a (V.U un)) ma
{-# inline tyAnnot #-}

-- Checking
--------------------------------------------------------------------------------

check0 :: Cxt -> P.Tm -> V.Ty -> CV -> IO S.Tm0
check0 cxt topT topA cv = case (topT, forceFU1 topA, forceCV cv) of

  (P.Lam _ (bindToName cxt -> x) i ma t, V.Fun a' b', !cv) -> do
    case i of P.NoName Expl -> pure ()
              _           -> elabError cxt topT NoImplicitLam0
    let qa' = quote1 cxt a'
    S.Lam0 x qa' <$!>
      check0 (bind0' x qa' a' V cxt) t b' C

  (P.RecCon _ ts, V.Rec0 as, _) -> do

    let go [] FNil = pure FNil
        go ((x, t): ts) (FCons x' a as) = do
          let rn = spanToRawName cxt x
          unless (NName rn == x') $
            elabError cxt topT (NoSuchField rn)
          t <- check0 cxt t a V
          ts <- go ts as
          pure $! FCons x' t ts
        go ((x, t):ts) FNil =
          elabError cxt topT ExpectedEmptyRecCon
        go _ _ =
          elabError cxt topT ExpectedNonEmptyRecCon

    S.RecCon0 <$!> go ts as

  -- TODO : index Fun with cod CV!
  (P.Fix _ (bindToName cxt -> x) (bindToName cxt -> y) t, V.Fun a b, _) -> do
    let cxt' = bind0' x (quote1 cxt b) b C $ bind0' x (quote1 cxt a) a V cxt
    t <- check0 cxt' t b V
    pure $! S.Fix x y t

  (P.Tuple _ ts, V.Rec0 as, _) -> do

    let go [] FNil = pure FNil
        go (t:ts) (FCons x' a as) =
          FCons x' <$!> check0 cxt t a V <*!> go ts as
        go (t:ts) FNil =
          elabError cxt topT ExpectedEmptyRecCon
        go _ _ =
          elabError cxt topT ExpectedNonEmptyRecCon

    S.RecCon0 <$!> go ts as

  (P.EmptyRec _, V.Rec0 as, _) -> do
    case as of FNil -> pure ()
               _    -> elabError cxt topT ExpectedEmptyRec
    pure $! S.RecCon0 FNil

  (P.Let0 _ (spanToName cxt -> x) ma t u, topA, cv) -> do
    acv  <- freshCV
    a    <- tyAnnot cxt ma (U0 acv)
    let va = eval1 cxt a
    t <- check0 cxt t va acv
    u <- check0 (bind0' x a va acv cxt) u topA cv
    pure $! S.Let0 x a t u

  (P.Hole _, topA, cv) -> do
    S.Down <$!> freshMeta cxt (S.Lift cv (quote1 cxt topA))

  (P.Down _ t, topA, cv) -> do
    S.down <$!> check1 cxt t (V.Lift cv topA)

  (t, topA, cv) -> do
    (t, a, cv') <- infer0 cxt t
    subtype0 cxt topA cv a cv'
    pure t

checkRec0 :: Cxt -> [(Span, P.Tm)] -> IO (Fields S.Ty)
checkRec0 cxt as = go as where
  go []          = pure FNil
  go ((x, a):as) = FCons (spanToName cxt x) <$!> check1 cxt a (V.U (U0 V))
                                            <*!> go as

checkRec1 :: Cxt -> [(Span, P.Tm)] -> IO (Fields S.Tm1)
checkRec1 cxt as = go cxt as where
  go cxt [] = pure FNil
  go cxt ((spanToName cxt -> x, a):as) = do
    a  <- check1 cxt a (V.U U1)
    as <- go (bind1' x a (eval1 cxt a) cxt) as
    pure $! FCons x a as

checkRecCon1 :: Cxt -> P.Tm -> [(Span, P.Tm)] -> V.Close (Fields S.Ty) -> IO (Fields S.Tm1)
checkRecCon1 cxt topT ts (V.Close env as) = case (ts, as) of
  ([], FNil) -> pure FNil
  ((spanToRawName cxt -> x, t):ts, FCons x' a as) -> do
    unless (NName x == x') $
      elabError cxt topT (NoSuchField x)
    let va = Eval.eval1 env a
        qa = Eval.quote1 (_lvl cxt) DontUnfold va
    t  <- check1 cxt t va
    let ~vt = eval1 cxt t
    ts <- checkRecCon1 (define' x' qa va t vt cxt)
                       topT ts (V.Close (V.Snoc1 env vt) as)
    pure $! FCons x' t ts
  ([], FCons{}) ->
    elabError cxt topT ExpectedNonEmptyRecCon
  _ ->
    elabError cxt topT ExpectedEmptyRecCon

checkTuple1 :: Cxt -> P.Tm -> [P.Tm] -> V.Close (Fields S.Ty) -> IO (Fields S.Tm1)
checkTuple1 cxt topT ts (V.Close env as) = case (ts, as) of
  ([], FNil) -> pure FNil
  (t:ts, FCons x' a as) -> do
    let va = Eval.eval1 env a
        qa = Eval.quote1 (_lvl cxt) DontUnfold va
    t  <- check1 cxt t va
    let ~vt = eval1 cxt t
    ts <- checkTuple1 (define' x' qa va t vt cxt)
                       topT ts (V.Close (V.Snoc1 env vt) as)
    pure $! FCons x' t ts
  ([], FCons{}) ->
    elabError cxt topT ExpectedNonEmptyRecCon
  _ ->
    elabError cxt topT ExpectedEmptyRecCon

check1 :: Cxt -> P.Tm -> V.Ty -> IO S.Tm1
check1 cxt topT topA = case (topT, forceFU1 topA) of

  (P.Lam _ (bindToName cxt -> x) i ma t, V.Pi x' i' a' b')
    | case i of P.NoName i                    -> i == i'
                P.Named (spanToName cxt -> x) -> x == x' -> do
    let qa' = quote1 cxt a'
    S.Lam1 x' i' qa' <$!>
      check1 (bind1' x (quote1 cxt a') a' cxt) t (b' $$ V.Var1 (_lvl cxt))

  (t, V.Pi x' Impl a' b') -> do
    let qa' = quote1 cxt a'
    S.Lam1 x' Impl qa' <$!>
      check1 (newBinder x' qa' cxt) t (b' $$ V.Var1 (_lvl cxt))

  (P.Pi _ P.DontBind Expl a b, V.U (U0 cv)) -> do
    unifyCV cv C
    a <- check1 cxt b (V.U (U0 V))
    b <- check1 cxt b (V.U (U0 C))
    pure $! S.Fun a b

  (P.Pi _ (bindToName cxt -> x) i a b, V.U U1) -> do
    a <- check1 cxt a (V.U U1)
    b <- check1 (bind1' x a (eval1 cxt a) cxt) b (V.U U1)
    pure $! S.Pi x i a b

  (P.Rec _ as, V.U (U0 cv)) -> do
    unifyCV cv V
    S.Rec0 <$!> checkRec0 cxt as

  (P.Rec _ as, V.U U1) -> do
    S.Rec1 <$!> checkRec1 cxt as

  (P.EmptyRec _, V.U u) -> do
    case u of U0 cv -> unifyCV cv V
              _     -> pure ()
    pure $! S.Rec1 FNil

  (P.Lift _ t, V.U U1) -> do
    cv <- freshCV
    S.Lift cv <$!> check1 cxt t (V.U (U0 cv))

  (P.RecCon _ ts, V.Rec1 as) -> do

    S.RecCon1 <$!> checkRecCon1 cxt topT ts as

  (P.Tuple _ ts, V.Rec1 as) -> do
    S.RecCon1 <$!> checkTuple1 cxt topT ts as

  (P.EmptyRec _, V.Rec1 (V.Close _ as)) -> do
    case as of FNil -> pure ()
               _    -> elabError cxt topT ExpectedEmptyRec
    pure $! S.RecCon1 FNil

  (P.Let1 _ (spanToName cxt -> x) ma t u, topA) -> do
    a <- tyAnnot cxt ma U1
    let va = eval1 cxt a
    t <- check1 cxt t va
    u <- check1 (define' x a va t (eval1 cxt t) cxt) u topA
    pure $! S.Let1 x a t u

  (P.Up _ t, V.Lift cv topA) -> do
    S.up <$!> check0 cxt t topA cv

  (t, V.Lift cv topA) -> do
    S.up <$!> check0 cxt t topA cv

  (P.Hole _, topA) -> do
    freshMeta cxt (quote1 cxt topA)

  (t, topA) -> do
    (t, a) <- insert cxt $ infer1 cxt t
    coe1 cxt t a topA

data TmU = Tm0 S.Tm0 V.Ty CV | Tm1 S.Tm1 V.Ty


-- TODO: add more inference cases here
infer0 :: Cxt -> P.Tm -> IO (S.Tm0, V.Ty, CV)
infer0 cxt topT =
  infer cxt topT >>= \case
    Tm0 t a cv ->
      pure (t, a, cv)
    Tm1 t a -> case forceFU1 a of
      V.Lift cv a -> do
        let t' = S.down t
        pure (t', a, cv)
      a -> do
        cv <- freshCV
        a' <- eval1 cxt <$!> freshMeta cxt (S.U (U0 cv))
        t  <- S.down <$!> coe1 cxt t a (V.Lift cv a')
        pure (t, a', cv)

-- TODO: add more inference cases here
infer1 :: Cxt -> P.Tm -> IO (S.Tm1, V.Ty)
infer1 cxt t =
  infer cxt t >>= \case
    Tm0 t a cv -> let t' = S.up t in pure (t', V.Lift cv a)
    Tm1 t a    -> pure (t, a)

checkU :: P.U -> S.Tm1
checkU = \case
  P.U0 P.C -> S.U (U0 C)
  P.U0 P.V -> S.U (U0 V)
  P.U1     -> S.U U1

inferRecCon0 :: Cxt -> [(Span, P.Tm)] -> IO (Fields S.Tm0, Fields V.Ty)
inferRecCon0 cxt cs = case cs of
  [] -> pure (FNil, FNil)
  (spanToName cxt -> x, t):cs -> do
    (t, a, cv) <- infer0 cxt t
    unifyCV cv V
    (ts, as) <- inferRecCon0 cxt cs
    pure (FCons x t ts, FCons x a as)

ensureFun :: Cxt -> V.Ty -> IO (V.Ty, V.Ty)
ensureFun cxt a = case forceFU1 a of
  V.Fun a b -> pure (a, b)
  a         -> do
    cv <- freshCV
    a' <- eval1 cxt <$!> freshMeta cxt (S.U (U0 V))
    b' <- eval1 cxt <$!> freshMeta cxt (S.U (U0 cv))
    unify1 cxt a (V.Fun a' b')
    pure (a', b')

coeToPi :: Cxt -> V.Ty -> Icit -> S.Tm1 -> IO (S.Tm1, Name, V.Ty, V.Close S.Ty)
coeToPi cxt a i t = case forceFU1 a of
  V.Pi x i' a b -> do
    when (i /= i) $ throwIO $ IcitMismatch i' i
    pure (t, x, a, b)
  a -> do
    a' <- freshMeta cxt (S.U U1)
    let va' = eval1 cxt a'
    b' <- V.Close V.Nil <$!> freshMeta (bind1' NX a' va' cxt) (S.U U1)
    t <- coe1 cxt t a (V.Pi NX i va' b')
    pure (t, NX, va', b')

infer :: Cxt -> P.Tm -> IO TmU
infer cxt topT = let

  err :: forall a. Ex -> IO a
  err = elabError cxt topT; {-# inline err #-}

  in case topT of
    P.Var (spanToRawName cxt -> x) ->
      case HM.lookup x (_nameTable cxt) of
        Just ni -> case ni of
          NameInfo x a (forceU -> au) -> case au of
            U0 cv -> pure $! Tm0 (S.Var0 (lvlToIx (_lvl cxt) x)) a cv
            U1    -> pure $! Tm1 (S.Var1 (lvlToIx (_lvl cxt) x)) a
        Nothing -> lookupTopName x >>= \case
          Nothing -> err $ NameNotInScope x
          Just x  -> readTop x >>= \case
            TEDef0 _ va _ _ cv _ _       -> pure $! Tm0 (S.Top0 x) va cv
            TEDef1 _ va _ _ _ _          -> pure $! Tm1 (S.Top1 x) va
            TETyCon _ va _ _ _           -> pure $! Tm1 (S.TyCon x) va
            TEDataCon _ va parent ix _ _ -> pure $! Tm1 (S.DataCon parent ix) va

    P.Let0 _ (spanToName cxt -> x) a t u -> do
      acv <- freshCV
      a  <- tyAnnot cxt a (U0 acv)
      let va = eval1 cxt a
      t <- check0 cxt t va acv
      (u, b, bcv) <- infer0 (bind0' x a va acv cxt) u
      pure $! Tm0 (S.Let0 x a t u) b bcv

    P.Let1 _ (spanToName cxt -> x) a t u -> do
      a <- tyAnnot cxt a U1
      let va = eval1 cxt a
      t <- check1 cxt t va
      let ~vt = eval1 cxt t
      (u, b) <- infer1 (define' x a va t vt cxt) u
      pure $! Tm1 (S.Let1 x a t u) b

    P.Lam _ (bindToName cxt -> x) i a t -> case i of
      P.Named{}     -> err NoNamedLambdaInference
      P.NoName Expl -> err CantInfer
      P.NoName Impl -> do
        a <- tyAnnot cxt a U1
        let va = eval1 cxt a
        let cxt' = bind1' x a va cxt
        (t, b) <- infer1 cxt' t
        let qb = quote1 cxt' b
        pure $ Tm1 (S.Lam1 x Impl a t) (V.Pi x Impl va (V.Close V.Nil qb))

    P.EmptyRec{} -> err CantInfer

    -- 0/1 ambiguity resolution based on domain universe
    P.Pi _ P.DontBind Expl a b -> do
      (a, au) <- infer1 cxt a
      case forceFU1 au of
        V.U (U0 cv) -> do
          unifyCV cv V
          b <- check1 cxt b (V.U (U0 C))
          pure $! Tm1 (S.Fun a b) (V.U (U0 C))
        V.U U1 -> do
          b <- check1 (bind1' NEmpty a (eval1 cxt a) cxt) b (V.U U1)
          pure $! Tm1 (S.Pi NEmpty Expl a b) (V.U U1)
        _ -> do
          err ExpectedType

    P.Pi _ (bindToName cxt -> x) i a b -> do
      a <- check1 cxt a (V.U U1)
      b <- check1 (bind1' x a (eval1 cxt a) cxt) b (V.U U1)
      pure $! Tm1 (S.Pi x i a b) (V.U U1)

    P.App t u i -> do
      case i of
        P.NoName Expl -> do
          t <- insertTmU' cxt $ infer cxt t
          case t of
            Tm0 t a cv -> do
              (a, b) <- ensureFun cxt a
              u      <- check0 cxt u a V
              pure $ Tm0 (S.App0 t u) b cv
            Tm1 t a -> do
              (t, x, a, b) <- coeToPi cxt a Expl t
              u            <- check1 cxt u a
              pure $ Tm1 (S.App1 t u Expl) (b $$$ eval1 cxt u)

        P.NoName Impl -> do
          (t, a)       <- infer1 cxt t
          (t, x, a, b) <- coeToPi cxt a Impl t
          u            <- check1 cxt u a
          pure $ Tm1 (S.App1 t u Impl) (b $$$ eval1 cxt u)

        P.Named (spanToRawName cxt -> x)  -> do
          (t, a)       <- insertUntilName cxt t x $ infer1 cxt t
          (t, x, a, b) <- coeToPi cxt a Impl t
          u            <- check1 cxt u a
          pure $ Tm1 (S.App1 t u Impl) (b $$$ eval1 cxt u)

    P.Ty _ (checkU -> u) -> do
      pure $! Tm1 u (V.U U1)

    P.Lift _ t -> do
      cv <- freshCV
      Tm1 <$!>  (S.Lift cv <$!> check1 cxt t (V.U (U0 cv)))
          <*!> pure (V.U U1)

    P.Up _ t -> do
      (t, a, cv) <- infer0 cxt t
      pure $! Tm1 (S.up t) (V.Lift cv a)

    P.Hole _ -> do
      err CantInfer

    P.Down _ t -> do
      (t, a) <- infer1 cxt t
      case forceFU1 a of
        V.Lift cv a -> do
          let t' = S.down t
          pure $! Tm0 t' a cv
        _ ->
          err CantSplice

    P.Rec _ [] ->
      impossible

    P.Rec _ ((spanToName cxt -> x, a):as) -> do
      (a, au) <- infer1 cxt a
      case forceFU1 au of
        V.U (U0 cv) -> do
          unifyCV cv V
          as <- checkRec0 cxt as
          pure $! Tm1 (S.Rec0 (FCons x a as)) (V.U (U0 V))
        V.U U1 -> do
          as <- checkRec1 cxt as
          pure $! Tm1 (S.Rec1 (FCons x a as)) (V.U U1)
        _ ->
          err ExpectedType

    P.RecCon _ [] ->
      impossible

    P.RecCon _ ((spanToName cxt -> x, t):ts) -> do
      t <- infer cxt t
      case t of
        Tm0 t a cv -> do
          unifyCV cv V
          (ts, as) <- inferRecCon0 cxt ts
          pure $! Tm0 (S.RecCon0 (FCons x t ts)) (V.Rec0 as) V
        Tm1 t a -> do
          err CantInferSigma

    P.Tuple _ []     -> impossible
    P.Tuple _ (t:ts) -> err CantInferTuple

    P.Field t (spanToRawName cxt -> x) -> do
      t <- infer cxt t

      case t of
        Tm0 t a _ -> do
          case forceFU1 a of
            V.Rec0 as -> do
              let go ix FNil = err $ NoSuchField x
                  go ix (FCons x' a as)
                    | NName x == x' = pure (a, ix)
                    | True          = go (ix + 1) as
              (a, ix) <- go 0 as
              pure $ Tm0 (S.Field0 t (NName x) ix) a V
            _ ->
              err ExpectedRecord

        Tm1 t a -> do
          case forceFU1 a of
            V.Rec1 (V.Close env as) -> do
              let go env ix FNil = err $ NoSuchField x
                  go env ix (FCons x' a as)
                    | NName x == x' = do
                        let va = Eval.eval1 env a
                        pure (va, ix)
                    | True = do
                        go (V.Snoc1 env (Eval.eval1 env (S.Field1 t x' ix))) (ix + 1) as
              (a, ix) <- go env 0 as
              pure $ Tm1 (S.Field1 t (NName x) ix) a
            _ ->
              err ExpectedRecord

    P.Fix _ (bindToName cxt -> x) (bindToName cxt -> y) t -> do
      a  <- freshMeta cxt (S.U (U0 V))
      cv <- freshCV
      b  <- freshMeta cxt (S.U (U0 cv))
      let va = eval1 cxt a
          vb = eval1 cxt b
      t  <- check0 (bind0' y b vb cv $ bind0' x a va V cxt) t vb cv
      pure $! Tm0 (S.Fix x y t) (V.Fun va vb) C

    P.Case _ t _ cs -> do
      error "TODO: case expressions"
      -- t <- infer cxt t
      -- case t of
      --   Tm1{} -> do
      --     err ExpectedRuntimeType
      --   Tm0 t a acv -> do
      --     unifyCV acv V
      --     bcv <- freshCV
      --     b   <- eval1 cxt <$!> freshMeta cxt (S.U (U0 bcv))
      --     cs  <- checkCases cxt cs b bcv
      --     pure $! Tm0 (S.Case t cs) b bcv

-- Top Level
--------------------------------------------------------------------------------

inferTop :: RawName -> Lvl -> P.TopLevel -> IO ()
inferTop src topSize = \case
  P.Nil ->
    pure ()

  P.Definition0 span@(Span pos _) ma rhs top -> do
    let x = unsafeSlice src span
    let cxt = emptyCxt src
    cv <- freshCV
    a  <- tyAnnot cxt ma (U0 cv)
    let va = eval1 cxt a
    rhs <- check0 cxt rhs va cv
    let ~vrhs = eval0 cxt rhs
    newTopName x topSize
    newTop topSize (TEDef0 a va rhs vrhs cv (NName x) pos)
    inferTop src (topSize + 1) top

  P.Definition1 span@(Span pos _) ma rhs top -> do
    let x = unsafeSlice src span
    let cxt = emptyCxt src
    a  <- tyAnnot cxt ma U1
    let va = eval1 cxt a
    rhs <- check1 cxt rhs va
    let ~vrhs = eval1 cxt rhs
    newTopName x topSize
    newTop topSize (TEDef1 a va rhs vrhs (NName x) pos)
    inferTop src (topSize + 1) top

  P.DataDecl pos x params cons top -> do
    -- TODO
    inferTop src topSize top
