{-# options_ghc -Wno-unused-imports -Wno-type-defaults #-}

module Elaboration where

import Control.Monad

import qualified Data.HashMap.Strict as HM
import qualified FlatParse.Stateful  as FP

import qualified Syntax              as S
import qualified Values              as V
import qualified Presyntax           as P
import qualified Evaluation          as Eval
import qualified UnifyCxt            as Unif
import qualified Unification         as Unif

import Common
import Cxt
import ElabState
import InCxt
import Exceptions
import Pretty

--------------------------------------------------------------------------------

{-
High-level design:

- lower : coercing from level 1 to level 0

- check0 :
  - purely presyntax-directed, matches canonical cases + let
  - refines checking type if necessary
  - no need for matching on checking types, because level 0 does not have type-directed
    elaboration!
  - why do we even have check0? a) because we can resolve stage ambiguity b) efficiency

- check1 :
  - Mixed presyntax + type directed, canonical cases + let
  - performs insertions (<_>, implicit lam)

- infer0 :
  - purely presyntax-directed, matches *level-ambiguous* canonical cases only

- infer1 :
  - purely presyntax-directed, matches *level-ambiguous* canonical cases only

- infer :
  - purely presyntax-directed


-}



--------------------------------------------------------------------------------

elabError :: Cxt -> P.Tm -> ElabEx -> IO a
elabError cxt t err = throwIO $ ElabError (unifyCxt cxt) t err
{-# inline elabError #-}

unify1 :: Cxt -> P.Tm -> V.Val1 -> V.Val1 -> IO ()
unify1 cxt topT t t' =
  Unif.unify1 (unifyCxt cxt) CSRigid t t'
  `catch`
  \(e :: UnifyInner) -> elabError cxt topT $ UnifyOuter t t' e

-- Converting spans to names
--------------------------------------------------------------------------------

unsafeSlice :: RawName -> Span -> RawName
unsafeSlice x span = coerce $ FP.unsafeSlice (coerce x) span
{-# inline unsafeSlice #-}

spanToRawName :: Cxt -> Span -> RawName
spanToRawName cxt span = unsafeSlice (cxt^.src) span
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

insert' :: Dbg => Cxt -> IO (S.Tm1, V.Ty) -> IO (S.Tm1, V.Ty)
insert' cxt inf = do {(t, a) <- inf; go t a} where
  go :: S.Tm1 -> V.Ty -> IO (S.Tm1, V.Ty)
  go t topA = case forceFU1 topA of
    V.Pi x Impl a b -> do
      m <- freshMeta cxt a
      go (S.App1 t m Impl) (b $$$ eval1 cxt m)
    topA ->
      pure (t, topA)

insertTmU' :: Dbg => Cxt -> IO TmU -> IO TmU
insertTmU' cxt inf = inf >>= \case
  Tm0 t a cv -> pure $! Tm0 t a cv
  Tm1 t a    -> do {(t, a) <- insert' cxt (pure (t, a)); pure (Tm1 t a)}

insertTmU :: Dbg => Cxt -> IO TmU -> IO TmU
insertTmU cxt inf = inf >>= \case
  Tm0 t a cv -> pure $! Tm0 t a cv
  Tm1 t a    -> do {(t, a) <- insert cxt (pure (t, a)); pure (Tm1 t a)}

insertUntilName :: Dbg => Cxt -> P.Tm -> RawName -> IO (S.Tm1, V.Ty) -> IO (S.Tm1, V.Ty)
insertUntilName cxt topT topX inf = do {(t, a) <- inf; go t a} where
  go :: S.Tm1 -> V.Ty -> IO (S.Tm1, V.Ty)
  go t topA = case forceFU1 topA of
    V.Pi x Impl a b -> do
      if NName topX == x then
        pure (t, topA)
      else do
        m <- freshMeta cxt a
        go (S.App1 t m Impl) (b $$ eval1 cxt m)
    _ ->
      elabError cxt topT $ NoSuchArgument topX

insert :: Dbg => Cxt -> IO (S.Tm1, V.Ty) -> IO (S.Tm1, V.Ty)
insert cxt inf = inf >>= \case
  res@(S.Lam1 _ Impl a t, va) -> pure res
  res                         -> insert' cxt (pure res)
{-# inline insert #-}

-- coercion
--------------------------------------------------------------------------------

-- | Heterogeneous coercion would be a bit more efficient, but more cases to implement.
coe :: Dbg => Cxt -> P.Tm -> S.Tm1 -> V.Ty -> V.Ty -> IO S.Tm1
coe cxt topT t a a' = do
  maybe t id <$> go cxt t a a' where

  -- ugly helper for record structural rule
  goRec1 :: Dbg => Cxt -> S.Tm1 -> Int -> V.Close (Fields S.Tm1)
                -> V.Close (Fields S.Tm1) -> IO (Fields (Bool, S.Tm1))
  goRec1 cxt t ix (V.Close e as) (V.Close e' as') = case (as, as') of
    (FNil, FNil) -> pure FNil
    (FCons x a as, FCons x' a' as') -> do
      when (x /= x') $ throwIO $ FieldNameMismatch x' x
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
      throwIO $ UnifyInner (unifyCxt cxt)
                           (Unify1 (V.Rec1 (V.Close e as)) (V.Rec1 (V.Close e' as')))


  -- lifting helpers
  --------------------------------------------------------------------------------

  -- compute ^(a -> b) to ^a -> ^b
  liftFun :: Cxt -> V.Ty -> V.Ty -> V.CV -> V.Ty
  liftFun cxt a b bcv =
    let l'   = cxt^.lvl + 1
        qbcv = Eval.quote1 l' UnfoldNone bcv
        qb   = Eval.quote1 l' UnfoldNone b
    in V.Pi NEmpty Expl (V.Lift V.Val a) (V.Close (cxt^.env) (S.Lift qbcv qb))

  --  ^(a -> b) -> ^a -> ^b
  --  f ↦ λ a. <~f ~a>
  upFun :: Cxt -> V.Ty -> S.Tm1 -> S.Tm1
  upFun cxt a t =
    S.Lam1 NEmpty Expl (S.Lift S.Val (quote1 cxt a))
       (S.Up (S.App0 (S.Wk10 (S.down t)) (S.Down (S.Var1 0))))

  --  (^a -> ^b) -> ^(a -> b)
  -- f ↦ <λ a. ~(f <a>)>
  downFun :: Cxt -> V.Ty -> S.Tm1 -> S.Tm1
  downFun cxt a t =
    S.Up (S.Lam0 NEmpty (quote1 cxt a)
         (S.Down (S.App1 (S.Wk01 t) (S.Up (S.Var0 0)) Expl)))

  -- compute ^(Rec0 as) to Rec1 (map ^ as)
  liftRec0 :: Cxt -> Fields V.Ty -> V.Ty
  liftRec0 cxt as = V.Rec1 (V.Close (cxt^.env) (go (cxt^.lvl) as)) where
    go :: Lvl -> Fields V.Ty -> Fields S.Tm1
    go l FNil           = FNil
    go l (FCons x a as) =
      FCons x (S.Lift S.Val (Eval.quote1 l UnfoldNone a)) (go (l + 1) as)

  --  ^(Rec as) -> Rec (map ^ as)
  upRecCon0 :: S.Tm1 -> Fields a -> S.Tm1
  upRecCon0 t as = S.RecCon1 (go 0 as) where
    go ix FNil           = FNil
    go ix (FCons x _ as) = FCons x (S.Up (S.Field0 (S.down t) x ix)) (go (ix + 1) as)

  -- Rec (map ^ as) -> ^(Rec as)
  downRecCon0 :: Fields a -> S.Tm1 -> S.Tm1
  downRecCon0 as t = S.Up (S.RecCon0 (go 0 as)) where
    go ix FNil           = FNil
    go ix (FCons x _ as) = FCons x (S.Down (S.Field1 t x ix)) (go (ix + 1) as)

  go :: Dbg => Cxt -> S.Tm1 -> V.Ty -> V.Ty -> IO (Maybe S.Tm1)
  go cxt t a a' = do
    -- traceShowM ("coe.go", showTm1 cxt t, showVal1' cxt a, showVal1' cxt a')
    -- traceShowM ("coe.go", t, a, a')
    case (forceFU1 a, forceFU1 a') of

      -- if we have flex on one side, we just switch to unification
      -- TODO (later): postpone coe on flex RHS
      (a@(V.Flex x sp), a')   -> Nothing <$ unify1 cxt topT a a'
      (a, a'@(V.Flex x' sp')) -> Nothing <$ unify1 cxt topT a a'

      -- universe subtyping
      (V.U0 cv, V.U1) -> do
        pure $! Just $! S.Lift (quote1 cxt cv) t

      -- structural rules
      ------------------------------------------------------------

      (V.Pi x i a b, V.Pi x' i' a' b') -> do
        when (i /= i') $ throwIO $ IcitMismatch i i'
        let cxt' = bind1' x (quote1 cxt a') a' cxt
        coev0 <- go cxt' (S.Var1 0) a' a
        case coev0 of
          Nothing ->
            (S.Lam1 x i (quote1 cxt a') <$!>) <$!>
              go cxt' (S.App1 (S.Wk11 t) (S.Var1 0) i)
                      (b  $$$ eval1 cxt' (S.Var1 0))
                      (b' $$  V.Var1 (cxt^.lvl))

          Just coev0 ->
            (Just . S.Lam1 x i (quote1 cxt a')) <$!>
              coe cxt' topT (S.App1 (S.Wk11 t) coev0 i) (b  $$$ eval1 cxt' coev0)
                                                        (b' $$  V.Var1 (cxt^.lvl))

      (V.Rec1 as, V.Rec1 as') -> do
        fs <- goRec1 cxt t 0 as as'
        if any fst fs
          then pure $! Just $! S.RecCon1 (snd <$> fs)
          else pure Nothing

      -- lift preservation
      ------------------------------------------------------------

      (V.Lift _ (V.Fun a b bcv), a') -> do
        Just <$!> coe cxt topT (upFun cxt a t) (liftFun cxt a b bcv) a'

      (a, V.Lift _ (V.Fun a' b' b'cv)) -> do
        Just . downFun cxt a' <$!> coe cxt topT t a (liftFun cxt a' b' b'cv)

      (V.Lift _ (V.Rec0 as), a') -> do
        Just <$!> coe cxt topT (upRecCon0 t as) (liftRec0 cxt as) a'

      (a, V.Lift _ (V.Rec0 as)) -> do
        Just . downRecCon0 as <$!> coe cxt topT t a (liftRec0 cxt as)

      -- refining under lift
      ------------------------------------------------------------

      (V.Pi x Expl a b, V.Lift cv a') -> do
        unify1 cxt topT cv V.Comp
        a1'   <- eval1 cxt <$!> freshMeta cxt (V.U0 V.Val)
        a2'cv <- freshCV cxt
        let va2'cv = eval1 cxt a2'cv
        a2'   <- eval1 cxt <$!> freshMeta cxt (V.U0 va2'cv)
        unify1 cxt topT a' (V.Fun a1' a2' va2'cv)
        go cxt t (V.Pi x Expl a b) (V.Lift V.Comp (V.Fun a1' a2' va2'cv))

      (V.Lift cv a, V.Pi x' Expl a' b') -> do
        unify1 cxt topT cv V.Comp
        a1 <- eval1 cxt <$!> freshMeta cxt (V.U0 V.Val)
        a2cv <- freshCV cxt
        let va2cv = eval1 cxt a2cv
        a2 <- eval1 cxt <$!> freshMeta cxt (V.U0 va2cv)
        unify1 cxt topT a (V.Fun a1 a2 va2cv)
        go cxt t (V.Lift V.Comp (V.Fun a1 a2 va2cv)) (V.Pi x' Expl a' b')

      -- TODO: same for non-empty Rec

      (V.Rec1 (V.Close _ FNil), V.Lift cv a') -> do
        unify1 cxt topT cv V.Val
        unify1 cxt topT a' (V.Rec0 FNil)
        pure $! Just $! S.Up (S.RecCon0 FNil)

      (V.Lift cv a, V.Rec1 (V.Close _ FNil)) -> do
        unify1 cxt topT cv V.Val
        unify1 cxt topT a (V.Rec0 FNil)
        pure $! Just $! S.RecCon1 FNil

      -- fallback
      ------------------------------------------------------------

      (a, a') -> do
        -- traceShowM ("coe fallback", a, a')
        Nothing <$ unify1 cxt topT a a'

tyAnnot :: Cxt -> Maybe P.Tm -> V.Ty -> IO S.Ty
tyAnnot cxt ma un = maybe (freshMeta cxt un) (\a -> check1 cxt a un) ma
{-# inline tyAnnot #-}

-- Checking
--------------------------------------------------------------------------------

check0 :: Dbg => Cxt -> P.Tm -> V.Ty -> V.CV -> IO S.Tm0
check0 cxt topT topA cv = do
  -- traceShowM ("check0", topT, forceFU1 topA, forceFU1 cv)
  case (topT, forceFU1 topA, forceFU1 cv) of
    (P.Lam _ (bindToName cxt -> x) i ma t, a', !cv) -> do
      case i of P.NoName Expl -> pure ()
                _             -> elabError cxt topT NoImplicitLam0
      (a', b', b'cv) <- ensureFun cxt topT a' cv
      let qa' = quote1 cxt a'
      S.Lam0 x qa' <$!>
        check0 (bind0' x qa' a' S.Val V.Val cxt) t b' b'cv

    (P.RecCon _ ts, topA@(V.Rec0 as), _) -> do

      let go [] FNil = pure FNil
          go ((x, t): ts) (FCons x' a as) = do
            let rn = spanToRawName cxt x
            unless (NName rn == x') $
              elabError cxt topT (NoSuchField (quote1 cxt topA) rn)
            t <- check0 cxt t a V.Val
            ts <- go ts as
            pure $! FCons x' t ts
          go ((x, t):ts) FNil =
            elabError cxt topT ExpectedEmptyRecCon
          go _ _ =
            elabError cxt topT ExpectedNonEmptyRecCon

      S.RecCon0 <$!> go ts as

    (P.Tuple _ ts, V.Rec0 as, _) -> do

      let go [] FNil = pure FNil
          go (t:ts) (FCons x' a as) =
            FCons x' <$!> check0 cxt t a V.Val <*!> go ts as
          go (t:ts) FNil =
            elabError cxt topT ExpectedEmptyRecCon
          go _ _ =
            elabError cxt topT ExpectedNonEmptyRecCon

      S.RecCon0 <$!> go ts as

    (P.EmptyRec _, a, cv) -> do
      unify1 cxt topT cv V.Val
      unify1 cxt topT a (V.Rec0 FNil)
      pure $! S.RecCon0 FNil

    (P.Let0 _ (spanToName cxt -> x) ma t u, topA, cv) -> do
      acv  <- freshCV cxt
      let vacv = eval1 cxt acv
      a    <- tyAnnot cxt ma (V.U0 vacv)
      let va = eval1 cxt a
      t <- check0 cxt t va vacv
      u <- check0 (bind0' x a va acv vacv cxt) u topA cv
      pure $! S.Let0 x a t u

    (P.Hole _, topA, cv) -> do
      S.down <$!> freshMeta cxt (V.Lift cv topA)

    (P.Down _ t, topA, cv) -> do
      S.down <$!> check1 cxt t (V.Lift cv topA)

    (t, topA, cv) -> do
      (t, a, cv') <- infer0 cxt t
      unify1 cxt topT cv' cv
      unify1 cxt topT a topA
      pure t

checkRec0 :: Dbg => Cxt -> [(Span, P.Tm)] -> IO (Fields S.Ty)
checkRec0 cxt as = go as where
  go []          = pure FNil
  go ((x, a):as) = FCons (spanToName cxt x) <$!> check1 cxt a (V.U0 V.Val)
                                            <*!> go as

checkRec1 :: Dbg => Cxt -> [(Span, P.Tm)] -> IO (Fields S.Tm1)
checkRec1 cxt as = go cxt as where
  go cxt [] = pure FNil
  go cxt ((spanToName cxt -> x, a):as) = do
    a  <- check1 cxt a V.U1
    as <- go (bind1' x a (eval1 cxt a) cxt) as
    pure $! FCons x a as

checkRecCon1 :: Dbg => Cxt -> P.Tm -> V.Ty -> [(Span, P.Tm)] -> V.Close (Fields S.Ty) -> IO (Fields S.Tm1)
checkRecCon1 cxt topT topA ts (V.Close env as) = case (ts, as) of
  ([], FNil) -> pure FNil
  ((spanToRawName cxt -> x, t):ts, FCons x' a as) -> do
    unless (NName x == x') $
      elabError cxt topT (NoSuchField (quote1 cxt topA) x)
    let va = Eval.eval1 env a
        qa = Eval.quote1 (cxt^.lvl) UnfoldNone va
    t  <- check1 cxt t va
    let ~vt = eval1 cxt t
    ts <- checkRecCon1 cxt topT topA ts (V.Close (V.Snoc1 env vt) as)
    pure $! FCons x' t ts
  ([], FCons{}) ->
    elabError cxt topT ExpectedNonEmptyRecCon
  _ ->
    elabError cxt topT ExpectedEmptyRecCon

checkTuple1 :: Dbg => Cxt -> P.Tm -> [P.Tm] -> V.Close (Fields S.Ty) -> IO (Fields S.Tm1)
checkTuple1 cxt topT ts (V.Close env as) = case (ts, as) of
  ([], FNil) -> pure FNil
  (t:ts, FCons x' a as) -> do
    let va = Eval.eval1 env a
        qa = Eval.quote1 (cxt^.lvl) UnfoldNone va
    t  <- check1 cxt t va
    let ~vt = eval1 cxt t
    ts <- checkTuple1 cxt topT ts (V.Close (V.Snoc1 env vt) as)
    pure $! FCons x' t ts
  ([], FCons{}) ->
    elabError cxt topT ExpectedNonEmptyRecCon
  _ ->
    elabError cxt topT ExpectedEmptyRecCon

check1 :: Dbg => Cxt -> P.Tm -> V.Ty -> IO S.Tm1
check1 cxt topT topA = do
  -- traceShowM ("check1", topT, showVal1' cxt topA)
  -- traceShowM ("check1", topT, forceFU1 topA)
  case (topT, forceFU1 topA) of

    (P.Lam _ (bindToName cxt -> x) i ma t, V.Pi x' i' a' b')
      | case i of P.NoName i                    -> i == i'
                  P.Named (spanToName cxt -> x) -> x == x' -> do
      let qa' = quote1 cxt a'
      S.Lam1 x i' qa' <$!>
        check1 (bind1' x qa' a' cxt) t (b' $$ V.Var1 (cxt^.lvl))

    (t, V.Pi x' Impl a' b') -> do
      let qa' = quote1 cxt a'
      S.Lam1 x' Impl qa' <$!>
        check1 (newBinder x' qa' cxt) t (b' $$ V.Var1 (cxt^.lvl))

    (P.Pi _ P.DontBind Expl a b, V.U0 cv) -> do
      unify1 cxt topT cv V.Comp
      a <- check1 cxt a (V.U0 V.Val)
      bcv <- freshCV cxt
      let vbcv = eval1 cxt bcv
      b <- check1 cxt b (V.U0 vbcv)
      pure $! S.Fun a b bcv

    (P.Pi _ (bindToName cxt -> x) i a b, V.U1) -> do
      a <- check1 cxt a V.U1
      b <- check1 (bind1' x a (eval1 cxt a) cxt) b V.U1
      pure $! S.Pi x i a b

    (P.Rec _ as, V.U0 cv) -> do
      unify1 cxt topT cv V.Val
      S.Rec0 <$!> checkRec0 cxt as

    (P.Rec _ as, V.U1) -> do
      S.Rec1 <$!> checkRec1 cxt as

    (P.EmptyRec _, V.U0 cv) -> do
      unify1 cxt topT cv V.Val
      pure $! S.Rec0 FNil

    (P.EmptyRec _, V.U1) -> do
      pure $! S.Rec1 FNil

    (P.Lift _ t, V.U1) -> do
      cv <- freshCV cxt
      S.Lift cv <$!> check1 cxt t (V.U0 (eval1 cxt cv))

    (P.RecCon _ ts, topA@(V.Rec1 as)) -> do
      S.RecCon1 <$!> checkRecCon1 cxt topT topA ts as

    (P.Tuple _ ts, V.Rec1 as) -> do
      S.RecCon1 <$!> checkTuple1 cxt topT ts as

    (P.EmptyRec _, V.Rec1 (V.Close _ as)) -> do
      case as of FNil -> pure ()
                 _    -> elabError cxt topT ExpectedEmptyRec
      pure $! S.RecCon1 FNil

    (P.Let1 _ (spanToName cxt -> x) ma t u, topA) -> do
      a <- tyAnnot cxt ma V.U1
      let va = eval1 cxt a
      t <- check1 cxt t va
      u <- check1 (define' x a va t (eval1 cxt t) cxt) u topA
      pure $! S.Let1 x a t u

    (P.Up _ t, V.Lift cv topA) -> do
      S.up <$!> check0 cxt t topA cv

    (t, V.Lift cv topA) -> do
      -- traceShowM ("check with lift", forceFU1 topA)
      S.up <$!> check0 cxt t topA cv

    (P.Hole _, topA) -> do
      freshMeta cxt topA

    (t, topA) -> do
      (t, a) <- insert cxt $ infer1 cxt t
      coe cxt topT t a topA

data TmU = Tm0 S.Tm0 V.Ty V.CV | Tm1 S.Tm1 V.Ty deriving Show

-- -- TODO: handle record constructors
-- infer0check :: Dbg => Cxt -> P.Tm -> IO (S.Tm0, V.Ty, V.CV)
-- infer0check cxt topT = case topT of

--   P.Lam _ (bindToName cxt -> x) i a t -> case i of
--     P.Named{}     -> elabError cxt topT NoImplicitLam0
--     P.NoName Impl -> elabError cxt topT NoImplicitLam0
--     P.NoName Expl -> do
--       a <- tyAnnot cxt a (V.U0 V.Val)
--       let va   = eval1 cxt a
--       let cxt' = bind0' x a va S.Val V.Val cxt
--       (t, b, bcv) <- infer0 cxt' t
--       pure $! (S.Lam0 x a t, V.Fun va b bcv, V.Comp)

--   topT -> insertTmU cxt (infer cxt topT) >>= \case
--     Tm0 t a cv ->
--       pure (t, a, cv)
--     Tm1 t a -> case forceFU1 a of
--       V.Lift cv a -> do
--         let t' = S.down t
--         pure (t', a, cv)

--       -- PRUNING needed (often)
--       a -> do
--         cv <- freshCV cxt
--         let vcv = eval1 cxt cv
--         a' <- eval1 cxt <$!> freshMeta cxt (V.U0 vcv)
--         t  <- S.down <$!> coe cxt topT t a (V.Lift vcv a')
--         pure (t, a', vcv)

-- TODO: handle records
infer0 :: Dbg => Cxt -> P.Tm -> IO (S.Tm0, V.Ty, V.CV)
infer0 cxt topT = case topT of

  P.Lam _ (bindToName cxt -> x) i a t -> case i of
    P.Named{}     -> elabError cxt topT NoImplicitLam0
    P.NoName Impl -> elabError cxt topT NoImplicitLam0
    P.NoName Expl -> do
      a  <- tyAnnot cxt a (V.U0 V.Val)
      cv <- freshCV cxt
      let vcv = eval1 cxt cv
      b  <- freshMeta cxt (V.U0 vcv)
      let vb = eval1 cxt b
      let va = eval1 cxt a
      let cxt' = bind0' x a va S.Val V.Val cxt
      t <- check0 cxt' t va vcv
      pure $! (S.Lam0 x a t, V.Fun va vb vcv, V.Comp)

  topT -> insertTmU cxt (infer cxt topT) >>= \case
    Tm0 t a cv ->
      pure (t, a, cv)
    Tm1 t a -> case forceFU1 a of
      V.Lift cv a -> do
        let t' = S.down t
        pure (t', a, cv)

      -- PRUNING needed
      a -> do
        cv <- freshCV cxt
        let vcv = eval1 cxt cv
        a' <- eval1 cxt <$!> freshMeta cxt (V.U0 vcv)
        t  <- S.down <$!> coe cxt topT t a (V.Lift vcv a')
        pure (t, a', vcv)

infer1 :: Dbg => Cxt -> P.Tm -> IO (S.Tm1, V.Ty)
infer1 cxt topT = case topT of

  P.Lam _ (bindToName cxt -> x) i a t -> case i of
    P.Named{}  -> elabError cxt topT NoNamedLambdaInference
    P.NoName i -> do
      a <- tyAnnot cxt a V.U1
      let va   = eval1 cxt a
      let cxt' = bind1' x a va cxt
      (t, b) <- insert cxt' $ infer1 cxt' t
      let qb = quote1 cxt' b
      pure $! (S.Lam1 x i a t, V.Pi x i va (V.Close (cxt^.env) qb))

  t -> infer cxt t >>= \case
    Tm0 t a cv -> do
      -- traceShowM ("inferred", showTm1 cxt (S.up t), showVal1 cxt a, showVal1 cxt cv)
      -- traceShowM ("inferred", showTm1 cxt (S.up t), forceFU1 a)
      let t' = S.up t in pure (t', V.Lift cv a)
    Tm1 t a -> pure (t, a)

inferRecCon0 :: Dbg => Cxt -> P.Tm -> [(Span, P.Tm)] -> IO (Fields S.Tm0, Fields V.Ty)
inferRecCon0 cxt topT cs = case cs of
  [] -> pure (FNil, FNil)
  (spanToName cxt -> x, t):cs -> do
    (t, a, cv) <- infer0 cxt t
    unify1 cxt topT cv V.Val
    (ts, as) <- inferRecCon0 cxt topT cs
    pure (FCons x t ts, FCons x a as)

ensureFun :: Dbg => Cxt -> P.Tm -> V.Ty -> V.CV -> IO (V.Ty, V.Ty, V.CV)
ensureFun cxt topT a acv = case forceFU1 a of
  V.Fun a b bcv -> pure (a, b, bcv)
  a -> do
    unify1 cxt topT acv V.Comp
    a'   <- eval1 cxt <$!> freshMeta cxt (V.U0 V.Val)
    b'cv <- freshCV cxt
    let vb'cv = eval1 cxt b'cv
    b'   <- eval1 cxt <$!> freshMeta cxt (V.U0 vb'cv)
    unify1 cxt topT a (V.Fun a' b' vb'cv)
    pure (a', b', vb'cv)

coeToPi :: Dbg => Cxt -> P.Tm -> V.Ty -> Icit -> S.Tm1 -> IO (S.Tm1, Name, V.Ty, V.Close S.Ty)
coeToPi cxt topT a i t = case forceFU1 a of
  V.Pi x i' a b -> do
    when (i /= i) $ throwIO $ IcitMismatch i' i
    pure (t, x, a, b)
  a -> do
    a' <- freshMeta cxt V.U1
    let va' = eval1 cxt a'
    b' <- V.Close (cxt^.env) <$!> freshMeta (bind1' NX a' va' cxt) V.U1
    t  <- coe cxt topT t a (V.Pi NX i va' b')
    pure (t, NX, va', b')

infer :: Dbg => Cxt -> P.Tm -> IO TmU
infer cxt topT = do

  let err :: forall a. ElabEx -> IO a
      err = elabError cxt topT; {-# inline err #-}

  -- traceShowM ("infer", topT)
  res <- case topT of

    P.Var (spanToRawName cxt -> x) -> do
      case HM.lookup x (cxt^.nameTable) of
        Just ni -> case ni of
          NameInfo0 x a cv -> pure $! Tm0 (S.Var0 (lvlToIx (cxt^.lvl) x)) a cv
          NameInfo1 x a    -> pure $! Tm1 (S.Var1 (lvlToIx (cxt^.lvl) x)) a
        Nothing -> lookupTopName x >>= \case
          Nothing -> err $ NameNotInScope x
          Just x  -> readTop x >>= \case
            TEDef0 _ va _ _ cv _ _       -> pure $! Tm0 (S.Top0 x) va cv
            TEDef1 _ va _ _ _ _          -> pure $! Tm1 (S.Top1 x) va
            TETyCon _ va _ _ _           -> pure $! Tm1 (S.TyCon x) va
            TEDataCon _ va parent ix _ _ -> pure $! Tm1 (S.DataCon parent ix) va

    P.Let0 _ (spanToName cxt -> x) a t u -> do
      acv <- freshCV cxt
      let vacv = eval1 cxt acv
      a  <- tyAnnot cxt a (V.U0 vacv)
      let va = eval1 cxt a
      t <- check0 cxt t va vacv
      (u, b, bcv) <- infer0 (bind0' x a va acv vacv cxt) u
      pure $! Tm0 (S.Let0 x a t u) b bcv

    P.Let1 _ (spanToName cxt -> x) a t u -> do
      a <- tyAnnot cxt a V.U1
      let va = eval1 cxt a
      t <- check1 cxt t va
      let ~vt = eval1 cxt t
      (u, b) <- infer1 (define' x a va t vt cxt) u
      pure $! Tm1 (S.Let1 x a t u) b

    P.Lam _ (bindToName cxt -> x) i a t -> case i of
      P.Named{}     -> err NoNamedLambdaInference
      P.NoName Expl -> err CantInfer
      P.NoName Impl -> do
        a <- tyAnnot cxt a V.U1
        let va = eval1 cxt a
        let cxt' = bind1' x a va cxt
        (t, b) <- insert cxt' $ infer1 cxt' t
        let qb = quote1 cxt' b
        pure $ Tm1 (S.Lam1 x Impl a t) (V.Pi x Impl va (V.Close (cxt^.env) qb))

    P.EmptyRec{} -> err CantInfer

    -- 0/1 ambiguity resolution based on domain universe
    P.Pi _ P.DontBind Expl a b -> do
      (a, au) <- insert cxt $ infer1 cxt a
      case forceFU1 au of
        V.U0 cv -> do
          unify1 cxt topT cv V.Val
          bcv <- freshCV cxt
          let vbcv = eval1 cxt bcv
          b <- check1 cxt b (V.U0 vbcv)
          pure $! Tm1 (S.Fun a b bcv) (V.U0 V.Comp)
        V.U1 -> do
          b <- check1 (bind1' NEmpty a (eval1 cxt a) cxt) b V.U1
          pure $! Tm1 (S.Pi NEmpty Expl a b) V.U1
        au -> do
          err (ExpectedType (quote1 cxt au))

    P.Pi _ (bindToName cxt -> x) i a b -> do
      a <- check1 cxt a V.U1
      b <- check1 (bind1' x a (eval1 cxt a) cxt) b V.U1
      pure $! Tm1 (S.Pi x i a b) V.U1

    P.App t u i -> do
      case i of
        P.NoName Expl -> do
          t <- insertTmU' cxt $ infer cxt t
          case t of
            Tm0 t a acv -> do
              (a, b, bcv) <- ensureFun cxt topT a acv
              u <- check0 cxt u a V.Val
              pure $ Tm0 (S.App0 t u) b bcv
            Tm1 t a -> do
              (t, x, a, b) <- coeToPi cxt topT a Expl t
              u            <- check1 cxt u a
              pure $ Tm1 (S.App1 t u Expl) (b $$$ eval1 cxt u)

        P.NoName Impl -> do
          (t, a)       <- infer1 cxt t
          (t, x, a, b) <- coeToPi cxt topT a Impl t
          u            <- check1 cxt u a
          pure $ Tm1 (S.App1 t u Impl) (b $$$ eval1 cxt u)

        P.Named (spanToRawName cxt -> x)  -> do
          (t, a)       <- insertUntilName cxt t x $ infer1 cxt t
          (t, x, a, b) <- coeToPi cxt topT a Impl t
          u            <- check1 cxt u a
          pure $ Tm1 (S.App1 t u Impl) (b $$$ eval1 cxt u)

    P.Lift _ t -> do
      cv <- freshCV cxt
      let vcv = eval1 cxt cv
      Tm1 <$!> (S.Lift cv <$!> check1 cxt t (V.U0 vcv))
          <*!> pure V.U1

    P.Up _ t -> do
      (t, a, cv) <- infer0 cxt t
      pure $! Tm1 (S.up t) (V.Lift cv a)

    P.Hole _ -> do
      err CantInfer

    P.Down _ t -> do
      (t, a) <- insert cxt $ infer1 cxt t
      case forceFU1 a of
        V.Lift cv a -> do
          let t' = S.down t
          pure $! Tm0 t' a cv
        a -> do
          cv <- freshCV cxt
          let vcv = eval1 cxt cv
          a' <- eval1 cxt <$!> freshMeta cxt (V.U0 vcv)
          t  <- S.down <$!> coe cxt topT t a (V.Lift vcv a')
          pure (Tm0 t a' vcv)

    P.Rec _ [] ->
      impossible

    P.Rec _ ((spanToName cxt -> x, a):as) -> do
      (a, au) <- insert cxt $ infer1 cxt a
      case forceFU1 au of
        V.U0 cv -> do
          unify1 cxt topT cv V.Val
          as <- checkRec0 cxt as
          pure $! Tm1 (S.Rec0 (FCons x a as)) (V.U0 V.Val)
        V.U1 -> do
          as <- checkRec1 (bind1' x a (eval1 cxt a) cxt) as
          pure $! Tm1 (S.Rec1 (FCons x a as)) V.U1
        au ->
          err (ExpectedType (quote1 cxt au))

    P.RecCon _ [] ->
      impossible

    P.RecCon _ ((spanToName cxt -> x, t):ts) -> do
      t <- insertTmU cxt $ infer cxt t
      case t of
        Tm0 t a cv -> do
          unify1 cxt topT cv V.Val
          (ts, as) <- inferRecCon0 cxt topT ts
          pure $! Tm0 (S.RecCon0 (FCons x t ts)) (V.Rec0 as) V.Val
        Tm1 t a -> do
          err CantInferRec1

    P.Tuple _ []     -> impossible
    P.Tuple _ (t:ts) -> err CantInferTuple

    P.Field t (spanToRawName cxt -> x) -> do
      t <- insertTmU cxt $ infer cxt t
      case t of
        Tm0 t a _ -> do
          case forceFU1 a of
            topA@(V.Rec0 as) -> do
              let go ix FNil = err $ NoSuchField (quote1 cxt topA) x
                  go ix (FCons x' a as)
                    | NName x == x' = pure (a, ix)
                    | True          = go (ix + 1) as
              (a, ix) <- go 0 as
              pure $ Tm0 (S.Field0 t (NName x) ix) a V.Val
            topA ->
              err $! ExpectedRecord (quote1 cxt topA)

        Tm1 t a -> do
          case forceFU1 a of
            topA@(V.Rec1 (V.Close env as)) -> do
              let go env ix FNil = err $ NoSuchField (quote1 cxt topA) x
                  go env ix (FCons x' a as)
                    | NName x == x' = do
                        let va = Eval.eval1 env a
                        pure (va, ix)
                    | True = do
                        go (V.Snoc1 env (eval1 cxt (S.Field1 t x' ix))) (ix + 1) as
              (a, ix) <- go env 0 as
              pure $ Tm1 (S.Field1 t (NName x) ix) a
            topA ->
              err $! ExpectedRecord (quote1 cxt topA)

    P.U0 _ cv -> do
      cv <- check1 cxt cv V.CV
      pure $! Tm1 (S.U0 cv) V.U1

    P.U1 _   -> pure $! Tm1 S.U1 V.U1
    P.CV _   -> pure $! Tm1 S.CV V.U1
    P.Comp _ -> pure $! Tm1 S.Comp V.CV
    P.Val _  -> pure $! Tm1 S.Val V.CV

    P.Case _ t _ cs -> do
      error "case not supported"

    P.Int _ -> do
      pure $! Tm1 S.Int (V.U0 V.Val)

    P.IntLit _ n -> do
      pure $! Tm0 (S.IntLit n) V.Int V.Val

    P.Add t u -> do
      t <- check0 cxt t V.Int V.Val
      u <- check0 cxt u V.Int V.Val
      pure $! Tm0 (S.Add t u) V.Int V.Val

    P.Mul t u -> do
      t <- check0 cxt t V.Int V.Val
      u <- check0 cxt u V.Int V.Val
      pure $! Tm0 (S.Mul t u) V.Int V.Val

    P.Sub t u -> do
      t <- check0 cxt t V.Int V.Val
      u <- check0 cxt u V.Int V.Val
      pure $! Tm0 (S.Sub t u) V.Int V.Val

  -- traceShowM ("inferred", res)
  pure res


-- Top Level
--------------------------------------------------------------------------------

inferTop :: Dbg => RawName -> P.TopLevel -> IO ()
inferTop src = \case
  P.Nil ->
    pure ()

  P.Definition0 span@(Span pos _) ma rhs top -> do
    let x   = unsafeSlice src span
    let cxt = emptyCxt src

    (rhs, a, va, vcv) <- case ma of
      Just a  -> do
        cv <- freshCV cxt
        let vcv = eval1 cxt cv
        a <- check1 cxt a (V.U0 vcv)
        let va = eval1 cxt a
        rhs <- check0 cxt rhs va vcv
        pure (rhs, a, va, vcv)
      Nothing -> do
        (rhs, va, cv) <- infer0 cxt rhs
        let qa = quote1 cxt va
        -- traceShowM ("infertop0", va, qa)
        pure (rhs, qa, va, cv)

    let ~vrhs = eval0 cxt rhs
    lvl <- pushTop (TEDef0 a va rhs vrhs vcv (NName x) pos)
    newTopName x lvl
    inferTop src top

  P.Definition1 span@(Span pos _) ma rhs top -> do
    let x   = unsafeSlice src span
    let cxt = emptyCxt src

    (rhs, a, va) <- case ma of
      Just a  -> do
        a <- check1 cxt a V.U1
        let va = eval1 cxt a
        rhs <- check1 cxt rhs va
        pure (rhs, a, va)
      Nothing -> do
        (rhs, va) <- infer1 cxt rhs
        let qa = quote1 cxt va
        pure (rhs, qa, va)

    let ~vrhs = eval1 cxt rhs
    lvl <- pushTop (TEDef1 a va rhs vrhs (NName x) pos)
    newTopName x lvl
    inferTop src top

  P.DataDecl pos x params cons top -> do
    error "data decl not supported"
