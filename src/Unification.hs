{-# options_ghc -Wno-unused-imports -Wno-type-defaults #-}

module Unification (freshCV, freshMeta, closeType, unify0, unify1) where

import qualified Data.Array.Dynamic.L as D
import qualified Data.IntMap.Strict as IM
import Control.Exception (try)

import Common
import Cxt
import Exceptions
import Values
import qualified ElabState as ES
import Evaluation
import qualified Syntax as S
import Pretty

{-
TODO:
  - pruning with currying and <_> handling
  - eta short unification
  - flex-flex
  - intersection
  - better data structures

-}

closeType :: S.Locals -> S.Ty -> S.Ty
closeType ls topA = case ls of
  S.Empty           -> topA
  S.Define ls x a t -> closeType ls (S.Let1 x a t topA)
  S.Bind0 ls x a cv -> closeType ls (S.Pi x Expl (S.Lift cv a) topA)
  S.Bind1 ls x a    -> closeType ls (S.Pi x Expl a topA)

freshMeta :: Cxt -> Ty -> IO S.Tm1
freshMeta cxt a = do
  let qa = closeType (_locals cxt) $! quote1 (_lvl cxt) (LiftVars (_lvl cxt)) a
  m <- ES.newMeta (eval1 Nil qa)
  pure $! S.AppPruning (S.Meta m) (_pruning cxt)

freshCV :: Cxt -> IO S.CV
freshCV cxt = do
  let ~va = eval1 Nil (closeType (_locals cxt) S.CV)
  m <- ES.newMeta va
  pure $! S.AppPruning (S.Meta m) (_pruning cxt)
{-# inline freshCV #-}

-- Solutions
--------------------------------------------------------------------------------

data PSEntry = PS0 Val0 | PS1 Val1
  deriving Show

data PartialSub = PSub {
    occurs   :: Maybe MetaVar     -- optional meta for occurs check
  , dom      :: Lvl               -- Γ
  , cod      :: Lvl               -- Δ
  , sub      :: IM.IntMap PSEntry -- Γ ~> Δ     a partial substitution
  , isLinear :: Bool              -- if sub is not linear, we have to check rhs type validity
  }                               -- after substitution
  deriving Show

emptyPSub :: PartialSub
emptyPSub = PSub Nothing 0 0 mempty True

lift1 :: PartialSub -> PartialSub
lift1 (PSub occ dom cod sub isLinear) =
  PSub occ (dom + 1) (cod + 1) (IM.insert (coerce cod) (PS1 (Var1 dom)) sub) isLinear

lift0 :: PartialSub -> PartialSub
lift0 (PSub occ dom cod sub isLinear) =
  PSub occ (dom + 1) (cod + 1) (IM.insert (coerce cod) (PS0 (Var0 dom)) sub) isLinear

skip :: PartialSub -> PartialSub
skip sub = sub {cod = cod sub + 1}

-- Inversion
--------------------------------------------------------------------------------

invertVal1 :: Val1 -> Val1 -> PartialSub -> IO PartialSub
invertVal1 v rhs psub = case forceFU1 v of
  Var1 x -> do
    -- x is non-linear
    if IM.member (coerce x) (sub psub) then do
      pure $! psub {isLinear = False, sub = IM.delete (coerce x) (sub psub)}

    -- x is linear
    else do
      pure $! psub {
        sub = IM.insert (coerce x) (PS1 rhs) (sub psub)}

  RecCon1 ts -> do
   let go psub ix FNil           = pure psub
       go psub ix (FCons x t ts) = do
         psub <- invertVal1 t (field1 rhs x ix) psub
         go psub (ix + 1) ts
   go psub 0 ts

  Up v -> do
    invertVal0 v (down rhs) psub

  _ -> do
    throwIO SpineError

invertVal0 :: Val0 -> Val0 -> PartialSub -> IO PartialSub
invertVal0 v rhs psub = case forceFU0 v of

  Var0 x -> do
    -- x is non-linear
    if IM.member (coerce x) (sub psub) then do
      pure $! psub {isLinear = False, sub = IM.delete (coerce x) (sub psub)}

    -- x is linear
    else do
      pure $! psub {
        sub = IM.insert (coerce x) (PS0 rhs) (sub psub)}

  RecCon0 ts -> do
    let go psub ix FNil           = pure psub
        go psub ix (FCons x t ts) = do
          psub <- invertVal0 t (Field0 rhs x ix) psub
          go psub (ix + 1) ts
    go psub 0 ts

  Down t -> do
    invertVal1 t (up rhs) psub

  _ -> do
    throwIO SpineError

invertSp :: Lvl -> Spine -> IO PartialSub
invertSp gamma = \case
  Id ->
    pure emptyPSub

  App1 t u i -> do
    psub <- invertSp gamma t
    let d = dom psub
    invertVal1 u (Var1 d) (psub {dom = d + 1})

  Field1{} -> do
    throwIO NeedExpansion

--------------------------------------------------------------------------------

-- | Create a fresh eta-expanded value for a meta such that applying it to the spine returns
--   a VFlex without projections.
metaExpansion :: MetaVar -> Spine -> IO Val1
metaExpansion m sp = do
  a <- ES.unsolvedMetaTy m

  let go :: Cxt -> Spine -> Ty -> IO S.Tm1
      go cxt sp a = case (sp, forceFU1 a) of

        (Id, a) -> do
          freshMeta cxt a

        (App1 sp t i, Pi x _ a b  ) -> do
          let qa = quote1 (_lvl cxt) UnfoldNone a
          S.Lam1 x i qa <$> go (bind1' x qa a cxt) sp (b $$ Var1 (_lvl cxt))

        (Field1 t x ix, Rec1 fs) -> do

          let goFields (Close env FNil) = do
                pure FNil
              goFields (Close env (FCons x' a as)) = do
                t <- if x == x' then go cxt t (eval1 env a)
                                else freshMeta cxt (eval1 env a)
                ts <- goFields (Close (Snoc1 env (eval1 (_env cxt) t)) as)
                pure $! FCons x' t ts

          S.RecCon1 <$!> goFields fs

        _ -> impossible

  eval1 Nil <$!> go (emptyCxt "") (reverseSpine sp) a

-- | Expand a meta, eliminating projections from the spine. Also update the meta with the expansion.
expandMeta :: MetaVar -> Spine -> IO Val1
expandMeta m sp = do
  m' <- metaExpansion m sp
  solve' m Id emptyPSub m'
  pure $! app1Sp m' sp

validateRhsType :: Ty -> Spine -> PartialSub -> IO ()
validateRhsType mty sp psub = do
  let getTy :: Ty -> Spine -> Ty
      getTy a            Id            = a
      getTy (Pi x i a b) (App1 sp t _) = getTy (b $$ t) sp
      getTy _             _            = impossible
  let rhsTy = getTy mty (reverseSpine sp)
  rhsTy <- psubst1 CSRigid psub rhsTy
  pure ()

-- | Solve m given the result of inversion on a spine.
solve' :: Dbg => MetaVar -> Spine -> PartialSub -> Val1 -> IO ()
solve' m sp psub rhs = do
  mty <- ES.unsolvedMetaTy m

  when (not $ isLinear psub) $ do
    validateRhsType mty sp psub

  rhs <- psubst1 CSRigid (psub {occurs = Just m}) rhs
  let solution = eval1 Nil $ lams (dom psub) mty rhs
  D.write ES.metaCxt (coerce m) (ES.Solved solution mty)

psubstSp :: Dbg => ConvState -> PartialSub -> S.Tm1 -> Spine -> IO S.Tm1
psubstSp st psub t sp = let
  go1   t = psubst1 st psub t;     {-# inline go1 #-}
  goSp sp = psubstSp st psub t sp; {-# inline goSp #-}
  in case sp of
    Id           -> pure $! t
    App1 t u i   -> S.App1 <$!> goSp t <*!> go1 u <*!> pure i
    Field1 t x n -> S.Field1  <$!> goSp t <*!> pure x <*!> pure n


-- | Remove some arguments from a closed iterated Pi type.
pruneTy :: S.RevPruning -> Ty -> IO S.Ty
pruneTy (S.RevPruning pr) a = go pr emptyPSub a where
  go :: S.Pruning -> PartialSub -> Ty -> IO S.Ty
  go pr psub a = case (pr, forceFU1 a) of
    ([]               , a         ) -> psubst1 CSRigid psub a
    (S.PESkip    : pr , Pi x i a b) -> go pr (skip psub) (b $$ Var1 (cod psub))
    (S.PEBind1{} : pr , Pi x i a b) -> S.Pi x i <$!> psubst1 CSRigid psub a
                                                <*!> go pr (lift1 psub) (b $$ Var1 (cod psub))
    _                               -> impossible

psubstSpWithPruning :: ConvState -> PartialSub -> MetaVar -> Spine -> IO S.Tm1
psubstSpWithPruning st psub m sp = do
  -- ms <- readIORef mcxt
  -- traceShowM ("prunesp", m, sp)
  -- try (psubstSp psub (S.Meta m) sp) >>= \case
  --   Left NeedExpansion -> impossible
  --   Left _ -> do
  --     -- traceShowM ("try to prune", m, sp)
  --     (m, sp) <- prunePrep psub m sp
  --     -- traceShowM ("prepped", m, sp)
  --     t <- pruneFlex psub m sp
  --     -- traceShowM ("pruned", t)
  --     pure t
  --   Right t -> do
  --     pure t
  undefined

psubst0 :: Dbg => ConvState -> PartialSub -> Val0 -> IO S.Tm0
psubst0 st psub t = let
  go0 = psubst0 st psub; {-# inline go0 #-}
  go1 = psubst1 st psub; {-# inline go1 #-}

  goClose0 t = psubst0 st (lift0 psub) (dive t (cod psub))
  {-# inline goClose0 #-}

  force t = case st of
    CSFull -> forceFU0 t
    _      -> forceF0 t
  {-# inline force #-}

  goCases (Close env cs) = goCs cs where
    goCs CNil =
      pure CNil
    goCs (CCons x xs t cs) =
      CCons x xs <$!> go0 (diveN (Close env t) (cod psub) (length xs))
                 <*!> goCs cs

  in case force t of
    Var0  x       -> case IM.lookup (coerce x) (sub psub) of
                       Nothing       -> throwIO $ OutOfScope x
                       Just (PS1 _)  -> impossible
                       Just (PS0 x') -> pure $! quote0 (dom psub) UnfoldNone x'

    Top0 x        -> pure $! S.Top0 x
    App0 t u      -> S.App0 <$!> go0 t <*!> go0 u
    Let0 x a t u  -> S.Let0 x <$!> go1 a <*!> go0 t <*!> goClose0 u
    Lam0 x a t    -> S.Lam0 x <$!> go1 a <*!> goClose0 t
    Down t        -> S.Down <$!> go1 t
    Case t ts     -> S.Case <$!> go0 t <*!> goCases ts
    RecCon0 fs    -> S.RecCon0 <$!> mapM go0 fs
    Field0 t x n  -> S.Field0 <$!> go0 t <*!> pure x <*!> pure n
    Add t u       -> S.Add <$!> go0 t <*!> go0 u
    Sub t u       -> S.Sub <$!> go0 t <*!> go0 u
    Mul t u       -> S.Mul <$!> go0 t <*!> go0 u
    IntLit n      -> pure $! S.IntLit n

psubst1 :: Dbg => ConvState -> PartialSub -> Val1 -> IO S.Tm1
psubst1 st psub t = let
  go0  = psubst0 st psub; {-# inline go0 #-}
  go1  = psubst1 st psub; {-# inline go1 #-}
  goSp = psubstSp st psub; {-# inline goSp #-}

  goClose1 t = psubst1 st (lift1 psub) (t $$ Var1 (cod psub));
  {-# inline goClose1 #-}

  force t = case st of
    CSFull -> forceFU1 t
    _      -> forceF1 t
  {-# inline force #-}

  goUH = \case Top1 x -> S.Top1 x; Solved x -> S.Meta x
  {-# inline goUH #-}

  goRH = \case
    RHVar1 x -> case IM.lookup (coerce x) (sub psub) of
      Nothing       -> throwIO $! OutOfScope x
      Just (PS0 _)  -> impossible
      Just (PS1 x') -> pure $! quote1 (dom psub) UnfoldNone x'
    RHTyCon x -> pure $! S.TyCon x
    RHDataCon x ix -> pure $! S.DataCon x ix
  {-# inline goRH #-}

  goRec1 :: PartialSub -> Close (Fields S.Ty) -> IO (Fields S.Ty)
  goRec1 psub (Close env FNil) =
    pure FNil
  goRec1 psub (Close env (FCons x a as)) =
    FCons x <$!> psubst1 st psub (eval1 env a)
            <*!> goRec1 (lift1 psub) (Close (Snoc1 env (Var1 (cod psub))) as)

  in case force t of

    Rigid h sp -> do
      h <- goRH h
      psubstSp st psub h sp

    Flex x sp ->
      if Just x == occurs psub then do
        throwIO (OccursCheck x)
      else do
        psubstSp st psub (S.Meta x) sp

    Unfold h sp t -> case st of
      CSRigid -> psubstSp CSFlex psub (goUH h) sp
                 `catch` \_ -> psubst1 CSFull psub t
      CSFlex  -> throwIO CantUnify
      _       -> impossible

    RecCon1 fs   -> S.RecCon1 <$!> mapM go1 fs
    Pi x i a b   -> S.Pi x i <$!> go1 a <*!> goClose1 b
    Fun a b bcv  -> S.Fun <$!> go1 a <*!> go1 b <*!> go1 bcv
    Lam1 x i a t -> S.Lam1 x i <$!> go1 a <*!> goClose1 t
    Up t         -> S.Up <$!> go0 t
    Lift cv a    -> S.Lift <$!> go1 cv <*!> go1 a
    Rec1 as      -> S.Rec1 <$!> goRec1 psub as
    Rec0 as      -> S.Rec0 <$!> mapM go1 as
    U1           -> pure S.U1
    U0 cv        -> S.U0 <$!> go1 cv
    CV           -> pure S.CV
    Comp         -> pure S.Comp
    Val          -> pure S.Val
    Int          -> pure $! S.Int


-- | Wrap a term in Lvl number of lambdas, getting the domain types
--   from a Ty.
lams :: Lvl -> Ty -> S.Tm1 -> S.Tm1
lams l a t = go l 0 a t where
  go :: Lvl -> Lvl -> Ty -> S.Tm1 -> S.Tm1
  go l l' a t | l == l' = t
  go l l' a t = case forceFU1 a of
    Pi x i a b ->
      S.Lam1 x i (quote1 l' UnfoldNone a) (go l (l' + 1) (b $$ Var1 l') t)
    foo -> error (show foo)

solve :: Dbg => Lvl -> ConvState -> MetaVar -> Spine -> Val1 -> IO ()
solve l st m sp rhs = do

  ma <- ES.unsolvedMetaTy m

  traceM ("?" ++ show m ++ " : " ++ showVal1Top' ma)
  traceM ((showTm1Top $ quote1 l UnfoldAll (Flex m sp)) ++ " =? " ++
           (showTm1Top $ quote1 l UnfoldAll rhs) ++ "\n")

  when (st == CSFlex) $ throwIO CantUnify
  psub <- invertSp l sp
  rhs  <- psubst1 CSRigid psub rhs
  let sol = eval1 Nil (lams (dom psub) ma rhs)
  D.write ES.metaCxt (coerce m) (ES.Solved sol ma)

-- Unification
--------------------------------------------------------------------------------

unifySp :: Dbg => Lvl -> ConvState -> Val1 -> Spine -> Val1 -> Spine -> IO ()
unifySp l st topT sp topT' sp' = case (sp, sp') of
  (Id          , Id            )           -> pure ()
  (App1 t u i  , App1 t' u' i' )           -> unifySp l st topT t topT' t' >> unify1 l st u u'
  (Field1 t _ n, Field1 t' _ n') | n == n' -> unifySp l st topT t topT' t'
  _ -> throwIO $ UnifyError1 (quote1 l UnfoldNone topT) (quote1 l UnfoldNone topT')

unifyEq :: (Eq a, Show a) => a -> a -> IO ()
unifyEq a a' = unless (a == a') (throwIO $ EqUnifyError a a')
{-# inline unifyEq #-}

unify0 :: Dbg => Lvl -> ConvState -> Val0 -> Val0 -> IO ()
unify0 l st t t' = let
  go0  = unify0 l st;   {-# inline go0 #-}
  go1  = unify1 l st;   {-# inline go1 #-}
  force t = case st of CSFull -> forceFU0 t
                       _      -> forceF0  t
  {-# inline force #-}
  q0 = quote0 l UnfoldNone; {-# inline q0 #-}

  err t t' = throwIO $ UnifyError0 (q0 t) (q0 t'); {-# inline err #-}

  goClose0 t t' = unify0 (l + 1) st (dive t l) (dive t' l)
  {-# inline goClose0 #-}

  goRecCon0 :: Dbg => Fields Val0 -> Fields Val0 -> IO ()
  goRecCon0 FNil           FNil                        = pure ()
  goRecCon0 (FCons x a fs) (FCons x' a' fs') | x == x' = go0 a a' >> goRecCon0 fs fs'
  goRecCon0 fs fs' = err (RecCon0 fs) (RecCon0 fs')

  goFix t t' = unify0 (l + 2) st (dive2 t l) (dive2 t' l)
  {-# inline goFix #-}

  goCases :: Dbg => Val0 -> Close (Cases S.Tm0) -> Val0 -> Close (Cases S.Tm0) -> IO ()
  goCases topT (Close env cs) topT' (Close env' cs') = case (cs, cs') of
    (CNil, CNil) ->
      pure ()
    (CCons x xs t cs, CCons x' xs' t' cs') -> do
      let len = length xs
      unifyEq x x'
      unify0 l st (diveN (Close env t) l len) (diveN (Close env' t') l len)
      goCases topT (Close env cs) topT' (Close env cs')
    (cs, cs') ->
      err (Case topT (Close env cs)) (Case topT' (Close env' cs'))

  in case (,) $$! force t $$! force t' of
    (Var0 x       , Var0 x'         ) -> unifyEq x x'
    (Top0 x       , Top0 x'         ) -> unifyEq x x'
    (Let0 _ a t u , Let0 _ a' t' u' ) -> go1 a a' >> go0 t t' >> goClose0 u u'
    (Down t       , Down t'         ) -> go1 t t'
    (Case t cs    , Case t' cs'     ) -> go0 t t' >> goCases t cs t' cs'
    (Lam0 _ a t   , Lam0 _ a' t'    ) -> goClose0 t t'
    (App0 t u     , App0 t' u'      ) -> go0 t t' >> go0 u u'
    (RecCon0 ts   , RecCon0 ts'     ) -> goRecCon0 ts ts'
    (Field0 t x n , Field0 t' x' n' ) -> go0 t t' >> unifyEq n n'
    (IntLit n     , IntLit n'       ) -> unifyEq n n'
    (Add    t u   , Add    t' u'    ) -> go0 t t' >> go0 u u'
    (Mul    t u   , Mul    t' u'    ) -> go0 t t' >> go0 u u'
    (Sub    t u   , Sub    t' u'    ) -> go0 t t' >> go0 u u'
    (t, t')                           -> err t t'


unify1 :: Dbg => Lvl -> ConvState -> Val1 -> Val1 -> IO ()
unify1 l st t t' = let
  go0  = unify0 l st;         {-# inline go0 #-}
  go1  = unify1 l st;         {-# inline go1 #-}
  goSp = unifySp l st;        {-# inline goSp #-}
  q1   = quote1 l UnfoldNone; {-# inline q1 #-}

  err t t' = throwIO $ UnifyError1 (q1 t) (q1 t'); {-# inline err #-}

  force t = case st of CSFull -> forceFU1 t
                       _      -> forceF1  t
  {-# inline force #-}

  goUH topT topT' h h' = case (h, h') of
    (Solved x, Solved x') -> unifyEq x x'
    (Top1 x  , Top1 x')   -> unifyEq x x'
    _                     -> err topT topT'
  {-# inline goUH #-}

  goRec0 FNil           FNil                        = pure ()
  goRec0 (FCons x a fs) (FCons x' a' fs') | x == x' = go1 a a' >> goRec0 fs fs'
  goRec0 fs fs' = err (Rec0 fs) (Rec0 fs')

  goRecCon1 FNil           FNil                        = pure ()
  goRecCon1 (FCons x a fs) (FCons x' a' fs') | x == x' = go1 a a' >> goRecCon1 fs fs'
  goRecCon1 fs fs' = err (RecCon1 fs) (RecCon1 fs')

  goClose1 :: Close S.Tm1 -> Close S.Tm1 -> IO ()
  goClose1 t t' =
    let v = Var1 l
    in unify1 (l + 1) st (t $$ v) (t' $$ v)
  {-# inline goClose1 #-}

  goRec1 :: Lvl -> Close (Fields S.Ty) -> Close (Fields S.Ty) -> IO ()
  goRec1 l (Close env as) (Close env' as') = case (as, as') of
    (FNil, FNil) ->
      pure ()
    (FCons x a as, FCons x' a' as') -> do
      unifyEq x x'
      unify1 l st (eval1 env a) (eval1 env' a')
      goRec1 (l + 1) (Close (Snoc1 env  (Var1 l)) as )
                     (Close (Snoc1 env' (Var1 l)) as')
    (as, as') ->
      err (Rec1 (Close env as)) (Rec1 (Close env' as'))

  in case (,) $$! force t $$! force t' of

    -- unfolding
    (topT@(Unfold h sp t), topT'@(Unfold h' sp' t')) -> case st of
      CSRigid -> (goUH topT topT' h h' >> goSp topT sp topT' sp')
                 `catch` \_ -> unify1 l CSFull t t'
      CSFlex  -> err (Unfold h sp t) (Unfold h' sp' t')
      _       -> impossible
    (Unfold h sp t, t') -> case st of
      CSRigid -> go1 t t'
      CSFlex  -> err (Unfold h sp t) t'
      _       -> impossible
    (t, Unfold h' sp' t') -> case st of
      CSRigid -> go1 t t'
      CSFlex  -> err t (Unfold h' sp' t')
      _       -> impossible

    -- rigid & canonical
    (t@(Rigid h sp) , t'@(Rigid h' sp') ) -> unifyEq h h' >> goSp t sp t' sp'
    (Var1 x         , Var1 x'           ) -> unifyEq x x'
    (Lift cv a      , Lift cv' a'       ) -> go1 cv cv' >> go1 a a'
    (Up t           , Up t'             ) -> go0 t t'
    (Pi x i a b     , Pi x' i' a' b'    ) -> go1 a a' >> goClose1 b b'
    (Fun a b bcv    , Fun a' b' bcv'    ) -> go1 a a' >> go1 bcv bcv' >> go1 b b'
    (Rec0 as        , Rec0 as'          ) -> goRec0 as as'
    (Rec1 as        , Rec1 as'          ) -> goRec1 l as as'
    (RecCon1 ts     , RecCon1 ts'       ) -> goRecCon1 ts ts'
    (U1             , U1                ) -> pure ()
    (U0 cv          , U0 cv'            ) -> go1 cv cv'
    (CV             , CV                ) -> pure ()
    (Comp           , Comp              ) -> pure ()
    (Val            , Val               ) -> pure ()
    (Int            , Int               ) -> pure ()

    -- eta
    (Lam1 _ _ _ t, Lam1 _ _ _ t') -> goClose1 t t'
    (Lam1 _ i _ t, t'           ) -> unify1 (l + 1) st (t $$ Var1 l) (app1 t' (Var1 l) i)
    (t           , Lam1 _ i _ t') -> unify1 (l + 1) st (app1 t (Var1 l) i) (t' $$ Var1 l)

    -- TODO: record eta
    -- TODO: flex-flex, intersection
    (Flex x sp, Flex x' sp') | x == x' -> goSp (Flex x sp) sp (Flex x' sp') sp'
    (Flex x sp, t'         ) -> solve l st x sp t'
    (t        , Flex x' sp') -> solve l st x' sp' t

    (t, t') -> err t t'
