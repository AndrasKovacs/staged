
module Unification where

import qualified Data.Array.Dynamic.L as D
import qualified Data.IntMap.Strict as IM

import Common
import Cxt
import qualified ElabState as ES
import Evaluation
import Exceptions
import Values
import qualified Syntax as S

{-
TODO:
  - eta-short unification solutions:
      1. try eta-short solution without unfolding
      2. retry eta-long full solution

    This does not require fancy heuristics, eta-conversion, or
    general backtracking. Should still reduce solution sizes in
    many cases

  - use array or list instead of IntMap for PartialRenaming

  - pruning, intersection

  - sigma unification

  - intern field names on elaboration instead of directly comparing them

  - split unification to 0 and 1

  - record eta, split RecCon for levels

  - cache meta occurs

  - cache pruning occurs
-}

closeType :: S.Locals -> S.Ty -> S.Ty
closeType ls topA = case ls of
  S.Empty           -> topA
  S.Define ls x a t -> closeType ls (S.Let1 x a t topA)
  S.Bind0 ls x a cv -> closeType ls (S.Pi x Expl (S.Lift cv a) topA)
  S.Bind1 ls x a    -> closeType ls (S.Pi x Expl a topA)

freshMeta :: Cxt -> S.Ty -> IO S.Tm1
freshMeta cxt ~a = do
  let ~va = eval1 Nil (closeType (_locals cxt) a)
  m <- ES.newMeta va
  pure $! S.Inserted m (_locals cxt)

freshCV :: IO CV
freshCV = CVVar <$!> ES.newCVMeta
{-# inline freshCV #-}

-- Solutions
--------------------------------------------------------------------------------

data PartialRenaming = PRen {
    dom :: Lvl
  , cod :: Lvl
  , ren :: IM.IntMap Lvl}

lift :: PartialRenaming -> PartialRenaming
lift (PRen dom cod ren) =
  PRen (dom + 1) (cod + 1) (IM.insert (coerce cod) dom ren)

invert :: Lvl -> Spine -> IO PartialRenaming
invert gamma sp = do

  let go :: Spine -> (Lvl, IM.IntMap Lvl) -- TODO: optimize
      go SId =
        (0, mempty)
      go (SField1 sp x n) = do
        throw CantUnify
      go (SApp1 sp t i) =
        let (!dom, !ren) = go sp in
        case forceFU1 t of
          Var1 (Lvl x) | IM.notMember x ren ->
            ((,) $$! (dom + 1) $$! (IM.insert x dom ren))
          _ ->
            throw CantUnify

  let (!dom, !ren) = go sp
  pure $ PRen dom gamma ren

renameSp :: MetaVar -> ConvState -> PartialRenaming -> S.Tm1 -> Spine -> IO S.Tm1
renameSp m st pren t sp = let
  go1   t = rename1 m st pren t;     {-# inline go1 #-}
  goSp sp = renameSp m st pren t sp; {-# inline goSp #-}
  in case sp of
    SId           -> pure $! t
    SApp1 t u i   -> S.App1 <$!> goSp t <*!> go1 u <*!> pure i
    SField1 t x n -> S.Field1  <$!> goSp t <*!> pure x <*!> pure n

rename0 :: MetaVar -> ConvState -> PartialRenaming -> Val0 -> IO S.Tm0
rename0 m st pren t = let
  go0 = rename0 m st pren; {-# inline go0 #-}
  go1 = rename1 m st pren; {-# inline go1 #-}

  goClose0 t = rename0 m st (lift pren) (dive t (cod pren))
  {-# inline goClose0 #-}

  force t = case st of
    CSFull -> forceFU0 t
    _      -> forceF0 t
  {-# inline force #-}

  goFix t = rename0 m st (lift (lift pren)) (dive2 t (cod pren))
  {-# inline goFix #-}

  goCases (Close env cs) = goCs cs where
    goCs CNil =
      pure CNil
    goCs (CCons x xs t cs) =
      CCons x xs <$!>  go0 (diveN (Close env t) (cod pren) (length xs))
                 <*!> goCs cs

  in case force t of
    Var0  x       -> case IM.lookup (coerce x) (ren pren) of
                       Nothing -> throwIO CantUnify
                       Just x' -> pure $! S.Var0 (lvlToIx (dom pren) x')
    Top0 x        -> pure $! S.Top0 x
    App0 t u      -> S.App0 <$!> go0 t <*!> go0 u
    Let0 x a t u  -> S.Let0 x <$!> go1 a <*!> go0 t <*!> goClose0 u
    Lam0 x a t    -> S.Lam0 x <$!> go1 a <*!> goClose0 t
    Down t        -> S.Down <$!> go1 t
    RecCon0 fs    -> S.RecCon0 <$!> mapM go0 fs
    Case t cs     -> S.Case <$!> go0 t <*!> goCases cs
    Field0 t x n  -> S.Field0 <$!> go0 t <*!> pure x <*!> pure n
    Fix x y t     -> S.Fix x y <$!> goFix t

rename1 :: MetaVar -> ConvState -> PartialRenaming -> Val1 -> IO S.Tm1
rename1 m st pren t = let
  go0 = rename0 m st pren; {-# inline go0 #-}
  go1 = rename1 m st pren; {-# inline go1 #-}

  goClose1 t = rename1 m st (lift pren) (t $$ Var1 (cod pren));
  {-# inline goClose1 #-}

  force t = case st of
    CSFull -> forceFU1 t
    _      -> forceF1 t
  {-# inline force #-}

  goUH = \case Top1 x -> S.Top1 x; Solved x -> S.Meta x
  {-# inline goUH #-}

  goRec1 :: PartialRenaming -> Close (Fields S.Ty) -> IO (Fields S.Ty)
  goRec1 pren (Close env FNil) =
    pure FNil
  goRec1 pren (Close env (FCons x a as)) =
    FCons x <$!>  rename1 m st pren (eval1 env a)
            <*!> goRec1 (lift pren) (Close (Snoc1 env (Var1 (cod pren))) as)

  in case force t of
    Var1  x       -> case IM.lookup (coerce x) (ren pren) of
                       Nothing -> throwIO CantUnify
                       Just x' -> pure $! S.Var1 (lvlToIx (dom pren) x')
    DataCon x i   -> pure $! S.DataCon x i
    RecCon1 fs    -> S.RecCon1 <$!> mapM go1 fs
    Field1 t x n  -> S.Field1 <$!> go1 t <*!> pure x <*!> pure n
    Flex x sp     -> if x == m then throwIO (OccursCheck x)
                               else renameSp m CSFlex pren (S.Meta x) sp
    Unfold h sp t -> case st of
                       CSRigid -> renameSp m CSFlex pren (goUH h) sp
                                  `catch` \_ -> rename1 m CSFull pren t
                       CSFlex  -> throwIO CantUnify
                       _       -> impossible
    Pi x i a b    -> S.Pi x i <$!> go1 a <*!> goClose1 b
    Fun a b       -> S.Fun <$!> go1 a <*!> go1 b
    Lam1 x i a t  -> S.Lam1 x i <$!> go1 a <*!> goClose1 t
    Up t          -> S.Up <$!> go0 t
    Lift cv a     -> S.Lift cv <$!> go1 a
    Rec1 as       -> S.Rec1 <$!> goRec1 pren as
    Rec0 as       -> S.Rec1 <$!> mapM go1 as
    U u           -> pure $! S.U u
    TyCon x       -> pure $! S.TyCon x
    App1 t u i    -> S.App1 <$!> go1 t <*!> go1 u <*!> pure i


-- | Wrap a term in Lvl number of lambdas, getting the domain types
--   from a Ty.
lams :: Lvl -> Ty -> S.Tm1 -> S.Tm1
lams l a t = go l 0 a t where
  go :: Lvl -> Lvl -> Ty -> S.Tm1 -> S.Tm1
  go l l' a t | l == l' = t
  go l l' a t = case forceFU1 a of
    Pi x i a b ->
      S.Lam1 x i (quote1 l' DontUnfold a) (go l (l' + 1) (b $$ Var1 l') t)
    _ -> impossible

solveMeta :: Dbg => Lvl -> ConvState -> MetaVar -> Spine -> Val1 -> IO ()
solveMeta l st m sp rhs = do
  ma <- ES.unsolvedMetaTy m
  when (st == CSFlex) $ throwIO CantUnify
  pren <- invert l sp
  rhs  <- rename1 m CSRigid pren rhs
  let sol = eval1 Nil (lams (dom pren) ma rhs)
  D.write ES.metaCxt (coerce m) (ES.Solved sol ma)


-- Unification
--------------------------------------------------------------------------------

unifySp :: Dbg => Lvl -> ConvState -> Val1 -> Spine -> Val1 -> Spine -> IO ()
unifySp l st topT sp topT' sp' = case (sp, sp') of
  (SId          , SId            )           -> pure ()
  (SApp1 t u i  , SApp1 t' u' i' )           -> unifySp l st topT t topT' t' >> unify1 l st u u'
  (SField1 t _ n, SField1 t' _ n') | n == n' -> unifySp l st topT t topT' t'
  _ -> throwIO $ UnifyError1 (quote1 l DontUnfold topT) (quote1 l DontUnfold topT')

unifyEq :: (Eq a, Show a) => a -> a -> IO ()
unifyEq a a' = unless (a == a') (throwIO $ EqUnifyError a a')
{-# inline unifyEq #-}

unifyU :: U -> U -> IO ()
unifyU u u' = case (u, u') of
  (U0 cv, U0 cv') -> unifyCV cv cv'
  (U1   , U1    ) -> pure ()
  (u    , u'    ) -> throwIO $ UUnifyError u u'

unifyCV :: Dbg => CV -> CV -> IO ()
unifyCV cv cv' = case (forceCV cv, forceCV cv') of
  (C       , C        ) -> pure ()
  (V       , V        ) -> pure ()
  (CVVar x , CVVar x' ) -> unifyEq x x'
  (CVVar x , cv'      ) -> D.write ES.cvCxt (coerce x)  (ES.CVSolved cv')
  (cv      , CVVar x' ) -> D.write ES.cvCxt (coerce x') (ES.CVSolved cv)
  (cv      , cv'      ) -> impossible -- throwIO $ CVUnifyError cv cv'

unify0 :: Dbg => Lvl -> ConvState -> Val0 -> Val0 -> IO ()
unify0 l st t t' = let
  go0  = unify0 l st;   {-# inline go0 #-}
  go1  = unify1 l st;   {-# inline go1 #-}
  force t = case st of CSFull -> forceFU0 t
                       _      -> forceF0  t
  {-# inline force #-}
  q0 = quote0 l DontUnfold; {-# inline q0 #-}

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
    (Fix _ _ t    , Fix _ _ t'      ) -> goFix t t'
    (Lam0 _ a t   , Lam0 _ a' t'    ) -> goClose0 t t'
    (App0 t u     , App0 t' u'      ) -> go0 t t' >> go0 u u'
    (RecCon0 ts   , RecCon0 ts'     ) -> goRecCon0 ts ts'
    (Field0 t x n , Field0 t' x' n' ) -> go0 t t' >> unifyEq n n'
    (t, t')                           -> err t t'

unify1 :: Dbg => Lvl -> ConvState -> Val1 -> Val1 -> IO ()
unify1 l st t t' = let
  go0  = unify0 l st;   {-# inline go0 #-}
  go1  = unify1 l st;   {-# inline go1 #-}
  goSp = unifySp l st; {-# inline goSp #-}
  q1   = quote1 l DontUnfold; {-# inline q1 #-}

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

    -- canonical/rigid
    (Var1 x       , Var1 x'         ) -> unifyEq x x'
    (Lift cv a    , Lift cv' a'     ) -> unifyCV cv cv' >> go1 a a'
    (Up t         , Up t'           ) -> go0 t t'
    (TyCon x      , TyCon x'        ) -> unifyEq x x'
    (DataCon  x n , DataCon x' n'   ) -> unifyEq n n'
    (Pi x i a b   , Pi x' i' a' b'  ) -> go1 a a' >> goClose1 b b'
    (App1 t u _   , App1 t' u' _    ) -> go1 t t' >> go1 u u'
    (Fun a b      , Fun a' b'       ) -> go1 a a' >> go1 b b'
    (Rec0 as      , Rec0 as'        ) -> goRec0 as as'
    (Rec1 as      , Rec1 as'        ) -> goRec1 l as as'
    (RecCon1 ts   , RecCon1 ts'     ) -> goRecCon1 ts ts'
    (Field1 t x n , Field1 t' x' n' ) -> go1 t t' >> unifyEq n n'
    (U u          , U u'            ) -> unifyU u u'

    -- eta
    (Lam1 _ _ _ t, Lam1 _ _ _ t') -> goClose1 t t'
    (Lam1 _ i _ t, t'           ) -> unify1 (l + 1) st (t $$ Var1 l) (app1 t' (Var1 l) i)
    (t           , Lam1 _ i _ t') -> unify1 (l + 1) st (app1 t (Var1 l) i) (t' $$ Var1 l)

    -- todo: record eta, split RecCon0 and RecCon1

    -- TODO: flex-flex
    (Flex x sp, Flex x' sp') -> unifyEq x x' >> goSp (Flex x sp) sp (Flex x' sp') sp'
    (Flex x sp, t'         ) -> solveMeta l st x sp t'
    (t        , Flex x' sp') -> solveMeta l st x' sp' t

    (t, t') -> err t t'
