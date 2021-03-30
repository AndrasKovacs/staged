
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
  S.Empty               -> topA
  S.Define ls x a t     -> closeType ls (S.Let x a t topA)
  S.Bind ls x a (U0 cv) -> closeType ls (S.Pi x Expl (S.Lift cv a) topA)
  S.Bind ls x a U1      -> closeType ls (S.Pi x Expl a topA)

freshMeta :: Cxt -> S.Ty -> IO S.Tm1
freshMeta cxt a = do
  let va = eval1 Nil (closeType (_locals cxt) a)
  m <- ES.newMeta va
  pure $! S.Inserted m (_locals cxt)

freshCV :: IO CV
freshCV = CVVar <$> ES.newCVMeta
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
        case forceFU t of
          Var (Lvl x) | IM.notMember x ren ->
            ((,) $$! (dom + 1) $$! (IM.insert x dom ren))
          _ ->
            throw CantUnify

  let (!dom, !ren) = go sp
  pure $ PRen dom gamma ren

renameSp :: MetaVar -> ConvState -> PartialRenaming -> S.Tm1 -> Spine -> IO S.Tm1
renameSp m st pren t sp = let
  go   t  = rename m st pren t;     {-# inline go #-}
  goSp sp = renameSp m st pren t sp; {-# inline goSp #-}
  in case sp of
    SId           -> pure $! t
    SApp1 t u i   -> S.App1 <$!> goSp t <*!> go u <*!> pure i
    SField1 t x n -> S.Field  <$!> goSp t <*!> pure x <*!> pure n

rename :: forall s. MetaVar -> ConvState -> PartialRenaming -> Val s -> IO (S.Tm s)
rename m st pren t = let

  go :: forall s. Val s -> IO (S.Tm s)
  go t = rename m st pren t
  {-# inline go #-}

  goClose1 :: Close S.Tm1 -> IO S.Tm1
  goClose1 t =
    rename m st (lift pren) (t $$ Var (cod pren));
  {-# inline goClose1 #-}

  goClose0 :: Close S.Tm0 -> IO S.Tm0
  goClose0 t = rename m st (lift pren) (dive t (cod pren))
  {-# inline goClose0 #-}

  force :: forall s. Val s -> Val s
  force t = case st of
    CSFull -> forceFU t
    _      -> forceF t
  {-# inline force #-}

  goUH :: UnfoldHead -> S.Tm1
  goUH = \case Top1 x -> S.Top x; Solved x -> S.Meta x
  {-# inline goUH #-}

  goFix :: Close S.Tm0 -> IO S.Tm0
  goFix (Close env t) =
    rename m st (lift (lift pren))
           (eval0 (env `Snoc0` cod pren `Snoc0` (cod pren + 1)) t)
  {-# inline goFix #-}

  goCases :: Close (Cases S.Tm0) -> IO (Cases S.Tm0)
  goCases (Close env cs) = goCs cs where
    goCs CNil =
      pure CNil
    goCs (CCons x xs t cs) =
      CCons x xs <$> go (diveN (Close env t) (dom pren) (length xs)) <*!> goCs cs

  in case force t of
    Var  x        -> case IM.lookup (coerce x) (ren pren) of
                       Nothing -> throwIO CantUnify
                       Just x' -> pure $! S.Var (lvlToIx (dom pren) x')
    Top0 x        -> pure $! S.Top x
    App0 t u      -> S.App0 <$> go t <*!> go u
    Let x a t u   -> S.Let x <$> go a <*!> go t <*!> goClose0 u
    Lam0 x a t    -> S.Lam0 x <$> go a <*!> goClose0 t
    Down t        -> S.Down <$> go t
    DataCon x i   -> pure $! S.DataCon x i
    RecCon fs     -> S.RecCon <$> mapM go fs
    Case t cs     -> S.Case <$> go t <*!> goCases cs
    Field t x n   -> S.Field <$> go t <*!> pure x <*!> pure n
    Flex x sp     -> if x == m then throwIO (OccursCheck x)
                               else renameSp m CSFlex pren (S.Meta x) sp

    Unfold h sp t -> case st of
                       CSRigid -> renameSp m CSFlex pren (goUH h) sp
                                  `catch` \_ -> rename m CSFull pren t
                       CSFlex  -> throwIO CantUnify
                       _       -> impossible

    Pi x i a b    -> S.Pi x i <$> go a <*!> goClose1 b
    Fun a b       -> S.Fun <$> go a <*!> go b
    Lam1 x i a t  -> S.Lam1 x i <$> go a <*!> goClose1 t
    Up t          -> S.Up <$> go t
    Lift cv a     -> S.Lift cv <$> go a
    Rec fs        -> S.Rec <$> mapM go fs
    U u           -> pure $ S.U u
    TyCon x       -> pure $ S.TyCon x
    Fix x y t     -> S.Fix x y <$!> goFix t
    App1 t u i    -> S.App1 <$> go t <*!> go u <*!> pure i


-- | Wrap a term in Lvl number of lambdas, getting the domain types
--   from a Ty.
lams :: Lvl -> Ty -> S.Tm1 -> S.Tm1
lams l a t = go l 0 a t where
  go :: Lvl -> Lvl -> Ty -> S.Tm1 -> S.Tm1
  go l l' a t | l == l' = t
  go l l' a t = case forceFU a of
    Pi x i a b ->
      S.Lam1 x i (quote l' DontUnfold a) (go l (l' + 1) (b $$ Var l') t)
    _ -> impossible

solveMeta :: Lvl -> ConvState -> MetaVar -> Spine -> Val1 -> IO ()
solveMeta l st m sp rhs = do
  ma <- ES.unsolvedMetaTy m
  when (st == CSFlex) $ throwIO CantUnify
  pren <- invert l sp
  rhs  <- rename m CSRigid pren rhs
  let sol = eval1 Nil (lams (dom pren) ma rhs)
  D.write ES.metaCxt (coerce m) (ES.Solved sol ma)


-- Unification
--------------------------------------------------------------------------------

unifySp :: Lvl -> ConvState -> Spine -> Spine -> IO ()
unifySp l st sp sp' = case (sp, sp') of
  (SId          , SId            ) -> pure ()
  (SApp1 t u i  , SApp1 t' u' i' ) -> unifySp l st t t' >> unify l st u u'
  (SField1 t _ n, SField1 t' _ n') | n == n' -> unifySp l st t t'
  _ -> throwIO CantUnify

unifyEq :: Eq a => a -> a -> IO ()
unifyEq a a' = unless (a == a') (throwIO CantUnify)
{-# inline unifyEq #-}

unifyCV :: CV -> CV -> IO ()
unifyCV C         C          = pure ()
unifyCV V         V          = pure ()
unifyCV (CVVar x) (CVVar x') = unifyEq x x'
unifyCV (CVVar x) cv'        = D.write ES.cvCxt (coerce x)  (ES.CVSolved cv')
unifyCV cv        (CVVar x') = D.write ES.cvCxt (coerce x') (ES.CVSolved cv)
unifyCV _         _          = throwIO CantUnify

unify :: forall s. Lvl -> ConvState -> Val s -> Val s -> IO ()
unify l st t t' = let

  go :: forall s. Val s -> Val s -> IO ()
  go = unify l st
  {-# inline go #-}

  goSp :: Spine -> Spine -> IO ()
  goSp = unifySp l st;
  {-# inline goSp #-}

  force :: forall s. Val s -> Val s
  force t = case st of CSFull -> forceFU t
                       _      -> forceF  t
  {-# inline force #-}

  goClose1 :: Close S.Tm1 -> Close S.Tm1 -> IO ()
  goClose1 t t' =
    let v = Var l
    in unify (l + 1) st (t $$ v) (t' $$ v)
  {-# inline goClose1 #-}

  goClose0 :: Close S.Tm0 -> Close S.Tm0 -> IO ()
  goClose0 t t' = unify (l + 1) st (dive t l) (dive t' l)
  {-# inline goClose0 #-}

  goU :: forall s s'. U s -> U s' -> IO ()
  goU (U0 cv) (U0 cv') = unifyCV cv cv'
  goU U1      U1       = pure ()
  goU _       _        = throwIO CantUnify

  goUH :: UnfoldHead -> UnfoldHead -> IO ()
  goUH h h' = case (h, h') of
    (Solved x, Solved x') -> unifyEq x x'
    (Top1 x  , Top1 x')   -> unifyEq x x'
    _                     -> throwIO CantUnify
  {-# inline goUH #-}

  goFields :: forall s. Fields (Val s) -> Fields (Val s) -> IO ()
  goFields FNil           FNil                        = pure ()
  goFields (FCons x a fs) (FCons x' a' fs') | x == x' = go a a' >> goFields fs fs'
  goFields _              _                           = throwIO CantUnify

  goFix :: Close S.Tm0 -> Close S.Tm0 -> IO ()
  goFix (Close env t) (Close env' t') =
    unify (l + 2) st (eval0 (env  `Snoc0` l `Snoc0` (l + 1)) t )
                     (eval0 (env' `Snoc0` l `Snoc0` (l + 1)) t')
  {-# inline goFix #-}

  in case (,) $$! force t $$! force t' of

    -- unfolding
    (Unfold h sp t, Unfold h' sp' t') -> case st of
      CSRigid -> (goUH h h' >> goSp sp sp') `catch` \_ -> unify l CSFull t t'
      CSFlex  -> throwIO CantUnify
      _       -> impossible
    (Unfold h sp t, t') -> case st of
      CSRigid -> go t t'
      CSFlex  -> throwIO CantUnify
      _       -> impossible
    (t, Unfold h' sp' t') -> case st of
      CSRigid -> go t t'
      CSFlex  -> throwIO CantUnify
      _       -> impossible

    -- canonical/rigid
    (Var x       , Var x'          ) -> unifyEq x x'
    (Top0 x      , Top0 x'         ) -> unifyEq x x'
    (Let _ a t u , Let _ a' t' u'  ) -> go a a' >> go t t' >> goClose0 u u'
    (Lift cv a   , Lift cv' a'     ) -> unifyCV cv cv' >> go a a'
    (Up t        , Up t'           ) -> go t t'
    (Down t      , Down t'         ) -> go t t'
    (TyCon x     , TyCon x'        ) -> unifyEq x x'
    (DataCon x n , DataCon x' n'   ) -> unifyEq n n'
    (Case t cs   , Case t' cs'     ) -> go t t' >> undefined
    (Fix _ _ t   , Fix _ _ t'      ) -> goFix t t'
    (Pi x i a b  , Pi x' i' a' b'  ) -> go a a' >> goClose1 b b'
    (App1 t u _  , App1 t' u' _    ) -> go t t' >> go u u'
    (Fun a b     , Fun a' b'       ) -> go a a' >> go b b'
    (Lam0 _ a t  , Lam0 _ a' t'    ) -> goClose0 t t'
    (App0 t u    , App0 t' u'      ) -> go t t' >> go u u'
    (Rec as      , Rec as'         ) -> goFields as as'
    (RecCon ts   , RecCon ts'      ) -> goFields ts ts'
    (Field t x n , Field t' x' n'  ) -> go t t' >> unifyEq n n'
    (U u         , U u'            ) -> goU u u'

    -- eta
    (Lam1 _ _ _ t, Lam1 _ _ _ t') -> goClose1 t t'
    (Lam1 _ i _ t, t'           ) -> unify (l + 1) st (t $$ Var l) (app1 t' (Var l) i)
    (t           , Lam1 _ i _ t') -> unify (l + 1) st (app1 t (Var l) i) (t' $$ Var l)

    -- todo: record eta, split RecCon0 and RecCon1

    -- flex
    (Flex x sp, Flex x' sp') -> unifyEq x x' >> goSp sp sp' -- TODO: flex-flex
    (Flex x sp, t'         ) -> solveMeta l st x sp t'
    (t        , Flex x' sp') -> solveMeta l st x' sp' t

    _ -> throwIO CantUnify
