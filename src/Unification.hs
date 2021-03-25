
module Unification where

import qualified Data.Array.Dynamic.L as D
import qualified Data.IntMap as IM

import Common
import Cxt
import ElabState
import Evaluation
import Exceptions
import Values
import qualified Syntax as S

{-
TODO:
  - eta-short unification solutions:
      1. try eta-short solution without unfolding
      2. retry eta-long full solution

    this does not require fancy heuristics, eta-conversion, or
    general backtracking. Should still reduce solution sizes in
    many cases

  - use array or list instead of IntMap for PartialRenaming

  - pruning, intersection

  - intern field names instead of directly comparing them

-}

--------------------------------------------------------------------------------

closeType :: S.Locals -> S.Ty -> S.Ty
closeType ls topA = case ls of
  S.Empty           -> topA
  S.Define ls x t a -> closeType ls (S.Let1 x a t topA)
  S.Bind ls x a     -> closeType ls (S.Pi x Expl a topA)

freshMeta :: Cxt -> S.Ty -> IO S.Tm1
freshMeta cxt a = do
  let va = eval1 Nil (closeType (_locals cxt) a)
  m <- newMeta va
  pure $! S.Inserted m (_locals cxt)


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

  let go :: Spine -> IO (Lvl, IM.IntMap Lvl) -- todo: unbox tuple, use array for partial renaming
      go SNil          = pure (0, mempty)
      go (SApp sp t i) = do
        (dom, ren) <- go sp
        case forceFU1 t of
          Var1 (Lvl x) | IM.notMember x ren ->
            pure $! ((,) $$! (dom + 1) $$! (IM.insert x dom ren))
          _ ->
            throwIO CantUnify

  (dom, ren) <- go sp
  pure $ PRen dom gamma ren

renameSp :: MetaVar -> ConvState -> PartialRenaming -> S.Tm1 -> Spine -> IO S.Tm1
renameSp m st pren t sp = let
  go   t  = rename1 m st pren t;     {-# inline go #-}
  goSp sp = renameSp m st pren t sp; {-# inline goSp #-}
  in case sp of
    SNil       -> pure $! t
    SApp t u i -> S.App1 <$!> goSp t <*!> go u <*!> pure i

rename0 :: MetaVar -> ConvState -> PartialRenaming -> Val0 -> IO S.Tm0
rename0 m st pren v = let
  go      t = rename0 m st pren t;                {-# inline go #-}
  go1     t = rename1 m st pren t;                {-# inline go1 #-}
  goClose t = rename0 m st (lift pren) undefined; {-# inline goClose #-}

  goCases :: Close (Cases S.Tm0) -> IO (Cases S.Tm0)
  goCases (Close env cs) = goCs cs where
    goCs CNil              = pure CNil
    goCs (CCons x xs t cs) = CCons x xs <$> go (eval0 (diveCase env xs (dom pren)) t) <*!> goCs cs

  in case v of
    Var0 x       -> case IM.lookup (coerce x) (ren pren) of
                      Nothing -> throwIO CantUnify
                      Just x' -> pure $! S.Var0 (lvlToIx (dom pren) x')
    Top0 x       -> pure $! S.Top0 x
    App0 t u     -> S.App0 <$> go t <*!> go u
    Let0 x a t u -> S.Let0 x <$> go1 a <*!> go t <*!> goClose u
    Lam0 x a t   -> S.Lam0 x <$> go1 a <*!> goClose t
    Down t       -> S.Down <$> go1 t
    DataCon0 x i -> pure $! S.DataCon0 x i
    RecCon fs    -> S.RecCon <$> mapM go fs
    Case t cs    -> S.Case <$> go t <*!> goCases cs
    Field t x n  -> S.Field <$> go t <*!> pure x <*!> pure n

rename1 :: MetaVar -> ConvState -> PartialRenaming -> Val1 -> IO S.Tm1
rename1 m st pren v = let
  go      t = rename1 m st pren t;                             {-# inline go #-}
  goClose t = rename1 m st (lift pren) (t $$ Var1 (cod pren)); {-# inline goClose #-}
  force   t  = case st of CSFull -> forceFU1 t
                          _      -> forceF1  t
  {-# inline force #-}

  goRH :: RigidHead -> IO S.Tm1
  goRH = \case
    RHVar x       -> case IM.lookup (coerce x) (ren pren) of
                       Nothing -> throwIO (OutOfScope x)
                       Just x' -> pure (S.Var1 (lvlToIx (dom pren) x'))
    RHDataCon x i -> pure $ S.DataCon1 x i
    RHTyCon x     -> pure $ S.TyCon x

  goUH :: UnfoldHead -> S.Tm1
  goUH = \case
    UHMeta x -> S.Meta x
    UHTop x  -> S.Top1 x

  in case force v of
    Rigid h sp    -> do {h <- goRH h; renameSp m CSFlex pren h sp}
    Flex x sp     -> if x == m then throwIO (OccursCheck x)
                               else renameSp m CSFlex pren (S.Meta x) sp
    Unfold h sp t -> case st of
                       CSRigid -> renameSp m CSFlex pren (goUH h) sp
                                  `catch` \_ -> rename1 m CSFull pren t
                       CSFlex  -> throwIO CantUnify
                       _       -> impossible

    Pi x i a b    -> S.Pi x i <$> go a <*!> goClose b
    Fun a b       -> S.Fun <$> go a <*!> go b
    Lam1 x i a t  -> S.Lam1 x i <$> go a <*!> goClose t
    Up t          -> S.Up <$> rename0 m st pren t
    Lift a        -> S.Lift <$> go a
    Rec fs        -> S.Rec <$> mapM go fs
    Ty u          -> pure $ S.Ty u


-- | Wrap a term in Lvl number of lambdas, getting the domain types
--   from a Ty.
lams :: Lvl -> Ty -> S.Tm1 -> S.Tm1
lams l a t = go l 0 a t where
  go l l' a t | l == l' = t
  go l l' a t = case forceFU1 a of
    Pi x i a b ->
      S.Lam1 x i (quote1 l' DontUnfold a) (go l (l' + 1) (b $$ Var1 l') t)
    _ -> impossible

solveMeta :: Lvl -> ConvState -> MetaVar -> Spine -> Val1 -> IO ()
solveMeta l st m sp rhs = do
  ma <- unsolvedMetaTy m
  when (st == CSFlex) $ throwIO CantUnify
  pren <- invert l sp
  rhs  <- rename1 m CSRigid pren rhs
  let sol = eval1 Nil (lams (dom pren) ma rhs)
  D.write metaCxt (coerce m) (MESolved sol ma)


-- Unification
--------------------------------------------------------------------------------

unifySp :: Lvl -> ConvState -> Spine -> Spine -> IO ()
unifySp l st sp sp' = case (sp, sp') of
  (SNil      , SNil         ) -> pure ()
  (SApp t u i, SApp t' u' i') -> unifySp l st t t' >> unify1 l st u u'
  _                           -> throwIO CantUnify

unify0 :: Lvl -> ConvState -> Val0 -> Val0 -> IO ()
unify0 l st v v' = undefined

unify1 :: Lvl -> ConvState -> Val1 -> Val1 -> IO ()
unify1 l st v v' = let
  go      = unify1 l st;  {-# inline go #-}
  go0     = unify0 l st;  {-# inline go0 #-}
  goSp    = unifySp l st; {-# inline goSp #-}
  force t = case st of CSFull -> forceFU1 t
                       _      -> forceF1  t
  {-# inline force #-}

  goClose :: Close S.Tm1 -> Close S.Tm1 -> IO ()
  goClose t t' =
    let v = Var1 l
    in unify1 (l + 1) st (t $$ v) (t' $$ v)
  {-# inline goClose #-}

  eq :: Eq a => a -> a -> IO ()
  eq a a' = unless (a == a') (throwIO CantUnify)
  {-# inline eq #-}

  goRH :: RigidHead -> RigidHead -> IO ()
  goRH h h' = case (h, h') of
    (RHVar x       , RHVar x'      ) -> eq x x'
    (RHDataCon _ i , RHDataCon _ i') -> eq i i'
    (RHTyCon x     , RHTyCon x'    ) -> eq x x'
    _                                -> throwIO CantUnify

  goUH :: UnfoldHead -> UnfoldHead -> IO ()
  goUH h h' = case (h, h') of
    (UHMeta x, UHMeta x') -> eq x x'
    (UHTop x, UHTop x')   -> eq x x'
    _                     -> throwIO CantUnify

  goFields :: Fields Ty -> Fields Ty -> IO ()
  goFields FNil           FNil                        = pure ()
  goFields (FCons x a fs) (FCons x' a' fs') | x == x' = go a a' >> goFields fs fs'
  goFields _              _                           = throwIO CantUnify

  in case (v, v') of

    -- unfolding
    (Unfold h sp t, Unfold h' sp' t') -> case st of
      CSRigid -> (goUH h h' >> goSp sp sp') `catch` \_ -> unify1 l CSFull t t'
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

    -- canonical
    (Pi x i a b, Pi x' i' a' b') -> go a a' >> goClose b b'
    (Up t      , Up t'         ) -> go0 t t'
    (Lift a    , Lift a'       ) -> go a a'
    (Ty u      , Ty u'         ) -> eq u u'
    (Rec as    , Rec as'       ) -> goFields as as'

    -- rigid
    (Rigid h sp, Rigid h' sp') -> goRH h h' >> goSp sp sp'

    -- eta
    (Lam1 _ _ _ t, Lam1 _ _ _ t') -> goClose t t'
    (Lam1 _ i _ t, t'           ) -> unify1 (l + 1) st (t $$ Var1 l) (app t' (Var1 l) i)
    (t           , Lam1 _ i _ t') -> unify1 (l + 1) st (app t (Var1 l) i) (t' $$ Var1 l)

    -- flex
    (Flex x sp, Flex x' sp') | x == x' -> goSp sp sp'
    (Flex x sp, t'         )           -> solveMeta l st x sp t'
    (t        , Flex x' sp')           -> solveMeta l st x' sp' t

    _ -> throwIO CantUnify
