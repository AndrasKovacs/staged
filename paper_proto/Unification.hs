
module Unification (unify, unifyCatch, freshMeta) where

import Control.Monad
import Control.Exception
import Data.IORef
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS

import Common
import Errors
import Evaluation
import Metacontext
import Syntax
import Value
import Cxt

-- TODO :
--   prune quote + splice
--   invert quote + splice
--   expand away splices

--------------------------------------------------------------------------------

freshMeta :: Cxt -> VTy -> Stage -> IO Tm
freshMeta cxt a st = do
  let ~closed = eval [] $ closeTy (path cxt) (quote (lvl cxt) a)
  m <- newMeta closed st
  pure $ InsertedMeta m (pruning cxt)

readUnsolved :: MetaVar -> IO (VTy, Stage)
readUnsolved m = readMeta m >>= \case
  Unsolved a s -> pure (a, s)
  _            -> error "unsolved meta"

--------------------------------------------------------------------------------

-- | partial renaming from Γ to Δ
data PartialRenaming = PRen {
    occ :: Maybe MetaVar   -- optional occurs check
  , dom :: Lvl             -- size of Γ
  , cod :: Lvl             -- size of Δ
  , ren :: IM.IntMap Lvl}  -- mapping from Δ vars to Γ vars

-- | lift : (σ : PRen Γ Δ) → PRen (Γ, x : A[σ]) (Δ, x : A)
lift :: PartialRenaming -> PartialRenaming
lift (PRen occ dom cod ren) = PRen occ (dom + 1) (cod + 1) (IM.insert (unLvl cod) dom ren)

-- | skip : PRen Γ Δ → PRen Γ (Δ, x : A)
skip :: PartialRenaming -> PartialRenaming
skip (PRen occ dom cod ren) = PRen occ dom (cod + 1) ren

-- | invert : (Γ : Cxt) → (spine : Sub Δ Γ) → PRen Γ Δ
--   Optionally returns a pruning of nonlinear spine entries, if there's any.
invert :: Lvl -> Spine -> IO (PartialRenaming, Maybe Pruning)
invert gamma sp = do

  let go :: Spine -> IO (Lvl, IS.IntSet, IM.IntMap Lvl, Pruning, Bool)
      go SId            = pure (0, mempty, mempty, [], True)
      go (SApp sp t i)  = do
        (dom, domvars, ren, pr, isLinear) <- go sp
        case force t of
          VVar (Lvl x) -> case IS.member x domvars of
            True  -> pure (dom + 1, domvars,             IM.delete x ren,     Nothing : pr, False   )
            False -> pure (dom + 1, IS.insert x domvars, IM.insert x dom ren, Just i  : pr, isLinear)
          _ -> throwIO UnifyError

      go SSplice{} =
        impossible

  (dom, domvars, ren, pr, isLinear) <- go sp
  pure (PRen Nothing dom gamma ren, pr <$ guard isLinear)

-- | Remove some arguments from a closed iterated Pi type.
pruneTy :: RevPruning -> VTy -> IO Ty
pruneTy (RevPruning pr) a = go pr (PRen Nothing 0 0 mempty) a where
  go pr pren a = case (pr, force a) of
    ([]          , a          ) -> rename pren a
    (Just{}  : pr, VPi x i a b) -> Pi x i <$> rename pren a
                                          <*> go pr (lift pren) (b $$ VVar (cod pren))
    (Nothing : pr, VPi x i a b) -> go pr (skip pren) (b $$ VVar (cod pren))
    _                           -> impossible

-- | Prune arguments from a meta, return new meta.
pruneMeta :: Pruning -> MetaVar -> IO MetaVar
pruneMeta pruning m = do
  (mty, mst) <- readUnsolved m
  prunedty   <- eval [] <$> pruneTy (revPruning pruning) mty
  m'         <- newMeta prunedty mst
  let solution = eval [] $ lams (Lvl $ length pruning) mty $ AppPruning (Meta m') pruning
  modifyIORef' mcxt $ IM.insert (coerce m) (Solved solution mty mst)
  pure m'


-- | Eta expand an unsolved meta. This removes splicing from spines of the meta.
etaExpandMeta :: MetaVar -> IO ()
etaExpandMeta m = do
  (a, s) <- readUnsolved m

  let go :: Cxt -> VTy -> Stage -> IO Tm
      go cxt a s = case force a of
        VPi x i a b -> Lam x i (quote (lvl cxt) a) <$> go (bind cxt x a s) (b $$ VVar (lvl cxt)) s
        VLift a     -> Quote <$> go cxt a S0
        a           -> freshMeta cxt a s

  let cxt0 = emptyCxt (initialPos "")
  t <- go cxt0 a s
  modifyIORef' mcxt $ IM.insert (coerce m) (Solved (eval [] t) a s)

etaExpandVFlex :: MetaVar -> Spine -> IO (MetaVar, Spine)
etaExpandVFlex m sp = do
  let hasSplice SId           = False
      hasSplice (SApp sp _ _) = hasSplice sp
      hasSplice SSplice{}     = True
  if hasSplice sp then do
    etaExpandMeta m
    VFlex m sp <- pure $! force (VFlex m sp)
    pure (m, sp)
  else do
    pure (m, sp)

data SpinePruneStatus
  = OKRenaming     -- ^ Valid spine which is a renaming
  | OKNonRenaming  -- ^ Valid spine but not a renaming (has a non-var entry)
  | NeedsPruning   -- ^ A spine which is a renaming and has out-of-scope var entries

-- | Prune illegal var occurrences from a meta + spine.
--   Returns: renamed + pruned term.
pruneVFlex :: PartialRenaming -> MetaVar -> Spine -> IO Tm
pruneVFlex pren m sp = do

  -- eta-expand splices if necessary
  (m, sp) <- etaExpandVFlex m sp

  -- rename spine while computing the required pruning, if there's one
  (sp :: [(Maybe Tm, Icit)], status :: SpinePruneStatus) <- let

    go :: Spine -> IO ([(Maybe Tm, Icit)], SpinePruneStatus)
    go SId           = pure ([], OKRenaming)
    go (SApp sp t i) = do

      (sp, status) <- go sp

      let varCase x = case (IM.lookup (coerce x) (ren pren), status) of
            (Just x , _            ) -> pure ((Just (Var (lvl2Ix (dom pren) x)), i):sp, status)
            (Nothing, OKNonRenaming) -> throwIO UnifyError
            (Nothing, _            ) -> pure ((Nothing, i):sp, NeedsPruning)

      case force t of
        VVar x                 -> varCase x
        VRigid x (SSplice SId) -> varCase x
        VQuote (VVar x)        -> varCase x

        t -> case status of
          NeedsPruning -> throwIO UnifyError
          _            -> do {t <- rename pren t; pure ((Just t, i):sp, OKNonRenaming)}

    go SSplice{} = impossible

    in go sp

  m' <- case status of
    OKRenaming    -> readUnsolved m >> pure m
    OKNonRenaming -> readUnsolved m >> pure m
    NeedsPruning  -> pruneMeta (map (\(mt, i) -> i <$ mt) sp) m

  let t = foldr' (\(mu, i) t -> maybe t (\u -> App t u i) mu) (Meta m') sp
  pure t

renameSp :: PartialRenaming -> Tm -> Spine -> IO Tm
renameSp pren t = \case
  SId            -> pure t
  SApp sp u i    -> App <$> renameSp pren t sp <*> rename pren u <*> pure i
  SSplice sp     -> Splice <$> renameSp pren t sp

rename :: PartialRenaming -> Val -> IO Tm
rename pren t = case force t of

  VFlex m' sp -> case occ pren of
    Just m | m == m' -> throwIO UnifyError -- occurs check
    _                -> pruneVFlex pren m' sp

  VRigid (Lvl x) sp -> case IM.lookup x (ren pren) of
    Nothing -> throwIO UnifyError  -- scope error ("escaping variable" error)
    Just x' -> renameSp pren (Var $ lvl2Ix (dom pren) x') sp

  VLam x i a t -> Lam x i <$> rename pren a <*> rename (lift pren) (t $$ VVar (cod pren))
  VPi x i a b  -> Pi x i <$> rename pren a <*> rename (lift pren) (b $$ VVar (cod pren))
  VU s         -> pure $ U s
  VLift t      -> Lift <$> rename pren t
  VQuote t     -> Quote <$> rename pren t

-- | Wrap a term in Lvl number of lambdas. We get the domain info from the VTy
--   argument.
lams :: Lvl -> VTy -> Tm -> Tm
lams l a t = go a (0 :: Lvl) where
  go a l' | l' == l = t
  go a l' = case force a of
    VPi "_" i a b -> Lam ("x"++show l') i (quote l' a) $ go (b $$ VVar l') (l' + 1)
    VPi x i a b   -> Lam x i (quote l' a) $ go (b $$ VVar l') (l' + 1)
    _             -> impossible

-- | Solve (Γ ⊢ m spine =? rhs)
solve :: Lvl -> MetaVar -> Spine -> Val -> IO ()
solve gamma m sp rhs = do
  (m, sp) <- etaExpandVFlex m sp
  pren <- invert gamma sp
  solveWithPRen m pren rhs

-- | Solve m given the result of inversion on a spine.
solveWithPRen :: MetaVar -> (PartialRenaming, Maybe Pruning) -> Val -> IO ()
solveWithPRen m (pren, pruneNonlinear) rhs = do

  (mty, mst) <- readUnsolved m

  -- if the spine was non-linear, we check that the non-linear arguments
  -- can be pruned from the meta type (i.e. that the pruned solution will
  -- be well-typed)
  case pruneNonlinear of
    Nothing -> pure ()
    Just pr -> () <$ pruneTy (revPruning pr) mty

  rhs <- rename (pren {occ = Just m}) rhs
  let solution = eval [] $ lams (dom pren) mty rhs
  modifyIORef' mcxt $ IM.insert (coerce m) (Solved solution mty mst)

unifySp :: Lvl -> Spine -> Spine -> IO ()
unifySp l sp sp' = case (sp, sp') of
  (SId         , SId            ) -> pure ()

  -- Note: we don't have to compare Icit-s, since we know from the recursive
  -- call that sp and sp' have the same type.
  (SApp sp t _, SApp sp' t' _)  -> unifySp l sp sp' >> unify l t t'
  (SSplice sp , SSplice sp'  )  -> unifySp l sp sp'
  _                             -> throwIO UnifyError -- rigid mismatch error

-- | Solve (Γ ⊢ m spine =? m' spine').
flexFlex :: Dbg => Lvl -> MetaVar -> Spine -> MetaVar -> Spine -> IO ()
flexFlex gamma m sp m' sp' = do
  (m,  sp)  <- etaExpandVFlex m sp
  (m', sp') <- etaExpandVFlex m' sp'
  try (invert gamma sp) >>= \case
    Left UnifyError -> solve gamma m' sp' (VFlex m sp)
    Right pren      -> solveWithPRen m pren (VFlex m' sp')

unify :: Lvl -> Val -> Val -> IO ()
unify l t u = case (force t, force u) of
  (VU s        , VU s'          ) | s == s' -> pure ()
  (VPi x i a b , VPi x' i' a' b') | i == i' -> unify l a a' >> unify (l + 1) (b $$ VVar l) (b' $$ VVar l)
  (VLift t     , VLift t'       )           -> unify l t t'
  (VQuote t    , VQuote t'      ) -> unify l t t'
  (VRigid x sp , VRigid x' sp'  ) | x == x' -> unifySp l sp sp'

  (VLam _ _ _ t, VLam _ _ _ t'  ) -> unify (l + 1) (t $$ VVar l) (t' $$ VVar l)
  (t           , VLam _ i _ t'  ) -> unify (l + 1) (vApp t (VVar l) i) (t' $$ VVar l)
  (VLam _ i _ t, t'             ) -> unify (l + 1) (t $$ VVar l) (vApp t' (VVar l) i)

  (VFlex m sp  , VFlex m' sp'   ) | m == m' -> unifySp l sp sp'
                                  | True    -> flexFlex l m sp m' sp'
  (VFlex m sp  , t'             ) -> solve l m sp t'
  (t           , VFlex m' sp'   ) -> solve l m' sp' t

  _                               -> throwIO UnifyError  -- rigid mismatch error

unifyCatch :: Cxt -> Val -> Val -> IO ()
unifyCatch cxt t t' =
  unify (lvl cxt) t t'
  `catch` \UnifyError ->
    throwIO $ Error cxt $ CantUnify (quote (lvl cxt) t) (quote (lvl cxt) t')
