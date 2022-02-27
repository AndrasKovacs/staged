
module Unification (unify, unifyCatch, freshMeta) where

import Control.Monad
import Control.Applicative
import Control.Exception
import Data.IORef
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import Data.Maybe

import Common
import Errors
import Evaluation
import Metacontext
import Syntax
import Value
import Cxt
-- import Pretty

--------------------------------------------------------------------------------

freshMeta :: Cxt -> VTy -> Stage -> IO Tm
freshMeta cxt a st = do
  let ~closed = eval [] $ closeTy (path cxt) (quote (lvl cxt) a)
  m <- newMeta closed st
  pure $ InsertedMeta m (pruning cxt)

readUnsolved :: MetaVar -> IO (VTy, Stage)
readUnsolved m = readMeta m >>= \case
  Unsolved a s -> pure (a, s)
  _            -> impossible

--------------------------------------------------------------------------------

-- | partial substitution from Γ to Δ
data PartialSub = PSub {
    occ :: Maybe MetaVar   -- optional occurs check
  , dom :: Lvl             -- size of Γ
  , cod :: Lvl             -- size of Δ
  , sub :: IM.IntMap Val}  -- mapping from Δ vars to Γ values

-- | lift : (σ : Psub Γ Δ) → Psub (Γ, x : A[σ]) (Δ, x : A)
lift :: PartialSub -> PartialSub
lift (PSub occ dom cod sub) = PSub occ (dom + 1) (cod + 1) (IM.insert (unLvl cod) (VVar dom) sub)

-- | skip : Psub Γ Δ → Psub Γ (Δ, x : A)
skip :: PartialSub -> PartialSub
skip (PSub occ dom cod sub) = PSub occ dom (cod + 1) sub


-- | invert : (Γ : Cxt) → (spine : Sub Δ Γ) → Psub Γ Δ
--   Optionally returns a pruning of nonlinear spine entries, if there's any.
invert :: Lvl -> Spine -> IO (PartialSub, Maybe Pruning)
invert gamma sp = do

  let go :: Spine -> IO (Lvl, IS.IntSet, IM.IntMap Val, Pruning, Bool)
      go SId             = pure (0, mempty, mempty, [], True)
      go (SApp sp t i _) = do
        (!dom, !domvars, !sub, !pr, !isLinear) <- go sp

        let invertVal x invx = case IS.member x domvars of
              True  -> pure (dom + 1, domvars,             IM.delete x sub     , Nothing : pr, False   )
              False -> pure (dom + 1, IS.insert x domvars, IM.insert x invx sub, Just i  : pr, isLinear)

        case force t of
          VVar (Lvl x)                 -> invertVal x (VVar dom)
          VQuote (VVar (Lvl x))        -> invertVal x (VRigid dom (SSplice SId))
          VRigid (Lvl x) (SSplice SId) -> invertVal x (VQuote (VVar dom))
          _                            -> throwIO UnifyError

      go SSplice{}  = impossible -- should be already expanded away
      go SNatElim{} = impossible -- should be already split off

  (dom, domvars, sub, pr, isLinear) <- go sp
  pure (PSub Nothing dom gamma sub, pr <$ guard isLinear)


-- | Remove some arguments from a closed iterated Pi type.
pruneTy :: RevPruning -> VTy -> IO Ty
pruneTy (RevPruning pr) a = go pr (PSub Nothing 0 0 mempty) a where
  go pr psub a = case (pr, force a) of
    ([]          , a          ) -> psubst psub a
    (Just{}  : pr, VPi x i a b) -> Pi x i <$!> psubst psub a
                                          <*!> go pr (lift psub) (b $ VVar (cod psub))
    (Nothing : pr, VPi x i a b) -> go pr (skip psub) (b $ VVar (cod psub))
    _                           -> impossible

-- | Prune arguments from a meta, return solution value.
pruneMeta :: Pruning -> MetaVar -> IO Val
pruneMeta pruning m = do
  (mty, mst) <- readUnsolved m
  prunedty   <- eval [] <$!> pruneTy (revPruning pruning) mty
  m'         <- newMeta prunedty mst
  let solution = eval [] $! lams (Lvl $ length pruning) mty $ AppPruning (Meta m') pruning
  modifyIORef' mcxt $ IM.insert (coerce m) (Solved solution mty mst)
  pure solution


-- | Eta expand an unsolved meta, return the solution value. This removes
--   splices from spines of the meta.
etaExpandMeta :: MetaVar -> IO Val
etaExpandMeta m = do
  (!a, !s) <- readUnsolved m

  let go :: Cxt -> VTy -> Stage -> IO Tm
      go cxt a s = case force a of
        VPi x i a b -> Lam x i (quote (lvl cxt) a) <$!> go (bind cxt x a s) (b $ VVar (lvl cxt)) s
                                                   <*!> pure V0
        VLift a     -> Quote <$!> go cxt a S0
        a           -> freshMeta cxt a s

  t <- go (emptyCxt (initialPos "")) a s
  let val = eval [] t
  modifyIORef' mcxt $ IM.insert (coerce m) (Solved val a s)
  pure val

-- | Eta-expand splices in the spine, if possible.
expandVFlex :: MetaVar -> Spine -> IO (MetaVar, Spine)
expandVFlex m sp = do

  let hasSplice SId             = False
      hasSplice (SApp sp _ _ _) = hasSplice sp
      hasSplice SSplice{}       = True
      hasSplice SNatElim{}      = impossible -- already split spine

  if hasSplice sp then do
    m          <- etaExpandMeta m
    VFlex m sp <- pure $! vAppSp m sp
    pure (m, sp)
  else do
    pure (m, sp)

-- | Eta-expand and then prune spine to the extent that it is possible.
pruneVFlex :: PartialSub -> MetaVar -> Spine -> IO (MetaVar, Spine)
pruneVFlex psub m sp = do

  -- eta-expand splices if possible
  (!m, !sp) <- expandVFlex m sp

  let pruning :: Spine -> Maybe Pruning
      pruning SId = Just []
      pruning (SApp sp t i _) = do
        pr <- pruning sp

        let varCase x = case IM.lookup (coerce x) (sub psub) of
              Just{}   -> pure (pr :> Just i )
              Nothing  -> pure (pr :> Nothing)

        case force t of
          VVar x                 -> varCase x
          VRigid x (SSplice SId) -> varCase x
          VQuote (VVar x)        -> varCase x
          _                      -> Nothing

      pruning SSplice{}  = Nothing
      pruning SNatElim{} = Nothing

  case pruning sp of
    Just pr | any isNothing pr -> do
      m          <- pruneMeta pr m
      VFlex m sp <- pure $! vAppSp m sp
      pure (m, sp)
    _ ->
      pure (m, sp)


psubstSp :: PartialSub -> Tm -> Spine -> IO Tm
psubstSp psub t = \case
  SId                  -> pure t
  SApp sp u i o        -> App <$!> psubstSp psub t sp <*!> psubst psub u <*!> pure i <*!> pure o
  SSplice sp           -> Splice <$!> psubstSp psub t sp
  SNatElim st p s z sp -> tNatElim st <$!> psubst psub p <*!> psubst psub s
                                      <*!> psubst psub z <*!> psubstSp psub t sp


-- | Quote a `Val` to normal form, while applying a partial substitution.
psubst :: PartialSub -> Val -> IO Tm
psubst psub t = case force t of

  VFlex m' sp -> case occ psub of
    Just m | m == m' -> throwIO UnifyError -- occurs check
    _                -> do
      (!sp, !outer) <- pure $ splitSpine sp
      (!m', !sp)    <- pruneVFlex psub m' sp
      inner         <- psubstSp psub (Meta m') sp
      psubstSp psub inner outer


  VRigid (Lvl x) sp -> case IM.lookup x (sub psub) of
    Nothing -> throwIO UnifyError  -- scope error ("escaping variable" error)
    Just v  -> psubstSp psub (quote (dom psub) v) sp

  VLam x i a t o -> Lam x i <$!> psubst psub a <*!> psubst (lift psub) (t $ VVar (cod psub))
                                               <*!> pure o
  VPi x i a b    -> Pi x i <$!> psubst psub a <*!> psubst (lift psub) (b $ VVar (cod psub))
  VU s           -> pure $ U s
  VLift t        -> Lift <$!> psubst psub t
  VQuote t       -> Quote <$!> psubst psub t
  VNat s         -> pure (Nat s)
  VZero s        -> pure (Zero s)
  VSuc s t       -> tSuc s <$!> psubst psub t

-- | Wrap a term in Lvl number of lambdas. We get the domain info from the VTy
--   argument.
lams :: Lvl -> VTy -> Tm -> Tm
lams l a t = go a (0 :: Lvl) where
  go a l' | l' == l = t
  go a l' = case force a of
    VPi "_" i a b -> Lam ("x"++show l') i (quote l' a) (go (b $ VVar l') (l' + 1)) V0
    VPi x i a b   -> Lam x i (quote l' a) (go (b $ VVar l') (l' + 1)) V0
    _             -> impossible

-- | Split a spine to a NatElim-free prefix and the rest.
splitSpine :: Spine -> (Spine, Spine)
splitSpine sp = maybe (sp, SId) id (go sp) where
  go :: Spine -> Maybe (Spine, Spine)
  go SId                     = Nothing
  go (SApp sp u i v)         = (\(l, r) -> (l, SApp r u i v)) <$!> go sp
  go (SSplice sp)            = (\(l, r) -> (l, SSplice r)) <$!> go sp
  go (SNatElim  st p s z sp) = (\(l, r) -> (l, SNatElim st p s z r)) <$!> (go sp <|> pure (sp, SId))

-- | Solve (Γ ⊢ m spine =? rhs)
solve :: Lvl -> MetaVar -> Spine -> Val -> IO ()
solve l m topSp topRhs = do
  (!sp, !outer) <- pure $! splitSpine topSp
  (!m, !sp)     <- expandVFlex m sp
  psub          <- invert l sp
  if isSId outer then do
    solveWithPSub m psub topRhs
  else case force topRhs of
    VRigid x rhsSp -> do

      let go SId                   sp'                       = solveWithPSub m psub (VRigid x sp')
          go (SApp sp u _ _)       (SApp sp' u' _ _)         = go sp sp' >> unify l u u'
          go (SSplice sp)          (SSplice sp')             = go sp sp'
          go (SNatElim _ p s z sp) (SNatElim _ p' s' z' sp') = unify l p p' >> unify l s s' >> unify l z z'
                                                             >> go sp sp'
          go _ _ = throwIO UnifyError

      go outer rhsSp
    _ ->
      throwIO UnifyError


-- | Solve m given the result of inversion on a spine.
solveWithPSub :: MetaVar -> (PartialSub, Maybe Pruning) -> Val -> IO ()
solveWithPSub m (psub, pruneNonlinear) rhs = do

  (!mty, !mst) <- readUnsolved m

  -- if the spine was non-linear, we check that the non-linear arguments
  -- can be pruned from the meta type (i.e. that the pruned solution will
  -- be well-typed)
  case pruneNonlinear of
    Nothing -> pure ()
    Just pr -> () <$ pruneTy (revPruning pr) mty

  rhs <- psubst (psub {occ = Just m}) rhs
  let solution = eval [] $ lams (dom psub) mty rhs
  modifyIORef' mcxt $ IM.insert (coerce m) (Solved solution mty mst)

unifySp :: Lvl -> Spine -> Spine -> IO ()
unifySp l sp sp' = case (sp, sp') of
  (SId                  , SId                    ) -> pure ()
  (SApp sp t _ _        , SApp sp' t' _ _        ) -> unifySp l sp sp' >> unify l t t'
  (SSplice sp           , SSplice sp'            ) -> unifySp l sp sp'
  (SNatElim st p s z sp , SNatElim _ p' s' z' sp') -> unify l p p' >> unify l s s' >> unify l z z'
                                                      >> unifySp l sp sp'
  _                                                -> throwIO UnifyError

isSId :: Spine -> Bool
isSId SId = True
isSId _   = False

flexFlex :: Lvl -> MetaVar -> Spine -> MetaVar -> Spine -> IO ()
flexFlex gamma m sp m' sp' = do
  let go = do
        (sp, outer) <- pure $! splitSpine sp
        unless (isSId outer) $ throwIO UnifyError
        (m, sp) <- expandVFlex m sp
        psub <- invert gamma sp
        pure (m, sp, psub)

  try go >>= \case
    Left UnifyError     -> solve gamma m' sp' (VFlex m sp)
    Right (m, sp, psub) -> solveWithPSub m psub (VFlex m' sp')


intersect :: Lvl -> MetaVar -> Spine -> Spine -> IO ()
intersect l m sp sp' = do
  (sp , outer)  <- pure $! splitSpine sp
  (sp', outer') <- pure $! splitSpine sp'
  if isSId outer && isSId outer' then do

    (m', sp)    <- expandVFlex m sp             -- expand m
    VFlex _ sp' <- pure $! force (VFlex m sp')  -- force sp' with old m
    m           <- pure m'                      -- we don't care about old m anymore

    let go SId SId = Just []
        go (SApp sp t i _) (SApp sp' t' _ _) = case (force t, force t') of
          (VVar x                , VVar x'                ) -> ((i <$ guard (x == x')):) <$!> go sp sp'
          (VQuote (VVar x)       , VQuote (VVar x')       ) -> ((i <$ guard (x == x')):) <$!> go sp sp'
          (VRigid x (SSplice SId), VRigid x' (SSplice SId)) -> ((i <$ guard (x == x')):) <$!> go sp sp'
          _                                                 -> Nothing
        go _ _ = impossible

    case go sp sp' of
      Nothing                      -> unifySp l sp sp'
      Just pr | any (==Nothing) pr -> () <$ pruneMeta pr m
              | otherwise          -> pure ()

  else do
    unifySp l sp sp'


unify :: Lvl -> Val -> Val -> IO ()
unify l t u = case (force t, force u) of
  (VU s        , VU s'          ) | s == s' -> pure ()
  (VPi x i a b , VPi x' i' a' b') | i == i' -> unify l a a' >> unify (l + 1) (b $ VVar l) (b' $ VVar l)
  (VLift t     , VLift t'       )           -> unify l t t'
  (VQuote t    , VQuote t'      )           -> unify l t t'
  (VRigid x sp , VRigid x' sp'  ) | x == x' -> unifySp l sp sp'
  (VNat _      , VNat _         )           -> pure ()
  (VZero _     , VZero _        )           -> pure ()
  (VSuc _ t    , VSuc _ t'      )           -> unify l t t'

  (VLam _ _ _ t _, VLam _ _ _ t' _) -> unify (l + 1) (t $ VVar l) (t' $ VVar l)
  (t             , VLam _ i _ t' o) -> unify (l + 1) (vApp t (VVar l) i o) (t' $ VVar l)
  (VLam _ i _ t o, t'             ) -> unify (l + 1) (t $ VVar l) (vApp t' (VVar l) i o)

  (VFlex m sp  , VFlex m' sp'   ) | m == m' -> intersect l m sp sp' -- unifySp l sp sp'
                                  | True    -> flexFlex l m sp m' sp'
  (VFlex m sp  , t'             ) -> solve l m sp t'
  (t           , VFlex m' sp'   ) -> solve l m' sp' t

  _                               -> throwIO UnifyError

unifyCatch :: Cxt -> Val -> Val -> IO ()
unifyCatch cxt t t' =
  unify (lvl cxt) t t'
  `catch` \UnifyError ->
    throwIO $ Error cxt $ CantUnify (quote (lvl cxt) t) (quote (lvl cxt) t')
