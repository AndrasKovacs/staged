
module Elaboration (check, infer, checkEverything) where

import Control.Exception
import Control.Monad
import Data.IORef
import Data.Maybe

import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Data.Map.Strict as M

import Common
import Cxt
import Errors
import Evaluation
import Metacontext
import Pretty
import Syntax
import Value

import qualified Presyntax as P

{-
TODO: handle quote/splice in unification & inversion
-}

-- Postponed checking
--------------------------------------------------------------------------------

-- | Unify the result of a postponed checking with its placeholder metavariable.
unifyPlaceholder :: Dbg => Cxt -> Tm -> MetaVar -> IO ()
unifyPlaceholder cxt t m = case lookupMeta m of

  -- If the placeholder meta is unsolved, we can solve it efficiently here,
  -- without any possibility of failure.
  Unsolved bs a -> do
    debug ["solve unconstrained placeholder", show m, showTm0 (closeTm (locals cxt) t)]

    -- we can simply close the checked term, to get the solution
    let solution = closeTm (locals cxt) t
    writeMeta m (Solved (eval [] solution) a)
    forM_ (IS.toList bs) (retryCheck . CheckVar)

  -- otherwise we have to do full unification
  Solved v _ -> do
    debug ["unify solved placeholder", showTm cxt t, show m, showVal cxt v]

    unifyCatch cxt
      (eval (env cxt) t)
      (vAppPruning (env cxt) v (pruning cxt))
      Placeholder

-- | Try to perform a delayed checking.
retryCheck :: Dbg => CheckVar -> IO ()
retryCheck c = case lookupCheck c of
  Unchecked cxt t a m -> case force a of
    -- still blocked by another meta
    VFlex m' _ -> do
      addBlocking c m'

    -- checking unblocked
    a -> do
      t <- check cxt t a
      unifyPlaceholder cxt t m
      writeCheck c $ Checked t
  _ ->
    pure ()

-- | Unblock and perform all remaining checking problems, assuming each time
--   that no implicit insertion should occur.
checkEverything :: Dbg => IO ()
checkEverything = go 0 where
  go :: CheckVar -> IO ()
  go c = do
    c' <- readIORef nextCheckVar
    when (c < c') $ do
      case lookupCheck c of
        Unchecked cxt t a m -> do
          debug ["checkEverything", show c, show c']
          (t, tty) <- insert cxt $ infer cxt t
          writeCheck c (Checked t)
          unifyCatch cxt a tty ExpectedInferred
          unifyPlaceholder cxt t m
        _ ->
          pure ()
      go (c + 1)

-- Unification
--------------------------------------------------------------------------------

-- | A partial renaming from Γ to Δ.
data PartialSub = PSub {
    occ :: Maybe MetaVar   -- ^ Optional occurs check.
  , dom :: Lvl             -- ^ Size of Γ.
  , cod :: Lvl             -- ^ Size of Δ.
  , sub :: IM.IntMap Val}  -- ^ Mapping from Δ vars to Γ vars.

-- | @lift : (σ : PSub Γ Δ) → PSub (Γ, x : A[σ]) (Δ, x : A)@
lift :: PartialSub -> PartialSub
lift (PSub occ dom cod sub) = PSub occ (dom + 1) (cod + 1) (IM.insert (unLvl cod) (VVar dom) sub)

-- | @skip : PSub Γ Δ → PSub Γ (Δ, x : A)@
skip :: PartialSub -> PartialSub
skip (PSub occ dom cod sub) = PSub occ dom (cod + 1) sub

-- | Eta expand an unsolved meta, return the solution value. This removes
--   splices from spines of the meta.
etaExpandMeta :: MetaVar -> IO Val
etaExpandMeta m = do
  (!_, !a) <- readUnsolved m

  let go :: Cxt -> VTy -> IO Tm
      go cxt a = case force a of
        VPi x i a b -> Lam x i <$> go (bind cxt x a) (b $$ VVar (lvl cxt))
        VBox a      -> Quote <$> go cxt a
        a           -> freshMeta cxt a

  t <- go (emptyCxt (initialPos "")) a
  let val = eval [] t
  modifyIORef' mcxt $ IM.insert (coerce m) (Solved val a)
  pure val

-- | Eta-expand splices in the spine, if possible.
expandVFlex :: MetaVar -> Spine -> IO (MetaVar, Spine)
expandVFlex m sp = do

  let hasSplice SId           = False
      hasSplice (SApp sp _ _) = hasSplice sp
      hasSplice SSplice{}     = True

  if hasSplice sp then do
    m          <- etaExpandMeta m
    VFlex m sp <- pure $! vAppSp m sp
    pure (m, sp)
  else do
    pure (m, sp)

-- | @invert : (Γ : Cxt) → (spine : Sub Δ Γ) → PSub Γ Δ@
--   Optionally returns a pruning of nonlinear spine entries, if there's any.
invert :: Lvl -> Spine -> IO (PartialSub, Maybe Pruning)
invert gamma sp = do
  let go :: Spine -> IO (Lvl, IM.IntMap Val, IS.IntSet, [(Lvl, Icit)])
      go SId                                = pure (0, mempty, mempty, [])
      go (SApp sp (force -> VVar (Lvl x)) i) = do
        (!dom, !sub, !nlvars, !fsp) <- go sp
        case IM.member x sub || IS.member x nlvars of
          True  -> pure $! (,,,) $$! (dom + 1) $$! (IM.delete x sub)            $$! (IS.insert x nlvars) $$! (fsp :> (Lvl x, i))
          False -> pure $! (,,,) $$! (dom + 1) $$! (IM.insert x (VVar dom) sub) $$! nlvars               $$! (fsp :> (Lvl x, i))
      go SApp{}    = throwIO UnifyException
      go SSplice{} = impossible

  (!dom, !sub, !nlvars, !fsp) <- go sp

  let mask :: [(Lvl, Icit)] -> Pruning
      mask []                  = []
      mask (fsp :> (Lvl x, i))
        | IS.member x nlvars   = Nothing : mask fsp
        | otherwise            = Just i  : mask fsp

  pure (PSub Nothing dom gamma sub, mask fsp <$ guard (not $ IS.null nlvars))

-- | Remove some arguments from a closed iterated Pi type.
pruneTy :: Dbg => RevPruning -> VTy -> IO Ty
pruneTy (RevPruning pr) a = go pr (PSub Nothing 0 0 mempty) a where
  go pr psub a = case (pr, force a) of
    ([]          , a          ) -> psubst psub a
    (Just{}  : pr, VPi x i a b) -> Pi x i <$> psubst psub a
                                          <*> go pr (lift psub) (b $$ VVar (cod psub))
    (Nothing : pr, VPi x i a b) -> go pr (skip psub) (b $$ VVar (cod psub))
    _                           -> impossible

-- | Prune arguments from a meta, return pruned value.
pruneMeta :: Dbg => Pruning -> MetaVar -> IO Val
pruneMeta pruning m = do
  (!bs, !mty) <- readUnsolved m
  prunedty <- eval [] <$> pruneTy (revPruning pruning) mty
  m' <- newRawMeta bs prunedty
  let solution = eval [] $ lams (Lvl $ length pruning) mty $ AppPruning (Meta m') pruning
  writeMeta m (Solved solution mty)
  pure solution

-- | Eta-expand and then prune spine, both to the extent that it's possible.
pruneVFlex :: PartialSub -> MetaVar -> Spine -> IO (MetaVar, Spine)
pruneVFlex psub m sp = do

  -- eta-expand splices if possible
  (!m, !sp) <- expandVFlex m sp

  let pruning :: Spine -> Maybe Pruning
      pruning SId           = Just []
      pruning (SApp sp t i) = do
        pr <- pruning sp

        let varCase x = case IM.lookup (coerce x) (sub psub) of
              Just{}   -> pure (pr :> Just i )
              Nothing  -> pure (pr :> Nothing)

        case force t of
          VVar x                 -> varCase x
          VRigid x (SSplice SId) -> varCase x
          VQuote (VVar x)        -> varCase x
          _                      -> Nothing

      pruning SSplice{} = impossible

  case pruning sp of
    Just pr | any isNothing pr -> do
      m          <- pruneMeta pr m
      VFlex m sp <- pure $! vAppSp m sp
      pure (m, sp)
    _ ->
      pure (m, sp)

psubstSp :: Dbg => PartialSub -> Tm -> Spine -> IO Tm
psubstSp pren t = \case
  SId            -> pure t
  SApp sp u i    -> App <$> psubstSp pren t sp <*> psubst pren u <*> pure i
  SSplice sp     -> Splice <$> psubstSp pren t sp

psubst :: Dbg => PartialSub -> Val -> IO Tm
psubst psub t = case force t of

  VFlex m' sp -> case occ psub of
    Just m | m == m' -> throwIO UnifyException -- occurs check
    _                -> do (!m', !sp) <- pruneVFlex psub m' sp
                           psubstSp psub (Meta m') sp

  VRigid (Lvl x) sp -> case IM.lookup x (sub psub) of
    Nothing -> throwIO UnifyException  -- scope error ("escaping variable" error)
    Just v  -> psubstSp psub (quote (dom psub) v) sp

  VLam x i t  -> Lam x i <$> psubst (lift psub) (t $$ VVar (cod psub))
  VPi x i a b -> Pi x i <$> psubst psub a <*> psubst (lift psub) (b $$ VVar (cod psub))
  VU          -> pure U
  VBox t      -> Box <$> psubst psub t
  VQuote t    -> Quote <$> psubst psub t
  VEff t      -> Eff <$> psubst psub t
  VReturn t   -> Return <$> psubst psub t
  VBind x t u -> Bind x <$> psubst psub t <*> psubst (lift psub) (u $$ VVar (cod psub))
  VSeq t u    -> Seq <$> psubst psub t <*> psubst psub u
  VUnit       -> pure Unit
  VTt         -> pure Tt
  VRef t      -> Ref <$> psubst psub t
  VNew t      -> New <$> psubst psub t
  VWrite t u  -> Write <$> psubst psub t <*> psubst psub u
  VRead t     -> Read <$> psubst psub t

-- | Wrap a term in Lvl number of lambdas. We get the domain info from the
--   VTy argument.
lams :: Dbg => Lvl -> VTy -> Tm -> Tm
lams l a t = go a (0 :: Lvl) where
  go a l' | l' == l = t
  go a l' = case force a of
    VPi "_" i a b -> Lam ("x"++show l') i $ go (b $$ VVar l') (l' + 1)
    VPi x i a b   -> Lam x i $ go (b $$ VVar l') (l' + 1)
    _             -> impossible

-- | Solve (Γ ⊢ m spine =? rhs)
solve :: Dbg => Lvl -> MetaVar -> Spine -> Val -> IO ()
solve gamma m sp rhs = do
  (!m, !sp) <- expandVFlex m sp
  psub <- invert gamma sp
  solveWithPSub gamma m psub rhs

-- | Solve m given the result of inversion on a spine.
solveWithPSub :: Dbg => Lvl -> MetaVar -> (PartialSub, Maybe Pruning) -> Val -> IO ()
solveWithPSub gamma m (psub, pruneNonlinear) rhs = do

  debug ["solve", show m, showTm0 (quote gamma rhs)]

  (blocked, mty) <- readUnsolved m

  -- if the spine was non-linear, we check that the non-linear arguments
  -- can be pruned from the meta type (i.e. that the pruned solution will
  -- be well-typed)
  case pruneNonlinear of
    Nothing -> pure ()
    Just pr -> () <$ pruneTy (revPruning pr) mty

  rhs <- psubst (psub {occ = Just m}) rhs
  let solution = eval [] $ lams (dom psub) mty rhs
  writeMeta m (Solved solution mty)

  -- retry all blocked problems
  forM_ (IS.toList blocked) (retryCheck . CheckVar)


unifySp :: Dbg => Lvl -> Spine -> Spine -> IO ()
unifySp l sp sp' = case (sp, sp') of
  (SId         , SId           ) -> pure ()

  -- Note: we don't have to compare Icit-s, since we know from the recursive
  -- call that sp and sp' have the same type.
  (SApp sp t _ , SApp sp' t' _ ) -> unifySp l sp sp' >> unify l t t'
  (SSplice sp  , SSplice sp'   ) -> unifySp l sp sp'

  _                              -> throwIO UnifyException -- rigid mismatch error


-- | Solve (Γ ⊢ m spine =? m' spine').
flexFlex :: Dbg => Lvl -> MetaVar -> Spine -> MetaVar -> Spine -> IO ()
flexFlex gamma m sp m' sp' = let

  -- It may be that only one of the two spines is invertible
  go :: Dbg => MetaVar -> Spine -> MetaVar -> Spine -> IO ()
  go m sp m' sp' = try (invert gamma sp) >>= \case
    Left UnifyException -> solve gamma m' sp' (VFlex m sp)
    Right psub          -> solveWithPSub gamma m psub (VFlex m' sp')

  -- usually, a longer spine indicates that the meta is in an inner scope. If we solve
  -- inner metas with outer metas, that means that we have to do less pruning.
  in if spineApps sp < spineApps sp' then go m' sp' m sp
                                     else go m sp m' sp'

-- | Try to solve the problem (Γ ⊢ m spine =? m spine') by intersection.
--   If spine and spine' are both renamings, but different, then
--   we prune all arguments from m which differ in spine and spine'.
--
--   If some of spine/spine' are not renamings, we fall back to simple unification.
intersect :: Dbg => Lvl -> MetaVar -> Spine -> Spine -> IO ()
intersect l m sp sp' = do

  (m', sp)    <- expandVFlex m sp             -- expand m
  VFlex _ sp' <- pure $! force (VFlex m sp')  -- force sp' with old m
  m           <- pure m'                      -- we don't care about old m anymore

  let go SId SId = Just []
      go (SApp sp t i) (SApp sp' t' _) =
        case (force t, force t') of
          (VVar x, VVar x') -> ((i <$ guard (x == x')):) <$> go sp sp'
          _                 -> Nothing
      go _ _ = impossible

  case go sp sp' of
    Nothing -> unifySp l sp sp'
    Just pr | any (==Nothing) pr -> () <$ pruneMeta pr m  -- at least 1 pruned entry
            | otherwise          -> pure ()


unify :: Dbg => Lvl -> Val -> Val -> IO ()
unify l t u = do
  debug ["unify", showTm0 (quote l t), showTm0 (quote l u)]
  case (force t, force u) of
    (VU         , VU             ) -> pure ()
    (VPi x i a b, VPi x' i' a' b') | i == i' -> unify l a a' >> unify (l + 1) (b $$ VVar l) (b' $$ VVar l)
    (VUnit      , VUnit          ) -> pure ()
    (VTt        , VTt            ) -> pure ()
    (VEff t     , VEff t'        ) -> unify l t t'
    (VBox t     , VBox t'        ) -> unify l t t'
    (VQuote t   , VQuote t'      ) -> unify l t t'
    (VReturn t  , VReturn t'     ) -> unify l t t'
    (VBind _ t u, VBind _ t' u'  ) -> unify l t t' >> unify (l + 1) (u $$ VVar l) (u' $$ VVar l)
    (VRef t     , VRef t'        ) -> unify l t t'
    (VRead t    , VRead t'       ) -> unify l t t'
    (VWrite t u , VWrite t' u'   ) -> unify l t t' >> unify l u u'
    (VNew t     , VNew t'        ) -> unify l t t'

    (VRigid x sp, VRigid x' sp'  ) | x == x' -> unifySp l sp sp'
    (VFlex m sp , VFlex m' sp'   ) | m == m' -> intersect l m sp sp'
    (VFlex m sp , VFlex m' sp'   )           -> flexFlex l m sp m' sp'
    (VLam _ _ t , VLam _ _ t'    ) -> unify (l + 1) (t $$ VVar l) (t' $$ VVar l)
    (t          , VLam _ i t'    ) -> unify (l + 1) (vApp t (VVar l) i) (t' $$ VVar l)
    (VLam _ i t , t'             ) -> unify (l + 1) (t $$ VVar l) (vApp t' (VVar l) i)
    (VFlex m sp , t'             ) -> solve l m sp t'
    (t          , VFlex m' sp'   ) -> solve l m' sp' t
    _                              -> throwIO UnifyException  -- rigid mismatch error


-- Elaboration
--------------------------------------------------------------------------------

closeVTy :: Cxt -> VTy -> VTy
closeVTy cxt a = eval [] $ closeTy (locals cxt) (quote (lvl cxt) a)

freshMeta :: Dbg => Cxt -> VTy -> IO Tm
freshMeta cxt a = do
  m <- newRawMeta mempty (closeVTy cxt a)
  debug ["freshMeta", show m, showVal cxt a]
  pure $ AppPruning (Meta m) (pruning cxt)

unifyCatch :: Dbg => Cxt -> Val -> Val -> CantUnify -> IO ()
unifyCatch cxt t t' cant =
  unify (lvl cxt) t t'
  `catch` \UnifyException ->
     throwIO $ Error cxt $ CantUnify (quote (lvl cxt) t) (quote (lvl cxt) t') cant

-- | Insert fresh implicit applications.
insert' :: Dbg => Cxt -> IO (Tm, VTy) -> IO (Tm, VTy)
insert' cxt act = go =<< act where
  go (t, va) = case force va of
    VPi x Impl a b -> do
      m <- freshMeta cxt a
      let mv = eval (env cxt) m
      go (App t m Impl, b $$ mv)
    va -> pure (t, va)

-- | Insert fresh implicit applications to a term which is not
--   an implicit lambda.
insert :: Dbg => Cxt -> IO (Tm, VTy) -> IO (Tm, VTy)
insert cxt act = act >>= \case
  (t@(Lam _ Impl _), va) -> pure (t, va)
  (t               , va) -> insert' cxt (pure (t, va))

-- | Insert fresh implicit applications until we hit a Pi with
--   a particular binder name.
insertUntilName :: Dbg => Cxt -> Name -> IO (Tm, VTy) -> IO (Tm, VTy)
insertUntilName cxt name act = go =<< act where
  go (t, va) = case force va of
    va@(VPi x Impl a b) -> do
      if x == name then
        pure (t, va)
      else do
        m <- freshMeta cxt a
        let mv = eval (env cxt) m
        go (App t m Impl, b $$ mv)
    _ ->
      throwIO $ Error cxt $ NoNamedImplicitArg name

ensureEff :: Cxt -> VTy -> IO VTy
ensureEff cxt a = case force a of
  VEff a ->
    pure a
  a -> do
    res <- eval (env cxt) <$> freshMeta cxt VU
    unifyCatch cxt (VEff res) a ExpectedInferred
    pure res

ensureBox :: Cxt -> VTy -> IO VTy
ensureBox cxt a = case force a of
  VBox a ->
    pure a
  a -> do
    res <- eval (env cxt) <$> freshMeta cxt VU
    unifyCatch cxt (VBox res) a ExpectedInferred
    pure res

ensureRef :: Cxt -> VTy -> IO VTy
ensureRef cxt a = case force a of
  VRef a ->
    pure a
  a -> do
    res <- eval (env cxt) <$> freshMeta cxt VU
    unifyCatch cxt (VRef res) a ExpectedInferred
    pure res

check :: Dbg => Cxt -> P.Tm -> VTy -> IO Tm
check cxt (P.SrcPos pos t) a =
  -- we handle the SrcPos case here, because we do not want to
  -- perform debug printing at position annotations.
  check (cxt {pos = pos}) t a
check cxt t a = do

  debug ["check", show (P.stripPos t), showVal cxt a]

  case (t, force a) of

    -- If the icitness of the lambda matches the Pi type, check as usual
    (P.Lam x i ma t, VPi x' i' a' b') | either (\x -> x == x' && i' == Impl) (==i') i -> do

      case ma of
        Nothing -> pure ()
        Just a  -> do a <- check cxt a VU
                      unifyCatch cxt (eval (env cxt) a) a' LamBinderType

      Lam x i' <$> check (bind cxt x a') t (b' $$ VVar (lvl cxt))

    -- If we're checking a local variable with unknown type, with an implicit function,
    -- we immediately unify types. This is a modest but useful approximation of
    -- polymorphic argument inference.
    (P.Var x, topA@(VPi _ Impl _ _))
      | Just (x, force -> a@(VFlex _ _)) <- M.lookup x (localNames cxt) -> do
      unify (lvl cxt) a topA
      pure (Var (lvl2Ix (lvl cxt) x))

    -- Otherwise if Pi is implicit, insert a new implicit lambda
    (t, VPi x Impl a b) -> do
      Lam x Impl <$> check (newBinder cxt x a) t (b $$ VVar (lvl cxt))

    -- If the checking type is unknown, we postpone checking.
    (t, topA@(VFlex m sp)) -> do
      placeholder <- newRawMeta mempty (closeVTy cxt topA)
      c <- newCheck cxt t topA placeholder
      addBlocking c m

      debug ["postpone", show c, show (P.stripPos t), showVal cxt topA, show placeholder]

      pure $ PostponedCheck c

    (P.Let x a t u, a') -> do
      a <- check cxt a VU
      let ~va = eval (env cxt) a
      t <- check cxt t va
      let ~vt = eval (env cxt) t
      u <- check (define cxt x t vt a va) u a'
      pure (Let x a t u)

    (P.Return t, VEff a) ->
      Return <$> check cxt t a

    (P.Bind x t u, VEff b) -> do
      (t, a) <- do
        (t, tty) <- infer cxt t
        a <- ensureEff cxt tty
        pure (t, a)
      u <- check (bind cxt x a) u (VEff b)
      pure (Bind x t u)

    (P.Quote t, VBox a) -> do
      Quote <$> check cxt t a

    (P.New t, VEff (force -> VRef a)) ->
      New <$> check cxt t a

    (P.Hole, a) ->
      freshMeta cxt a

    (t, expected) -> do
      (t, inferred) <- insert cxt $ infer cxt t
      unifyCatch cxt expected inferred ExpectedInferred
      pure t

infer :: Dbg => Cxt -> P.Tm -> IO (Tm, VTy)
infer cxt (P.SrcPos pos t) =
  -- we handle the SrcPos case here, because we do not want to
  -- perform debug printing at position annotations.
  infer (cxt {pos = pos}) t

infer cxt t = do

  debug ["infer", show (P.stripPos t)]

  res <- case t of

    P.Var x -> do
      case M.lookup x (localNames cxt) of
        Just (x', a) -> pure (Var (lvl2Ix (lvl cxt) x'), a)
        Nothing      -> throwIO $ Error cxt $ NameNotInScope x

    P.Lam x (Right i) ma t -> do
      a  <- eval (env cxt) <$> case ma of
        Nothing -> freshMeta cxt VU
        Just a  -> check cxt a VU

      let cxt' = bind cxt x a
      (t, b) <- insert cxt' $ infer cxt' t
      pure (Lam x i t, VPi x i a $ valToClosure cxt b)

    P.Lam x Left{} ma t ->
      throwIO $ Error cxt $ InferNamedLam

    P.App t u i -> do

      -- choose implicit insertion
      (i, t, tty) <- case i of
        Left name -> do
          (t, tty) <- insertUntilName cxt name $ infer cxt t
          pure (Impl, t, tty)
        Right Impl -> do
          (t, tty) <- infer cxt t
          pure (Impl, t, tty)
        Right Expl -> do
          (t, tty) <- insert' cxt $ infer cxt t
          pure (Expl, t, tty)

      -- ensure that tty is Pi
      (a, b) <- case force tty of
        VPi x i' a b -> do
          unless (i == i') $
            throwIO $ Error cxt $ IcitMismatch i i'
          pure (a, b)
        tty -> do
          a <- eval (env cxt) <$> freshMeta cxt VU
          b <- Closure (env cxt) <$> freshMeta (bind cxt "x" a) VU
          unifyCatch cxt (VPi "x" i a b) tty ExpectedInferred
          pure (a, b)

      u <- check cxt u a
      pure (App t u i, b $$ eval (env cxt) u)

    P.U ->
      pure (U, VU)

    P.Pi x i a b -> do
      a <- check cxt a VU
      b <- check (bind cxt x (eval (env cxt) a)) b VU
      pure (Pi x i a b, VU)

    P.Let x a t u -> do
      a <- check cxt a VU
      let ~va = eval (env cxt) a
      t <- check cxt t va
      let ~vt = eval (env cxt) t
      (u, b) <- infer (define cxt x t vt a va) u
      pure (Let x a t u, b)

    P.Hole -> do
      a <- eval (env cxt) <$> freshMeta cxt VU
      t <- freshMeta cxt a
      pure (t, a)

    P.Box t -> do
      t <- check cxt t VU
      pure (Box t, VU)

    P.Quote t -> do
      (t, tty) <- infer cxt t
      pure (Quote t, VBox tty)

    P.Splice t -> do
      (t, tty) <- infer cxt t
      a <- ensureBox cxt tty
      pure (Splice t, a)

    P.Eff t -> do
      t <- check cxt t VU
      pure (Eff t, VU)

    P.Return t -> do
      (t, tty) <- infer cxt t
      pure (Return t, VEff tty)

    P.Bind x t u -> do
      (t, a) <- do
        (t, tty) <- infer cxt t
        a <- ensureEff cxt tty
        pure (t, a)
      (u, uty) <- infer (bind cxt x a) u
      b <- ensureEff cxt uty
      pure (Bind x t u, VEff b)

    P.Seq t u -> do
      (t, a) <- do
        (t, tty) <- infer cxt t
        a <- ensureEff cxt tty
        pure (t, a)
      (u, uty) <- infer cxt u
      b <- ensureEff cxt uty
      pure (Seq t u, VEff b)

    P.Unit -> do
      pure (Unit, VU)

    P.Tt -> do
      pure (Tt, VUnit)

    P.Ref t -> do
      t <- check cxt t VU
      pure (Ref t, VU)

    P.New t -> do
      (t, tty) <- infer cxt t
      pure (New t, VEff (VRef tty))

    P.Write t u -> do
      (t, tty) <- infer cxt t
      a <- ensureRef cxt tty
      u <- check cxt u a
      pure (Write t u, VEff VUnit)

    P.Read t -> do
      (t, tty) <- infer cxt t
      a <- ensureRef cxt tty
      pure (Read t, VEff a)

  debug ["inferred", showTm cxt (fst res), showVal cxt (snd res)]
  pure res

--------------------------------------------------------------------------------
