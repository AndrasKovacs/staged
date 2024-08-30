{-# language
     Strict, LambdaCase, BlockArguments, BangPatterns, DerivingVia, FunctionalDependencies,
     MultiParamTypeClasses, UndecidableInstances #-}
{-# options_ghc -Wincomplete-patterns #-}

module Polarized1 where

import Control.Monad
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Control.Monad.State.Strict
import qualified Data.IntSet as IS
import Data.Functor.Identity


type Ix     = Int
type Lvl    = Int
type LvlSet = IS.IntSet
type Id     = Int

($$) f x = f x
infixl 8 $$

(<*!>) f x = f <*> x
infixl 4 <*!>

-- TODO: dead function argument elim (replace with unit?)
--       dead code elim
--       case beta/eta
--       constant case elim
--       pair eta/beta
--       function eta
--

-- ANF
--------------------------------------------------------------------------------

data VTy = Prod VTy VTy | Sum VTy VTy deriving (Eq, Show, Ord)
data CTy = Fun VTy Ty deriving (Eq, Show, Ord)
data Ty  = VTy VTy | CTy CTy deriving (Eq, Show, Ord)

data Arg = Var Ix | Tt
  deriving Show

data Val
  = Apply Ix [Arg]
  | In1 Ix
  | In2 Ix
  | Proj1 Ix
  | Proj2 Ix
  | Pair Ix Ix
  deriving Show

data Comp
  = Lam String VTy Comp
  | Body Tm
  deriving Show

data Tm
  = LetV String VTy Val Tm       -- let x : A = t; u
  | LetC String CTy Comp Tm      -- let x : A = t; u
  | RetVal Val                   -- return t
  | RetVar Ix                    -- return x
  | Case Ix Comp Comp            -- case x (\x -> ...) (\y -> ...)
  deriving Show

--------------------------------------------------------------------------------

type Closure = (LvlSet, Id)

data VArg = VVar Lvl | VTt
  deriving (Eq, Show, Ord)

data VVal
  = VApply Lvl [VArg]
  | VIn1 Lvl
  | VIn2 Lvl
  | VProj1 Lvl
  | VProj2 Lvl
  | VPair Lvl Lvl
  deriving (Eq, Show, Ord)

data OComp
  = OLam String VTy OComp
  | OBody OTm
  deriving (Eq, Show, Ord)

data OTm
  = OLetV String VTy VVal OTm
  | OLetC String CTy Closure OTm
  | ODeadLet OTm
  | ORet Lvl
  | OCase Lvl Closure Closure
  deriving (Eq, Show, Ord)

data CComp
  = CLam String VTy CComp
  | CBody CTm
  deriving (Eq, Show, Ord)

data CTm
  = CLetV String VTy VVal CTm
  | CLetC String CTy Closure CTm
  | CRet Lvl
  | CCase Lvl Closure Closure
  deriving (Eq, Show, Ord)

--------------------------------------------------------------------------------

type Store = Map (Lvl, CComp) Id  -- key: number of free vars, binder body

store :: (Lvl, CComp) -> M Id
store t@(!l, !body) = do
  (s, ls) <- get
  case M.lookup t s of
    Nothing -> do
      let id = M.size s
      put $! (,) $$ (M.insert t id s) $$ ls
      pure id
    Just id ->
      pure id

--------------------------------------------------------------------------------

type Ren = (Lvl, Lvl, Map Lvl Lvl)

closingRen :: Lvl -> LvlSet -> Ren
closingRen cod xs =
  IS.foldl' (\(!dom,!cod,!ren) x -> (,,) $$ (dom+1) $$ cod $$ M.insert x dom ren)
            (0, cod, mempty) xs

liftRen :: Ren -> Ren
liftRen (!dom, !cod, !ren) = (,,) $$ (dom + 1) $$ (cod + 1) $$ M.insert cod dom ren

strRen :: Ren -> Ren
strRen (!d, !c, !r) = (,,) $$ d $$ (c + 1) $$ r

dom :: Ren -> Lvl
dom (!d, !_, !_) = d

class Rename a b | a -> b, b -> a where
  ren :: Ren -> a -> b

instance Rename Lvl Lvl where
  ren (!_, !_, !r) x = case M.lookup x r of
    Nothing -> error $ "appRen: " ++ show x ++ " " ++ show r
    Just y  -> y

instance Rename LvlSet LvlSet where
  ren (!_, !_, !r) xs = IS.map (r M.!) xs

instance (Rename a b) => Rename [a] [b] where
  ren r as = map (ren r) as

instance (Rename a a', Rename b b') => Rename (a, b) (a', b') where
  ren r (a, b) = (,) $$ ren r a $$ ren r b

instance Rename OTm CTm where
  ren r (OLetV x a t u)      = CLetV x a (ren r t) (ren (liftRen r) u)
  ren r (OLetC x a t u)      = CLetC x a (ren r t) (ren (liftRen r) u)
  ren r (ORet t)             = CRet (ren r t)
  ren r (OCase l left right) = CCase (ren r l) (ren r left) (ren r right)
  ren r (ODeadLet t)         = ren (strRen r) t

instance Rename VArg VArg where
  ren r (VVar l) = VVar (ren r l)
  ren r VTt      = VTt

instance Rename VVal VVal where
  ren r (VApply f ls) = VApply (ren r f) (ren r ls)
  ren r (VIn1 l)      = VIn1 (ren r l)
  ren r (VIn2 l)      = VIn2 (ren r l)
  ren r (VProj1 l)    = VProj1 (ren r l)
  ren r (VProj2 l)    = VProj2 (ren r l)
  ren r (VPair l l')  = VPair (ren r l) (ren r l')

instance Rename OComp CComp where
  ren r (OLam x a t)  = CLam x a (ren (liftRen r) t)
  ren r (OBody t)     = CBody (ren r t)

--------------------------------------------------------------------------------

type M = State (Store, LvlSet)
type Entry = Maybe (Either Closure VVal)
           --
data Cxt = Cxt {
    _fresh :: Int
  , _env   :: [(Lvl, Entry)]
  , _memo  :: Map (Either Closure VVal) Lvl
  }
  deriving Show

bind :: Cxt -> Cxt
bind (Cxt fresh env memo) = Cxt (fresh + 1) ((fresh, Nothing):env) memo

addLvl :: Lvl -> M ()
addLvl l = modify' \(s, ls) -> (,) $$ s $$ IS.insert l ls

removeLvl :: Lvl -> M ()
removeLvl l = modify' \(s, ls) -> (s,) $! IS.delete l ls

resetLvl :: M ()
resetLvl = modify' $ fmap $ const mempty

withFreeVars :: M a -> M (a, LvlSet)
withFreeVars act = do
  resetLvl
  a <- act
  fvs <- gets snd
  resetLvl
  pure (a, fvs)

class Eval a b | a -> b, b -> a where
  eval :: Cxt -> a -> M b

instance Eval Ix Lvl where
  eval cxt i = do
    let (l, _) = _env cxt !! i
    addLvl l
    pure l

instance Eval Arg VArg where
  eval cxt = \case
    Var i -> VVar <$!> eval cxt i
    Tt    -> pure VTt

instance Eval Val VVal where
  eval cxt = \case
    Apply i is -> VApply <$!> eval cxt i <*!> traverse (eval cxt) is
    In1 i      -> VIn1 <$!> eval cxt i
    In2 i      -> VIn2 <$!> eval cxt i
    Proj1 i    -> VProj1 <$!> eval cxt i
    Proj2 i    -> VProj2 <$!> eval cxt i
    Pair i i'  -> VPair <$!> eval cxt i <*!> eval cxt i'

instance Eval Comp OComp where
  eval cxt = \case
    Lam x a t -> OLam x a <$!> eval (bind cxt) t
    Body t    -> OBody <$!> eval cxt t

instance Eval Tm OTm where

  eval cxt = \case
    Ret t -> do
      t <- eval cxt t
      case M.lookup (Right t) (_memo cxt) of
        Nothing -> _
        Just id -> _


-- eval :: Lvl -> [Lvl] -> Map Val Lvl -> Tm -> State Store (OTm, LvlSet)
-- eval l env memo t = do
--   res <- case t of
--     LetLam name t u -> do
--       (!t, !tvars) <- eval (l + 1) (l:env) memo t
--       let ren = closingRen (l + 1) tvars
--       t <- pure $ rename (closingRen (l + 1) tvars) t
--       t <- store (dom ren, t)
--       let capture = IS.delete l tvars
--       let v = VLam capture t
--       case M.lookup v memo of
--         Nothing -> do
--           (u, uvars) <- eval (l + 1) (l:env) (M.insert v l memo) u
--           if IS.member l uvars then do
--             pure $! (,) $$ OLet name v u $$ (capture <> IS.delete l uvars)
--           else do
--             pure (ODeadLet u, uvars)
--         Just x' -> do
--           eval l (x':env) memo u

--     LetApp name x y t -> do
--       let x' = env !! x
--           y' = env !! y
--           v  = VApp x' y'
--       case M.lookup v memo of
--         Nothing -> do
--           (!t, !tvars) <- eval (l + 1) (l:env) (M.insert v l memo) t
--           if IS.member l tvars then do
--             pure $! (,) $$ OLet name v t $$ (IS.insert x' $ IS.insert y' $ IS.delete l tvars)
--           else do
--             pure (ODeadLet t, tvars)
--         Just x' -> do
--           eval l (x':env) memo t

--     Ret x -> do
--       let x' = env !! x
--       pure $! (,) $$ ORet x' $$ IS.singleton x'
--   pure res

-- --------------------------------------------------------------------------------

-- run t = case runState (eval 0 [] mempty (elab0 t)) mempty of
--   (!(!t, !_), !s) -> pure (s, rename (0, 0, mempty) t)

-- --------------------------------------------------------------------------------

-- lam x t = RLetLam "res" x t (RRet "res")
-- retApp x y = RLetApp "res" x y (RRet "res")


-- -- {-
-- -- Test case: Y combinator in ANF with code duplication

-- -- y =
-- --   let res = \f.
-- --     let dup1 = \x.
-- --       let y    = x x;
-- --       let res  = f y;
-- --       res;
-- --     let dup2 = \x.
-- --       let y   = x x;
-- --       let res = f y;
-- --       res;
-- --     let res = dup1 dup2;
-- --     res;
-- --   res

-- -- output:
-- --   let res = \f.
-- --     let dup = \x.
-- --       let y   = x x;
-- --       let res = f y;
-- --       res;
-- --     let res = dup dup;
-- --     res;
-- --   res
-- -- -}

-- y = lam "f" $
--       RLetLam "dup1" "x" (
--         RLetApp "y" "x" "x" $
--         retApp "f" "y"
--       )$
--       RLetLam "dup2" "x" (
--         RLetApp "y" "x" "x" $
--         RLetApp "dead" "y" "y" $
--         retApp "f" "y"
--       )$
--       retApp "dup1" "dup2"

-- deadCodeTest =
--   lam "f" $ lam "x" $
--     RLetApp "foo" "f" "x" $
--     RLetApp "bar" "x" "foo" $
--     RLetApp "res" "foo" "foo" $
--     RLetApp "kek" "res" "res" $
--     RRet "res"
