{-# language Strict, LambdaCase, BlockArguments, BangPatterns, DerivingVia #-}
{-# options_ghc -Wincomplete-patterns -Wunused-imports #-}

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Control.Monad.State.Strict
import qualified Data.IntSet as IS

-- global CSE + closure conversion for CBV untyped ANF LC in O(n*log n). The log factor comes
-- from local scope size (env lookups, free variables sets) so it's not very prominent in
-- non-pathological input.

-- Raw terms
--------------------------------------------------------------------------------

data RTm
  = RLetLam String String RTm RTm
  | RLetApp String String String RTm
  | RRet String
  deriving Show

elabVar :: Lvl -> [(String, Lvl)] -> String -> Int
elabVar l xs x = case lookup x xs of
  Nothing -> error $ "name not in scope: " ++ x
  Just x  -> l - x - 1

elab :: Lvl -> [(String, Lvl)] -> RTm -> Tm
elab l xs = \case
  RLetLam f x t u ->
    LetLam (elab (l + 1) ((x, l):xs) t) (elab (l + 1) ((f, l):xs) u)
  RLetApp x y z t ->
    LetApp (elabVar l xs y) (elabVar l xs z) (elab (l + 1) ((x, l):xs) t)
  RRet x ->
    Ret (elabVar l xs x)

elab0 = elab 0 []

--------------------------------------------------------------------------------

type Ix     = Int
type Lvl    = Int
type LvlSet = IS.IntSet
type OTmId  = Int

data Tm
  = LetLam Tm Tm     -- let f = \x. t; u
  | LetApp Ix Ix Tm  -- let x = f y; t
  | Ret Ix           -- return x
  deriving (Show)

data Val
  = VApp Lvl Lvl
  | VLam LvlSet OTmId
  deriving (Eq, Show, Ord)

data OTm
  = OLet Val OTm
  | ORet Lvl
  deriving (Show, Eq, Ord)


-- global store, memoizes closure bodies
--------------------------------------------------------------------------------

type Store = Map OTm OTmId

store :: OTm -> State Store OTmId
store t = do
  s <- get
  case M.lookup t s of
    Nothing -> do
      let id = M.size s
      put $ M.insert t id s
      pure id
    Just id ->
      pure id

-- variable renaming
--------------------------------------------------------------------------------

type Ren = (Lvl, Lvl, Map Lvl Lvl)

close :: Lvl -> LvlSet -> Ren
close cod xs =
  IS.foldl' (\(!dom,!cod,!ren) x -> (dom+1, cod, M.insert x dom ren))
            (0, cod, mempty) xs

liftRen :: Ren -> Ren
liftRen (cod, dom, ren) = (cod + 1, dom + 1, M.insert dom cod ren)

appRen :: Ren -> Lvl -> Lvl
appRen (_, _, ren) x = ren M.! x

renameLvlSet :: Ren -> LvlSet -> LvlSet
renameLvlSet (_, _, ren) xs = IS.map (ren M.!) xs

rename :: Ren -> OTm -> OTm
rename ren = \case
  OLet (VApp x y) t ->
    OLet (VApp (appRen ren x) (appRen ren y)) (rename (liftRen ren) t)
  OLet (VLam xs t) u ->
    OLet (VLam (renameLvlSet ren xs) t) (rename (liftRen ren) u)
  ORet x ->
    ORet (appRen ren x)

-- evaluation
--------------------------------------------------------------------------------

eval :: Lvl -> [Lvl] -> Map Val Lvl -> Tm -> State Store (OTm, LvlSet)
eval l env memo = \case

  LetLam t u -> do
    (t, tvars) <- eval (l + 1) (l:env) memo t
    t <- store $ rename (close (l + 1) tvars) t
    let capture = IS.delete l tvars
    let v = VLam capture t
    case M.lookup v memo of
      Nothing -> do
        (u, uvars) <- eval (l + 1) (l:env) (M.insert v l memo) u
        pure (OLet v u, capture <> IS.delete l uvars)
      Just x' -> do
        eval l (x':env) memo u

  LetApp x y t -> do
    let x' = env !! x
        y' = env !! y
        v  = VApp x' y'
    case M.lookup v memo of
      Nothing -> do
        (t, tvars) <- eval (l + 1) (l:env) (M.insert v l memo) t
        pure (OLet v t, IS.insert x' $ IS.insert y' $ IS.delete l tvars)
      Just x' -> do
        eval l (x':env) memo t

  Ret x -> do
    let x' = env !! x
    pure (ORet x', IS.singleton x')

--------------------------------------------------------------------------------

run :: RTm -> (Store, OTm)
run t = case runState (eval 0 [] mempty (elab0 t)) mempty of
  ((t, _), s) -> (s, t)

--------------------------------------------------------------------------------


{-
Test case: Y combinator in ANF with code duplication

y = \f. (\x. f (x x)) (\x. f (x x))

y =
  let res = \f.
    let dup1 = \x.
      let y   = x x;
      let res = f y;
      res;
    let dup2 = \x.
      let y   = x x;
      let res = f y;
      res;
    let res = dup1 dup2;
    res;
  res

output:
  let res = \f.
    let dup = \x.
      let y   = x x;
      let res = f y;
      res;
    let res = dup dup;
    res;
  res
-}

y = RLetLam "res" "f" (
      RLetLam "dup1" "x" (
        RLetApp "y" "x" "x" $
        RLetApp "res" "f" "y" $
        RRet "res"
      )$
      RLetLam "dup2" "x" (
        RLetApp "y" "x" "x" $
        RLetApp "res" "f" "y" $
        RRet "res"
      )$
      RLetApp "res" "dup1" "dup2" $
      RRet "res"
    )$
    RRet "res"

-- (fromList [
--      (OLet (VApp 1 1) (OLet (VApp 0 2) (ORet 3)),0)
--     ,(OLet (VLam (fromList [0]) 0) (OLet (VApp 1 1) (ORet 2)),1)]
--     ,OLet (VLam (fromList []) 1) (ORet 0))
