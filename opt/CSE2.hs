
{-# language Strict, LambdaCase, BlockArguments, BangPatterns, DerivingVia #-}
{-# options_ghc -Wincomplete-patterns #-}

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Control.Monad.State.Strict
import qualified Data.IntSet as IS

import Debug.Trace

-- global CSE + closure conversion + dead code elimination for pure LC in ANF,
-- in log-linear time.

-- dead value elimination is "fast and loose", it can convert infinite loops
-- to non-loops
-- pedantic optimization would only delete dead total code

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
    LetLam f (elab (l + 1) ((x, l):xs) t) (elab (l + 1) ((f, l):xs) u)
  RLetApp x y z t ->
    LetApp x (elabVar l xs y) (elabVar l xs z) (elab (l + 1) ((x, l):xs) t)
  RRet x ->
    Ret (elabVar l xs x)

elab0 = elab 0 []

($$) f x = f x
infixl 8 $$

--------------------------------------------------------------------------------

type Ix     = Int
type Lvl    = Int
type LvlSet = IS.IntSet
type CTmId  = Int

data Tm
  = LetLam String Tm Tm     -- let f = \x. t; u
  | LetApp String Ix Ix Tm  -- let x = f y; t
  | Ret Ix                  -- return x
  deriving (Show)

data Val
  = VApp Lvl Lvl
  | VLam LvlSet CTmId
  deriving (Eq, Show, Ord)

-- "open term"
data OTm
  = OLet String Val OTm
  | ORet Lvl
  | ODeadLet OTm        -- explicit strengthening left after dead let-s
  deriving (Show, Eq, Ord)

-- "closed term"
data CTm
  = CLet String Val CTm
  | CRet Lvl
  deriving (Show, Eq, Ord)

-- global store, memoizes lambda bodies
--------------------------------------------------------------------------------

type Store = Map (Lvl, CTm) CTmId  -- key: number of captured vars, lambda body

store :: (Lvl, CTm) -> State Store CTmId
store t@(!l, !body) = do
  s <- get
  case M.lookup t s of
    Nothing -> do
      let id = M.size s
      put $ M.insert t id s
      pure id
    Just id ->
      pure id

-- closing terms
--------------------------------------------------------------------------------

type Ren = (Lvl, Lvl, Map Lvl Lvl)

closingRen :: Lvl -> LvlSet -> Ren
closingRen cod xs =
  IS.foldl' (\(!dom,!cod,!ren) x -> (,,) $$ (dom+1) $$ cod $$ M.insert x dom ren)
            (0, cod, mempty) xs

liftRen :: Ren -> Ren
liftRen (dom, cod, ren) = (,,) $$ (dom + 1) $$ (cod + 1) $$ M.insert cod dom ren

appRen :: Ren -> Lvl -> Lvl
appRen (_, _, ren) x =
  case M.lookup x ren of
    Nothing -> error $ "appRen: " ++ show x ++ " " ++ show ren
    Just y  -> y

dom :: Ren -> Lvl
dom (d, _, _) = d

strRen :: Ren -> Ren
strRen (d, c, r) = (d, c + 1, r)

renameLvlSet :: Ren -> LvlSet -> LvlSet
renameLvlSet (_, _, ren) xs = IS.map (ren M.!) xs

rename :: Ren -> OTm -> CTm
rename ren = \case
  OLet name (VApp x y) t ->
    CLet name (VApp (appRen ren x) (appRen ren y)) (rename (liftRen ren) t)
  OLet name (VLam xs t) u ->
    CLet name (VLam (renameLvlSet ren xs) t) (rename (liftRen ren) u)
  ORet x ->
    CRet (appRen ren x)
  ODeadLet t ->
    rename (strRen ren) t

-- etaReduce :: CTm -> Maybe Lvl
-- etaReduce = \case

-- \x y. f x y

-- \x.
--    let res = \y.
--      let a = f x
--      let res = a y
--      res
--    res

-- \x. let res = f x; res

-- problem: multi-arity eta has wacky shape
--          we could try to detect it generally, but
--          we really should only eta-contract to function variables

-- with polarized ANF, it's much nicer, because we can handle
-- N-ary lam/app all the time, and everything's saturated.


-- evaluation
--------------------------------------------------------------------------------

eval :: Lvl -> [Lvl] -> Map Val Lvl -> Tm -> State Store (OTm, LvlSet)
eval l env memo t = do
  res <- case t of
    LetLam name t u -> do
      (!t, !tvars) <- eval (l + 1) (l:env) memo t
      let ren = closingRen (l + 1) tvars
      t <- pure $ rename (closingRen (l + 1) tvars) t
      t <- store (dom ren, t)
      let capture = IS.delete l tvars
      let v = VLam capture t
      case M.lookup v memo of
        Nothing -> do
          (u, uvars) <- eval (l + 1) (l:env) (M.insert v l memo) u
          if IS.member l uvars then do
            pure $! (,) $$ OLet name v u $$ (capture <> IS.delete l uvars)
          else do
            pure (ODeadLet u, uvars)
        Just x' -> do
          eval l (x':env) memo u

    LetApp name x y t -> do
      let x' = env !! x
          y' = env !! y
          v  = VApp x' y'
      case M.lookup v memo of
        Nothing -> do
          (!t, !tvars) <- eval (l + 1) (l:env) (M.insert v l memo) t
          if IS.member l tvars then do
            pure $! (,) $$ OLet name v t $$ (IS.insert x' $ IS.insert y' $ IS.delete l tvars)
          else do
            pure (ODeadLet t, tvars)
        Just x' -> do
          eval l (x':env) memo t

    Ret x -> do
      let x' = env !! x
      pure $! (,) $$ ORet x' $$ IS.singleton x'
  pure res

--------------------------------------------------------------------------------

run :: RTm -> (Store, CTm)
run t = case runState (eval 0 [] mempty (elab0 t)) mempty of
  (!(!t, !_), !s) -> (s, rename (0, 0, mempty) t)

--------------------------------------------------------------------------------

lam x t = RLetLam "res" x t (RRet "res")
retApp x y = RLetApp "res" x y (RRet "res")


-- {-
-- Test case: Y combinator in ANF with code duplication

-- y =
--   let res = \f.
--     let dup1 = \x.
--       let y    = x x;
--       let res  = f y;
--       res;
--     let dup2 = \x.
--       let y   = x x;
--       let res = f y;
--       res;
--     let res = dup1 dup2;
--     res;
--   res

-- output:
--   let res = \f.
--     let dup = \x.
--       let y   = x x;
--       let res = f y;
--       res;
--     let res = dup dup;
--     res;
--   res
-- -}

y = lam "f" $
      RLetLam "dup1" "x" (
        RLetApp "y" "x" "x" $
        retApp "f" "y"
      )$
      RLetLam "dup2" "x" (
        RLetApp "y" "x" "x" $
        RLetApp "dead" "y" "y" $
        retApp "f" "y"
      )$
      retApp "dup1" "dup2"

deadCodeTest =
  lam "f" $ lam "x" $
    RLetApp "foo" "f" "x" $
    RLetApp "bar" "x" "foo" $
    RLetApp "res" "foo" "foo" $
    RLetApp "kek" "res" "res" $
    RRet "res"
