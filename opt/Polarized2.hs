{-# LANGUAGE RecordWildCards #-}

{-# language
     Strict, LambdaCase, BlockArguments, BangPatterns, DerivingVia, FunctionalDependencies,
     MultiParamTypeClasses, UndecidableInstances #-}
{-# options_ghc -Wincomplete-patterns #-}

module Polarized2 where

import Control.Monad
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Control.Monad.State.Strict
import qualified Data.IntSet as IS
import Data.Functor.Identity
import GHC.Stack


type Ix     = Int
type Lvl    = Int
type LvlSet = IS.IntSet
type Id     = Int

($$) f x = f x
infixl 8 $$

(<*!>) f x = f <*> x
infixl 4 <*!>

impossible :: HasCallStack => a
impossible = error "impossible"

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

data Atom = Var Ix | Tt
  deriving Show

data Val
  = Apply Ix [Atom]
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
  | Ret Atom                     -- return t
  | Case Ix Comp Comp            -- case x (\x -> ...) (\y -> ...)
  deriving Show

--------------------------------------------------------------------------------

type Closure = (LvlSet, Id)

data VAtom = VVar Lvl | VTt
  deriving (Eq, Show, Ord)

data VVal
  = VApply Lvl [VAtom]
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
  | ORet VAtom
  | OCase Lvl Closure Closure
  deriving (Eq, Show, Ord)

data CComp
  = CLam String VTy CComp
  | CBody CTm
  deriving (Eq, Show, Ord)

data CTm
  = CLetV String VTy VVal CTm
  | CLetC String CTy Closure CTm
  | CRet VAtom
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

instance Rename VAtom VAtom where
  ren r (VVar l) = VVar (ren r l)
  ren r VTt      = VTt

instance Rename OTm CTm where
  ren r (OLetV x a t u)      = CLetV x a (ren r t) (ren (liftRen r) u)
  ren r (OLetC x a t u)      = CLetC x a (ren r t) (ren (liftRen r) u)
  ren r (ORet t)             = CRet (ren r t)
  ren r (OCase l left right) = CCase (ren r l) (ren r left) (ren r right)
  ren r (ODeadLet t)         = ren (strRen r) t

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

data Cxt = Cxt {
    _fresh :: Int
  , _env   :: [Lvl]
  , _memo  :: Map (Either Closure VVal) Lvl
  , _omem  :: Map Lvl (Either Closure VVal)
  }
  deriving Show

bind :: Cxt -> Cxt
bind (Cxt fresh env memo omem) = Cxt (fresh + 1) (fresh:env) memo omem

define :: Either Closure VVal -> Cxt -> Cxt
define v (Cxt fresh env memo omem) =
  Cxt (fresh + 1) (fresh:env) (M.insert v fresh memo) (M.insert fresh v omem)

addLvl :: Lvl -> M ()
addLvl l = modify' \(s, ls) -> (,) $$ s $$ IS.insert l ls

addLvls :: LvlSet -> M ()
addLvls ls = modify' \(s, ls') -> (,) $$ s $$ (ls <> ls')

removeLvl :: Lvl -> M ()
removeLvl l = modify' \(s, ls) -> (s,) $! IS.delete l ls

localFreeVars :: M a -> M (a, LvlSet)
localFreeVars act = do
  (s, oldFvs) <- get
  put (s, mempty)
  a <- act
  (s, newFvs) <- get
  put (s, oldFvs)
  pure (a, newFvs)

--------------------------------------------------------------------------------

class Eval a b | a -> b, b -> a where
  eval :: Cxt -> a -> M b

instance Eval Ix Lvl where
  eval cxt i = do
    let l = _env cxt !! i
    addLvl l
    pure l

instance Eval Atom VAtom where
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
    Lam x a t -> OLam x a <$!> (eval (bind cxt) t <* removeLvl (_fresh cxt))
    Body t    -> OBody <$!> eval cxt t

makeClosure :: Cxt -> Comp -> M Closure
makeClosure cxt@Cxt{..} t = do
  (t, fvs) <- localFreeVars (eval cxt t)
  let r = closingRen _fresh fvs
  t <- store (dom r, ren r t)
  addLvls fvs
  pure (fvs, t)

instance Eval Tm OTm where

  eval cxt@Cxt{..} = \case
    LetV x a t u -> do
      t <- eval cxt t
      case M.lookup (Right t) _memo of
        Nothing -> do
          (u, ufvs) <- localFreeVars $ eval (define (Right t) cxt) u
          if IS.member _fresh ufvs then do
            addLvls (IS.delete _fresh ufvs)
            pure $ OLetV x a t u
          else do
            addLvls ufvs
            pure $ ODeadLet u
        Just id -> do
          let cxt' = Cxt _fresh (id:_env) _memo _omem
          eval cxt' u

    LetC x a t u -> do
      t <- makeClosure cxt t
      case M.lookup (Left t) _memo of
        Nothing -> do
          (u, ufvs) <- localFreeVars $ eval (define (Left t) cxt) u
          if IS.member _fresh ufvs then do
            addLvls (IS.delete _fresh ufvs)
            pure $ OLetC x a t u
          else do
            addLvls ufvs
            pure $ ODeadLet u
        Just id -> do
          let cxt' = Cxt _fresh (id:_env) _memo _omem
          eval cxt' u

    Ret x ->
      ORet <$!> eval cxt x

    Case i leftcomp@(Lam x a (Body left)) rightcomp@(Lam y b (Body right)) -> do
      let l = _env !! i
      case M.lookup l _omem of
        Just (Right (VIn1 l')) -> do
          eval (Cxt _fresh (l':_env) _memo _omem) left
        Just (Right (VIn2 l')) -> do
          eval (Cxt _fresh (l':_env) _memo _omem) right
        Just (Right v) -> do
          _
        Just Left{} ->
          impossible
        Nothing -> do
          left <- eval (Cxt _fresh left
          _


{-
1
in this case, I want to redefine boundvar to be Left x / Right y in the bodies
which isn't difficult
case boundvar of
  Left x -> _
  Right y -> _

2
In this case I want to identify "f args" and "Left/Right y" in the bodies.

let x = <neutral app/proj>
...
case x of
  Left y -> ...
  Right y -> ...

How to do that?
Add a memo mapping from <neutral app/proj> to


let x = proj2 y
case x of
  Left z -> let bar = proj1 y; (bar, x)

I need *both* that x = proj2 y and x = Left z for different opts.
First, for prod-eta, second for case-beta.

What if I just don't want to do prod-eta, because officially we
have no prod-eta for value prods?

If I only have prod-eta for comp prods, then I never get
such a "clash".

But I want value prod-eta! Who cares about nontermination.

Every variable can only simulataneously be TWO things:
  - a neutral
  - a canonical (bc of pattern match)

It's not possible for a var to be simultaneously equal to two different
neutrals. It would be only possible if we have a generic "equality" testing
primitive in the core.

Hence, we only need to remember TWO values at most for each variable
In the memo direction, we just map both values back to the same var.








-}


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
