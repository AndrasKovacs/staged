
{-# language Strict, LambdaCase, BlockArguments, ViewPatterns, ImplicitParams #-}
{-# options_ghc -Wincomplete-patterns #-}

-- ANF conversion with join points, tail calls, labeled switch branches,
-- + closure conversion

import Control.Monad hiding (join)
import Control.Monad.State.Strict
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.IntSet as IS

type Lvl = Int

data Tm
  = Var Lvl
  | App Tm Tm
  | Lam Tm
  | Let Tm Tm
  | Inl Tm
  | Inr Tm
  | Case Tm Tm Tm
  deriving Show

type FunAddr = Int         -- "address" of top-level functions
type LvlSet  = IS.IntSet

data ANF
  = ARet Lvl                -- return
  | AJoin ANF ANF           -- declare join point
  | AJump Lvl Lvl           -- tailcall join point
  | ALam LvlSet FunAddr ANF -- allocate closure
  | ACall Lvl Lvl ANF       -- call a closure
  | ATailCall Lvl Lvl       -- tail call a closure
  | AInl Lvl ANF            -- allocate Inl
  | AInr Lvl ANF            -- allocate Inr
  | ASwitch Lvl Lvl Lvl     -- switch and jump to joint point
  deriving Show

------------------------------------------------------------

($$) f x = f x
infixl 8 $$

type Fresh  = (?fresh :: Lvl)
type Env    = (?env   :: [Lvl])
type Free   = (?free  :: LvlSet)
type Args a = Fresh => Env => a

def :: Lvl -> Args a -> Args a
def x act = let ?env = x : ?env in act

addVar :: Lvl -> M ()
addVar x = modify (fmap (IS.insert x))

deleteVar :: Lvl -> M ()
deleteVar x = modify (fmap (IS.delete x))

bind :: Args (Lvl -> M a) -> Args (M a)
bind act = do
  let x      = ?fresh
  let ?fresh = x + 1
  let ?env   = x : ?env
  a <- act x
  deleteVar x
  pure a

ret :: Lvl -> ANF
ret = ARet

-- join :: Args (Lvl -> ANF) -> Args (Lvl -> ANF) -> Args ANF
-- join t k = AJoin (bind t) (bind k)

-- jump :: Lvl -> Lvl -> ANF
-- jump = AJump

-- lam :: Args (Lvl -> ANF) -> Args (Lvl -> ANF) -> Args ANF
-- lam t k = ALam (bind t) (bind k)

-- inl :: Lvl -> Args (Lvl -> ANF) -> Args ANF
-- inl x k = AInl x (bind k)

-- inr :: Lvl -> Args (Lvl -> ANF) -> Args ANF
-- inr x k = AInr x (bind k)

call :: Lvl -> Lvl -> Args (Lvl -> M ANF) -> Args (M ANF)
call x y k = ACall x y <$> bind k

-- tailcall :: Lvl -> Lvl -> ANF
-- tailcall = ATailCall

-- switch :: Lvl -> Lvl -> Lvl -> ANF
-- switch = ASwitch

--------------------------------------------------------------------------------

type Ren = (Lvl, Lvl, Map Lvl Lvl)

-- closingRen :: Lvl -> LvlSet -> Ren
-- closingRen cod xs =
--   IS.foldl' (\(!dom,!cod,!ren) x -> (,,) $$ (dom+1) $$ cod $$ M.insert x dom ren)
--             (0, cod, mempty) xs

-- liftRen :: Ren -> Ren
-- liftRen (dom, cod, ren) = (,,) $$ (dom + 1) $$ (cod + 1) $$ M.insert cod dom ren

-- appRen :: Ren -> Lvl -> Lvl
-- appRen (_, _, ren) x =
--   case M.lookup x ren of
--     Nothing -> error $ "appRen: " ++ show x ++ " " ++ show ren
--     Just y  -> y

-- dom :: Ren -> Lvl
-- dom (d, _, _) = d

-- strRen :: Ren -> Ren
-- strRen (d, c, r) = (d, c + 1, r)

-- renameLvlSet :: Ren -> LvlSet -> LvlSet
-- renameLvlSet (_, _, ren) xs = IS.map (ren M.!) xs

------------------------------------------------------------

type M = State (Map ANF FunAddr, LvlSet)

applyEnv :: Env => Lvl -> Lvl
applyEnv x = ?env !! (length ?env - x - 1)

withFreeVars :: M a -> M (a, LvlSet)
withFreeVars act = do
  (st, fvs) <- get
  put (st, mempty)
  a <- act
  (st', fvs') <- get
  put (st', fvs <> fvs')
  pure (a, fvs')

anf :: Fresh => Env => Tm -> (Fresh => Lvl -> M ANF) -> M ANF
anf t k = case t of
  Var x   -> let y = applyEnv x in addVar y >> k y
  App t u -> anf u \u -> anf t \t -> call t u k

  Lam t -> do
    (t, fvs) <- withFreeVars (bind \_ -> anfTail t)

    _


anfTail :: Fresh => Env => Tm -> M ANF
anfTail = undefined

  -- Lam t -> do



-- anf :: Fresh => Env => Tm -> (Fresh => Lvl -> State Store ANF) -> State Store ANF
-- anf t k = case t of
--   Var x      -> k $ applyEnv x
--   App t u    -> anf u \u -> anf t \t -> call t u k
--   Lam t      -> lam (\_ -> anfTail t) k
--   Let t u    -> anf t \t -> def t $ anf u k
--   Inl t      -> anf t \t -> inl t k
--   Inr t      -> anf t \t -> inr t k
--   Case t l r -> anf t \t -> join k \k ->
--                             join (\_ -> anf l (jump k)) \l ->
--                             join (\_ -> anf r (jump k)) \r ->
--                             switch t l r

-- anfTail :: Fresh => Env => Tm -> ANF
-- anfTail = \case
--   Var x      -> ret $ applyEnv x
--   App t u    -> anf u \u -> anf t \t -> tailcall t u
--   Lam t      -> lam (\_ -> anfTail t) ret
--   Let t u    -> anf t \t -> def t $ anfTail u
--   Inl t      -> anf t \t -> inl t ret
--   Inr t      -> anf t \t -> inr t ret
--   Case t l r -> anf t \t -> join (\_ -> anfTail l) \l ->
--                             join (\_ -> anfTail r) \r ->
--                             switch t l r

-- anf0 :: Tm -> ANF
-- anf0 t = let ?fresh = 0; ?env = [] in anfTail t

-- ------------------------------------------------------------

-- t = Lam $ App (Var 0) (Case (Var 0) (Inl (Var 1)) (Inr (Var 1)))
