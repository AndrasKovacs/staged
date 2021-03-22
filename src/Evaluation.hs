{-# options_ghc -Wno-unused-imports #-}

module Evaluation where

import IO

import Common
import qualified Syntax as S
import qualified ElabState as ES
import Values

-- import Control.Monad

--------------------------------------------------------------------------------

var :: Env -> Ix -> Val
var (Snoc _ v)   0 = v
var (Snoc env _) x = var env (x - 1)
var _ _            = impossible

top :: Lvl -> Val
top x = runIO $ ES.readTop x >>= \case
  ES.TEDef v _ _ _ _ _ -> pure $ Unfold (UHTop x) SNil v
  _                    -> impossible
{-# inline top #-}

eval1 :: Env -> S.Tm -> Val
eval1 env t = let
  go = eval1 env; {-# inline go #-}
  in case t of
    S.Var x -> var env x
    S.Top x -> top x
    S.Let x a un t u -> _

-- down :: Val1 -> Val0
-- down = undefined

-- eval0 :: Env -> S.Tm -> Val0
-- eval0 env t = let
--   go = eval0 env; {-# inline go #-}
--   in case t of
--     S.LocalVar x -> down (localVar env x)
--     S.TopDef x   -> TopDef x




-- --------------------------------------------------------------------------------

-- -- Evaluate an object term
-- eval0 :: Env -> S.Tm -> Val
-- eval0 env t = let
--   go = eval0 env; {-# inline go #-}
--   in case t of
--     S.LocalVar x -> localVar env x
--     S.TopDef x   ->


-- --------------------------------------------------------------------------------


-- -- Evaluate a term in MTy
-- eval :: Env -> S.Tm -> Val
-- eval env t = let
--   go = eval env; {-# inline go #-}
--   in case t of
--     S.LocalVar x    -> localVar env x
--     S.TopDef x      -> topDef x
--     S.Pi x i a b    -> Pi x i (go a) (Close env b)
--     S.Lam x i a t   -> Lam x i (go a) (Close env t)
--     S.App t u i     -> inlineApp (go t) (go u) i
--     S.Ty un         -> Ty un
--     S.Lift t        -> Lift (go t)
--     S.Up t          -> _
--     -- S.Down t        -> inlineDown (go t)
--     -- S.Rec fields    -> Rec (map (\(x, t) -> (x, go t)) fields)
--     -- S.RecCon fields -> RecCon (map (\(x, t) -> (x, go t)) fields)
--     -- S.Field t x n   -> inlineField (go t) x n
--     -- S.Fix x y t     -> Rigid (RHFix x y (Close env t)) SNil
--     -- S.Case t ts     -> inlineEvalCase env (go t) ts
--     -- S.DataCon x i   -> Rigid (RHDataCon x i) SNil
--     -- S.TyCon x       -> Rigid (RHTyCon x) SNil

-- -- Variables
-- --------------------------------------------------------------------------------



-- meta :: MetaVar -> Val
-- meta x = runIO $ readMeta x >>= \case
--   MEUnsolved{}    -> pure $ Flex x SNil
--   MESolved v  _ _ -> pure $ Unfold (UHMeta x) SNil v
-- {-# inline meta #-}




-- -- Functions
-- --------------------------------------------------------------------------------

-- infixl 2 $$
--   -- | Strict closure application.
-- ($$) :: Closure -> Val -> Val
-- ($$) (Close env t) u = eval (Snoc env u) t
-- {-# inline ($$) #-}

-- infixl 2 $$$
--   -- | Lazy closure application.
-- ($$$) :: Closure -> Val -> Val
-- ($$$) (Close env t) ~u = eval (Snoc env u) t
-- {-# inline ($$$) #-}

-- app :: Val -> Val -> Icit -> Val
-- app t u i = case t of
--   Lam x i a t     -> t $$ u
--   Rigid h sp      -> Rigid h (SApp sp u i)
--   Flex h sp       -> Flex h (SApp sp u i)
--   Unfold h sp t   -> Unfold h (SApp sp u i) (app t u i)
--   _               -> impossible

-- inlineApp :: Val -> Val -> Icit -> Val
-- inlineApp t u i = case t of
--   Lam x i a t   -> t $$ u
--   Rigid h sp    -> Rigid h (SApp sp u i)
--   Flex h sp     -> Flex h (SApp sp u i)
--   Unfold h sp t -> Unfold h (SApp sp u i) (app t u i)
--   _             -> impossible
-- {-# inline inlineApp #-}

-- -- Field projection
-- --------------------------------------------------------------------------------

-- -- field :: Val -> Name -> Int -> Val
-- -- field t x n = case t of
-- --   RecCon fields -> snd (fields !! n)
-- --   Rigid h sp    -> Rigid h (SField sp x n)
-- --   Flex h sp     -> Flex h (SField sp x n)
-- --   Unfold h sp t -> Unfold h (SField sp x n) (field t x n)
-- --   _             -> impossible

-- -- inlineField :: Val -> Name -> Int -> Val
-- -- inlineField t x n = case t of
-- --   RecCon fields -> snd (fields !! n)
-- --   Rigid h sp    -> Rigid h (SField sp x n)
-- --   Flex h sp     -> Flex h (SField sp x n)
-- --   Unfold h sp t -> Unfold h (SField sp x n) (field t x n)
-- --   _             -> impossible
-- -- {-# inline inlineField #-}

-- -- -- Lifting
-- -- --------------------------------------------------------------------------------

-- -- down :: Val -> Val
-- -- down = \case
-- --   Up t          -> t
-- --   Rigid h sp    -> Rigid h (SDown sp)
-- --   Flex h sp     -> Flex h (SDown sp)
-- --   Unfold h sp t -> Unfold h (SDown sp) (down t)
-- --   _             -> impossible

-- -- inlineDown :: Val -> Val
-- -- inlineDown = \case
-- --   Up t          -> t
-- --   Rigid h sp    -> Rigid h (SDown sp)
-- --   Flex h sp     -> Flex h (SDown sp)
-- --   Unfold h sp t -> Unfold h (SDown sp) (down t)
-- --   _             -> impossible
-- -- {-# inline inlineDown #-}

-- -- up :: Val -> Val
-- -- up = \case
-- --   Rigid h (SDown sp)    -> Rigid h sp
-- --   Flex h (SDown sp)     -> Flex h sp
-- --   Unfold h (SDown sp) t -> Unfold h sp (up t)
-- --   t                     -> Up t

-- -- inlineUp :: Val -> Val
-- -- inlineUp = \case
-- --   Rigid h (SDown sp)    -> Rigid h sp
-- --   Flex h (SDown sp)     -> Flex h sp
-- --   Unfold h (SDown sp) t -> Unfold h sp (up t)
-- --   t                     -> Up t
-- -- {-# inline inlineUp #-}


-- -- -- forcing
-- -- --------------------------------------------------------------------------------

-- -- -- forceFlexHead :: MetaVar -> Spine -> Val
-- -- -- forceFlexHead x sp = runIO $ readMeta x >>= \case
-- -- --   MESolved v _ _ -> pure $! Unfold (UHMeta x) sp (forceF (spine v sp))
-- -- --   _              -> pure $! Flex x sp

-- -- -- -- | Force only flex.
-- -- -- forceF :: Val -> Val
-- -- -- forceF = \case
-- -- --   Flex x sp -> forceFlexHead x sp
-- -- --   t         -> t
-- -- -- {-# inline forceF #-}

-- -- -- -- | Force flex and unfolding
-- -- -- forceFU :: Val -> Val
-- -- -- forceFU = \case
-- -- --   Flex x sp -> forceFlex


-- -- -- spine application
-- -- --------------------------------------------------------------------------------

-- -- spine :: Val -> Spine -> Val
-- -- spine ~v = \case
-- --   SNil            -> v
-- --   SApp sp t i     -> inlineApp (spine v sp) t i
-- --   SField sp x n   -> inlineField (spine v sp) x n
-- --   SCase sp env ts -> inlineEvalCase env (spine v sp) ts
-- --   SDown sp        -> inlineDown (spine v sp)

-- -- -- cases
-- -- --------------------------------------------------------------------------------

-- -- appendSpine :: Env -> Spine -> Env
-- -- appendSpine env = \case
-- --   SNil        -> env
-- --   SApp sp t _ -> Snoc (appendSpine env sp) t
-- --   _           -> impossible

-- -- evalCase :: Env -> Val -> [(Lvl, [Name], S.Tm)] -> Val
-- -- evalCase env v ts = case v of
-- --   Rigid (RHDataCon _ i) sp -> case ts !! i of
-- --     (_, _, rhs) -> eval (appendSpine env sp) rhs
-- --   Rigid h sp    -> Rigid h (SCase sp env ts)
-- --   Flex h sp     -> Flex h (SCase sp env ts)
-- --   Unfold h sp t -> Unfold h (SCase sp env ts) (evalCase env t ts)
-- --   _             -> impossible

-- -- inlineEvalCase :: Env -> Val -> [(Lvl, [Name], S.Tm)] -> Val
-- -- inlineEvalCase env v ts = case v of
-- --   Rigid (RHDataCon _ i) sp -> case ts !! coerce i of
-- --     (_, _, rhs) -> eval (appendSpine env sp) rhs
-- --   Rigid h sp    -> Rigid h (SCase sp env ts)
-- --   Flex h sp     -> Flex h (SCase sp env ts)
-- --   Unfold h sp t -> Unfold h (SCase sp env ts) (evalCase env t ts)
-- --   _             -> impossible
-- -- {-# inline inlineEvalCase #-}

-- -- -- eval
-- -- --------------------------------------------------------------------------------

-- -- eval :: Env -> S.Tm -> Val
-- -- eval env t = let
-- --   go = eval env; {-# inline go #-}
-- --   in case t of
-- --     S.LocalVar x    -> localVar env x
-- --     S.TopDef x      -> topDef x
-- --     S.Pi x i a b    -> Pi x i (go a) (Close env b)
-- --     S.Fun a b       -> Fun (go a) (go b)
-- --     S.Lam x i a t   -> Lam x i (go a) (Close env t)
-- --     S.App t u i     -> inlineApp (go t) (go u) i
-- --     S.Ty un         -> Ty un
-- --     S.Lift t        -> Lift (go t)
-- --     S.Up t          -> inlineUp (go t)
-- --     S.Down t        -> inlineDown (go t)
-- --     S.Rec fields    -> Rec (map (\(x, t) -> (x, go t)) fields)
-- --     S.RecCon fields -> RecCon (map (\(x, t) -> (x, go t)) fields)
-- --     S.Field t x n   -> inlineField (go t) x n
-- --     S.Fix x y t     -> Rigid (RHFix x y (Close env t)) SNil
-- --     S.Case t ts     -> inlineEvalCase env (go t) ts
-- --     S.DataCon x i   -> Rigid (RHDataCon x i) SNil
-- --     S.TyCon x       -> Rigid (RHTyCon x) SNil
