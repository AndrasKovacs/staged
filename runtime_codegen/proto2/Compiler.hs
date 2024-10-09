{-# language OverloadedStrings #-}
{-# options_ghc -Wno-unused-imports #-}

module Compiler where

{-
1. Closure conversion
2. Codegen
-}

import Common hiding (Lvl)

import qualified Zonk   as Z
import qualified Common as C
import Pretty
import ElabState
import Errors

import Control.Monad.State.Strict
import Data.IORef
import Data.String
import Data.Void
import System.IO.Unsafe
import qualified Data.IntSet as IS

--------------------------------------------------------------------------------

data Build = Chunk String | Append Build Build | Newline Int | Empty
  deriving Show

instance IsString Build  where fromString = Chunk
instance Semigroup Build where (<>) = Append
instance Monoid Build    where mempty = Empty

newtype Out = Out (Int -> Build)
  deriving (Semigroup, Monoid) via (Int -> Build)

str :: String -> Out
str = fromString

instance IsString Out where fromString s = Out (\_ -> Chunk s)

indent :: Out -> Out
indent (Out f) = Out (\i -> f $! i + 4)

newl :: Out
newl = Out Newline

out :: Out -> String
out (Out f) = go (f 0) "" where
  go :: Build -> String -> String
  go b acc = case b of
    Chunk s      -> s ++ acc
    Append b1 b2 -> go b1 (go b2 acc)
    Newline i    -> let acc' = indent i acc in '\n':acc'
    Empty        -> acc

  indent :: Int -> String -> String
  indent 0 acc = acc
  indent i acc = indent (i - 1) (' ':acc)


-- Closure conversion
--------------------------------------------------------------------------------

type ZTm = Z.Tm Void

data Tm =
    Var Name
  | Let Name Tm Tm
  | Lam Name [Name] -- code, application to env vars
  | App Tm Tm
  | Erased String
  | Quote Tm
  | Splice Tm (Maybe SourcePos)
  | Return Tm
  | Bind Name Tm Tm
  | Seq Tm Tm
  | New Tm
  | Write Tm Tm
  | Read Tm
  deriving Show

data S = S {freeVars :: IS.IntSet, topLen :: Int, top :: [(Name, [Name], Name, Tm)]}
type Env = (?env :: [Name])

fresh :: Name -> (Env => Name -> a) -> Env => a
fresh x act | elem x ?env = let x' = x ++ show (length ?env) in
                            let ?env = x' : ?env in
                            act x'
            | otherwise   = let ?env = x : ?env in
                            act x

cconv :: Env => ZTm -> State S Tm
cconv = \case
  Z.Var x ->
    pure $! Var (?env !! coerce x)

  Z.Let x t u -> do
    t <- cconv t
    fresh x \x -> Let x t <$> cconv u

  -- Z.Lam x t -> fresh x \x -> do
  --   _




--------------------------------------------------------------------------------

-- type Tm      = Z.Tm Void
-- type Lvl     = (?lvl   :: C.Lvl)
-- type Stage   = (?stage :: Int)

-- lvl :: C.Lvl -> (Lvl => a) -> a
-- lvl l act = let ?lvl = l in act

-- bind :: (Lvl => a) -> (Lvl => a)
-- bind act = let ?lvl = ?lvl + 1 in act

-- stage :: Int -> (Stage => a) -> a
-- stage s act = let ?stage = s in act

-- ix :: Lvl => Ix -> Out
-- ix x = str ('x' : show (coerce ?lvl - x - 1))

-- top :: Lvl => Out
-- top = str ('x' : show ?lvl)

-- eRun :: Out -> Out
-- eRun t = t <> "()"

-- cApp :: Out -> Out -> Out
-- cApp t u = t <> "(" <> u <> ")"

-- exec :: Lvl => Tm -> Out
-- exec = \case
--  Var x      -> eRun $ ix x
--  Let _ t u  -> "const " <> top <> " = " <> ceval t <> ";" <> newl <> bind (exec u)
--  Lam{}      -> impossible
--  App t u    -> eRun (cApp (ceval t) (ceval u))
--  Erased _   -> impossible
--  Quote{}    -> impossible
--  Splice t _ -> eRun $ "_ceval(" <> ceval t <> ")"
--  Return t   -> "return " <> ceval t <> ";"
--  Bind _ t u -> "const " <> top <> " = " <> exec t <> ";" <> newl <> bind (exec u)
--  Seq t u    -> exec t <> ";" <> newl <> exec u
--  New t      -> "{_1 : " <> ceval t <> "}"
--  Write t u  -> ceval t <> "._1 = " <> ceval u
--  Read t     -> ceval t <> "._1"

-- lam :: Lvl => Out -> Out
-- lam t = "(" <> top <> ") => {" <> t <> "}"

-- ceval :: Lvl => Tm -> Out
-- ceval = \case
--   Var x     -> ix x
--   Let _ t u -> "const " <> top <> " = " <> ceval t <> ";" <> newl <> bind (ceval u)

--   -- TODO: get rid of code duplication!!
--   Lam _ t   -> "{_1: " <> lam (bind (ceval t)) <> ", _2: " <> lam (bind $ stage 0 $ oeval t) <> "}"

--   App t u   -> cApp (ceval t) (ceval u)
--   Erased{}  -> "undefined"
--   Quote t   -> stage 1 $ oeval t  -- TODO: close environment!!!

-- oeval :: Lvl => Stage => Tm -> Out
-- oeval = \case


-- -- oeval :: OEnv => Lvl => Stage => Tm -> Open
-- -- oeval = \case
-- --   Var x      -> snd (?env !! coerce x)
-- --   Lam x t    -> OLam x (coerce \l v -> def x v $ lvl l $ oeval t)
-- --   Erased s   -> OErased s
-- --   Quote t    -> OQuote $ stage (?stage + 1) $ oeval t
-- --   Return t   -> OReturn (oeval t)
-- --   Bind x t u -> OBind x (oeval t) (NoShow \l -> lvl l $ oeval u)
-- --   Seq t u    -> OSeq (oeval t) (oeval u)
-- --   New t      -> ONew (oeval t)
-- --   Write t u  -> OWrite (oeval t) (oeval u)
-- --   Read t     -> ORead (oeval t)
-- --   CSP x t    -> OClosed x t
-- --   t -> case ?stage of
-- --     0 -> case t of
-- --       Let x t u    -> def x (oeval t) (oeval u)
-- --       App t u      -> case (oeval t, oeval u) of
-- --                         (OClosed x (CLam _ f _), OClosed x' u) -> OClosed Nothing (coerce f u)
-- --                         (OClosed x (CLam _ _ f), u           ) -> coerce f ?lvl u
-- --                         (OLam _ f              , u           ) -> coerce f ?lvl u
-- --                         (t                     , u           ) -> OApp t u
-- --       Splice t pos -> case oeval t of
-- --                         OQuote t -> env (idEnv ?lvl) $ oeval $ traceGen t pos
-- --                         t        -> OSplice t
-- --     _ -> case t of
-- --       Let x t u    -> OLet x (oeval t) (NoShow \l -> lvl l $ oeval u)
-- --       App t u      -> OApp (oeval t) (oeval u)
-- --       Splice t pos -> case stage (?stage - 1) $ oeval t of
-- --                         OQuote t -> t
-- --                         t        -> OSplice t








-- -- exec :: CEnv => Tm -> IO Closed
-- -- exec = \case
-- --   Var x        -> eRun (snd (?env !! coerce x))
-- --   Let x t u    -> def x (ceval t) (exec u)
-- --   Lam x t      -> impossible
-- --   App t u      -> eRun $ cApp (ceval t) (ceval u)
-- --   Erased{}     -> impossible
-- --   Quote t      -> impossible
-- --   Splice t pos -> eRun $ cSplice (ceval t) pos
-- --   Return t     -> pure $! ceval t
-- --   Bind x t u   -> do {t <- exec t; def x t $ exec u}
-- --   Seq t u      -> exec t >> exec u
-- --   New t        -> CRef <$!> (coerce (newIORef $! ceval (coerce t)))
-- --   Write t u    -> case ceval t of
-- --                     CRef r -> CErased "tt" <$ (writeIORef (coerce r) $! ceval u)
-- --                     _      -> impossible
-- --   Read t       -> case ceval t of
-- --                     CRef r -> readIORef (coerce r)
-- --                     _      -> impossible
-- --   CSP x t      -> eRun t











-- -- --------------------------------------------------------------------------------

-- -- type Tm    = Z.Tm Void
-- -- type Env   = (?env   :: [(Name, JS)])
-- -- type Lvl   = (?lvl   :: C.Lvl)
-- -- type Stage = (?stage :: Int)

-- -- data JS
-- --   = Var Name
-- --   | Lam Name (JS -> JS)
-- --   | App JS [JS]
-- --   | Obj [(Name, JS)]
-- --   | Field JS Name
-- --   | Arr [JS]
-- --   | Index JS JS
-- --   | Str String
-- --   | ConsoleLog [JS]
-- --   | Let Name JS (JS -> JS)
-- --   | Undefined

-- --   -- -- Open code is kinda an ADT in js
-- --   -- | OVar C.Lvl
-- --   -- | OLet Name JS (JS -> JS)
-- --   -- | OLam Name (NoShow (C.Lvl -> Open -> Open))
-- --   -- | OApp Open Open
-- --   -- | OErased String
-- --   -- | OQuote Open
-- --   -- | OSplice Open
-- --   -- | OReturn Open
-- --   -- | OBind Name Open (NoShow (C.Lvl -> Open))
-- --   -- | OSeq Open Open
-- --   -- | ONew Open
-- --   -- | OWrite Open Open
-- --   -- | ORead Open
-- --   -- | OClosed (Maybe Name) Closed
-- --   -- deriving Show

-- -- infixl 8 $$
-- -- ($$) :: JS -> [JS] -> JS
-- -- ($$) = App

-- -- stage :: Int -> (Stage => a) -> a
-- -- stage s act = let ?stage = s in act

-- -- lvl :: C.Lvl -> (Lvl => a) -> a
-- -- lvl l act = let ?lvl = l in act

-- -- --------------------------------------------------------------------------------

-- -- def :: Name -> JS -> (Env => a) -> (Env => a)
-- -- def x t act = let ?env = (x, t): ?env in act

-- -- eRun :: JS -> JS
-- -- eRun v = v $$ []

-- -- -- cSplice :: JS -> Maybe SourcePos -> JS
-- -- -- cSplice t pos = Case

-- --   -- CQuote t -> env [] $ lvl 0 $ ceval $ traceGen t pos
-- --   -- _        -> impossible

-- -- exec :: Env => Tm -> JS
-- -- exec = \case
-- --   Z.Var x        -> eRun $ snd (?env !! coerce x)
-- --   Z.Let x t u    -> Let x (ceval t) \v -> def x v $ exec u
-- --   Z.Lam x t      -> impossible
-- --   Z.App t u      -> eRun (ceval t $$ [ceval u])
-- --   Z.Erased{}     -> impossible
-- --   Z.Quote{}      -> impossible
-- --   -- Z.Splice t pos -> eRun $ cSplice (ceval t) pos

-- -- ceval :: Env => Tm -> JS
-- -- ceval = undefined
