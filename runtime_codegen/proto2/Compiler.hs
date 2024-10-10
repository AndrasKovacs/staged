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
import qualified Data.Set as S
import Lens.Micro.Platform
import Prelude hiding (const, tail)

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
  | CSP Name
  | Let Name Tm Tm
  | LiftedLam Name [Name] -- top code name, application to env
  | Lam Name Tm
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

type TopClosures = [(Name, [Name], Name, Tm)]

data S = S {
    sFreeVars :: S.Set Name
  , sTopLen   :: Int
  , sTop      :: TopClosures
  } deriving Show

makeFields ''S

type Env   = (?env  :: [(Bool, Name)])  -- is var closed, name
type Mode  = (?mode :: Maybe Int)

fresh :: Name -> (Env => Name -> a) -> Env => a
fresh x act
  | any ((==x).snd) ?env =
     let x' = x ++ show (length ?env) in
     let ?env = (False, x') : ?env in act x'
  | otherwise =
     let ?env = (False, x) : ?env in act x

-- create fresh name, run action, delete bound name from freevars of the result
bind :: Name -> (Env => Name -> State S a) -> Env => State S a
bind x act = fresh x \x -> do
  a <- act x
  s <- get
  freeVars %= S.delete x
  pure a

cconv :: Env => Mode => ZTm -> State S Tm
cconv = \case
  Z.Var x -> do
    (c, x) <- pure $! ?env !! coerce x
    freeVars %= S.insert x
    pure $! if c then CSP x else Var x

  Z.Let x t u -> do
    t <- cconv t
    bind x \x -> Let x t <$> cconv u

  Z.Lam x t -> case ?mode of
    Nothing -> fresh x \x -> do
      old_fvs   <- use freeVars
      freeVars  .= S.empty
      t         <- cconv t
      t_capture <- S.delete x <$> use freeVars
      topid     <- topLen <<%= (+1)
      topid     <- pure $! "_"++show topid
      capture   <- pure $! S.toList t_capture
      top       %= ((topid, capture, x, t):)
      freeVars  .= old_fvs
      pure $ LiftedLam topid capture
    Just{} ->
      bind x \x -> Lam x <$> cconv t

  Z.App t u    -> App <$> cconv t <*> cconv u
  Z.Erased s   -> pure $ Erased s
  Z.Quote t    -> case ?mode of
                    Nothing -> do
                      let ?mode = Just 1
                          ?env  = map (\(_, x) -> (True, x)) ?env
                      Quote <$> cconv t
                    Just s -> do
                      let ?mode = Just $! s + 1
                      Quote <$> cconv t
  Z.Splice t p -> do let ?mode = case ?mode of
                          Nothing -> Nothing
                          Just 0  -> Just 0
                          Just s  -> Just $! s - 1
                     Splice <$> cconv t <*> pure p

  Z.Return t   -> Return <$> cconv t
  Z.Bind x t u -> do {t <- cconv t; bind x \x -> Bind x t <$> cconv u}
  Z.Seq t u    -> Seq <$> cconv t <*> cconv u
  Z.New t      -> New <$> cconv t
  Z.Write t u  -> Write <$> cconv t <*> cconv u
  Z.Read t     -> Read <$> cconv t

cconvTop :: ZTm -> (TopClosures, Tm)
cconvTop t =
  let ?env  = []
      ?mode = Nothing in
  let (t', s) = runState (cconv t) (S mempty 0 []) in
  (s^.top, t')

--------------------------------------------------------------------------------

type Stage = (?stage :: Int)
data Context = Tail | NonTail deriving Show
type Cxt   = (?cxt :: Context)

stage :: Int -> (Stage => a) -> a
stage s act = let ?stage = s in act

tail :: (Cxt => Out) -> Out
tail act = let ?cxt = Tail in act

nonTail :: (Cxt => Out) -> Out
nonTail act = let ?cxt = NonTail in act

jLet :: Cxt => Name -> (Cxt => Out) -> (Cxt => Out) -> Out
jLet x t u = case ?cxt of
  Tail -> "const " <> str x <> " = " <> nonTail t <> ";" <> newl <> tail u
  _    -> "((" <> str x <> ") => " <> nonTail u <> ")" <> nonTail t

jTuple :: [Out] -> Out
jTuple xs = "(" <> go xs <> ")" where
  go = \case
    []     -> mempty
    [x]    -> x
    (x:xs) -> x <> ", " <> go xs

jReturn :: (Cxt => Out) -> (Cxt => Out)
jReturn t = case ?cxt of
  Tail -> "return " <> nonTail t
  _    -> t

jLam :: Cxt => [Name] -> (Cxt => Out) -> Out
jLam xs t = jReturn $ jTuple (map str xs) <> " => {" <> tail t <> "}"

jClApp :: (Cxt => Out) -> (Cxt => Out) -> (Cxt => Out)
jClApp t u = jReturn $ t <> "._1(" <> u <> ")"

jApp :: (Cxt => Out) -> (Cxt => [Out]) -> (Cxt => Out)
jApp t args = jReturn $ t <> jTuple args

jRun :: (Cxt => Out) -> (Cxt => Out)
jRun t = jReturn $ t <> "()"

jUndefined :: Cxt => Out
jUndefined = case ?cxt of
  Tail -> "return undefined"
  _    -> "undefined"

--------------------------------------------------------------------------------

cevalTop :: (TopClosures, Tm) -> Out
cevalTop (topcls, body) = tail $ go topcls where

  go :: Cxt => TopClosures -> Out
  go []                        = cexec body
  go ((n, env, arg, t):topcls) =
    jLet (n ++ "closed") (jLam env $ jLam [arg] $ ceval t) $
    jLet (n ++ "open")   (jLam env $ jLam [arg] $ stage 0 $ oeval t) $
    go topcls

cexec :: Cxt => Tm -> Out
cexec = \case
  Var x           -> jRun (str x)
  CSP {}          -> impossible
  Let x t u       -> jLet x (ceval t) (cexec u)
  LiftedLam{}     -> impossible
  Lam{}           -> impossible
  App t u         -> jRun (ceval t `jClApp` ceval u)
  Erased{}        -> impossible
  Quote{}         -> impossible
  Splice t _      -> jApp "cexec_" [ceval t]
  Return t        -> jReturn $ ceval t
  Bind x t u      -> jLet x (cexec t) (cexec u)
  Seq t u         -> jLet "_" (cexec t) (cexec u)
  New t           -> jReturn $ "{_1 : " <> ceval t <> "}"
  Write t u       -> nonTail (ceval t) <> "._1 = " <> nonTail (ceval u)
  Read t          -> jReturn $ ceval t <> "._1"

ceval :: Cxt => Tm -> Out
ceval = \case
  Var x           -> jReturn (str x)
  CSP{}           -> impossible
  Let x t u       -> jLet x (ceval t) (ceval u)
  LiftedLam x env -> jReturn $ "{ _1 : " <> jApp (str (x ++ "closed")) (map str env) <>
                               ", _2 : " <> jApp (str (x ++ "open"))   (map str env) <> "}"
  Lam{}           -> impossible
  App t u         -> jClApp (ceval t) (ceval u)
  Erased s        -> jUndefined
  Quote t         -> stage 1 $ oeval t
  Splice t _      -> jApp "ceval_" [ceval t]
  t@Return{}      -> jLam [] $ cexec t
  t@Bind{}        -> jLam [] $ cexec t
  t@Seq{}         -> jLam [] $ cexec t
  t@Write{}       -> jLam [] $ cexec t
  t@Read{}        -> jLam [] $ cexec t
  t@New{}         -> jLam [] $ cexec t

oClosed :: (Cxt => Out) -> (Cxt => Out)
oClosed t = jReturn $ "Closed_" <> jTuple [t]

oLet :: Cxt => Stage => Name -> (Cxt => Out) -> (Cxt => Out) -> Out
oLet x t u = case ?stage of
  0 -> jLet x t u
  _ -> jApp "Let_" [str x, t, u]

oApp :: Cxt => Stage => (Cxt => Out) -> (Cxt => Out) -> Out
oApp t u = case ?stage of
  0 -> jApp "openApp_" [t, u]
  _ -> _

oeval :: Cxt => Stage => Tm -> Out
oeval = \case
  Var x       -> jReturn (str x)
  CSP x       -> oClosed (str x)
  Let x t u   -> oLet x (oeval t) (oeval u)
  LiftedLam{} -> impossible
  Lam x t     -> jLam [x] (oeval t)
  App t u     -> oApp (oeval t) (oeval u)

-- appCl :: Out -> Out -> Out
-- appCl t u = t <> "._1(" <> u <> ")"

-- app :: Out -> Out -> Out
-- app t u = t <> "(" <> u <> ")"

-- runAction :: Out -> Out
-- runAction t = t <> "()"



-- ret :: Out -> Out
-- ret t = "return " <> tc

-- cexec :: Tm -> Out
-- cexec = \case
--   Var x           -> runAction (str x)
--   Let x t u       -> const x (ceval t) (cexec u)
--   LiftedLam{}     -> impossible
--   Lam{}           -> impossible
--   App t u         -> ret $ runAction (ceval t `appCl` ceval u)
--   Erased{}        -> impossible
--   Quote{}         -> impossible
--   Splice t _      -> ret $ runAction ("_ceval" `app` ceval t)
--   -- Return t        -> _
--   -- Return t        ->
--   -- Var x      -> eRun $ ix x
--   -- Let _ t u  -> "const " <> top <> " = " <> ceval t <> ";" <> newl <> bind (exec u)
--   -- Lam{}      -> impossible
--   -- App t u    -> eRun (cApp (ceval t) (ceval u))
--   -- Erased _   -> impossible
--   -- Quote{}    -> impossible
--   -- Splice t _ -> eRun $ "_ceval(" <> ceval t <> ")"
--   -- Return t   -> "return " <> ceval t <> ";"
--   -- Bind _ t u -> "const " <> top <> " = " <> exec t <> ";" <> newl <> bind (exec u)
--   -- Seq t u    -> exec t <> ";" <> newl <> exec u
--   -- New t      -> "{_1 : " <> ceval t <> "}"
--   -- Write t u  -> ceval t <> "._1 = " <> ceval u
--   -- Read t     -> ceval t <> "._1"


-- ceval :: Tm -> Out
-- ceval = undefined

-- oeval :: Stage => Tm -> Out
-- oeval = undefined


-- -- ix :: Lvl => Ix -> Out
-- -- ix x = str ('x' : show (coerce ?lvl - x - 1))

-- -- top :: Lvl => Out
-- -- top = str ('x' : show ?lvl)

-- -- eRun :: Out -> Out
-- -- eRun t = t <> "()"

-- -- cApp :: Out -> Out -> Out
-- -- cApp t u = t <> "(" <> u <> ")"

-- -- exec :: Lvl => Tm -> Out
-- -- exec = \case
-- --  Var x      -> eRun $ ix x
-- --  Let _ t u  -> "const " <> top <> " = " <> ceval t <> ";" <> newl <> bind (exec u)
-- --  Lam{}      -> impossible
-- --  App t u    -> eRun (cApp (ceval t) (ceval u))
-- --  Erased _   -> impossible
-- --  Quote{}    -> impossible
-- --  Splice t _ -> eRun $ "_ceval(" <> ceval t <> ")"
-- --  Return t   -> "return " <> ceval t <> ";"
-- --  Bind _ t u -> "const " <> top <> " = " <> exec t <> ";" <> newl <> bind (exec u)
-- --  Seq t u    -> exec t <> ";" <> newl <> exec u
-- --  New t      -> "{_1 : " <> ceval t <> "}"
-- --  Write t u  -> ceval t <> "._1 = " <> ceval u
-- --  Read t     -> ceval t <> "._1"

-- -- lam :: Lvl => Out -> Out
-- -- lam t = "(" <> top <> ") => {" <> t <> "}"

-- -- ceval :: Lvl => Tm -> Out
-- -- ceval = \case
-- --   Var x     -> ix x
-- --   Let _ t u -> "const " <> top <> " = " <> ceval t <> ";" <> newl <> bind (ceval u)

-- --   -- TODO: get rid of code duplication!!
-- --   Lam _ t   -> "{_1: " <> lam (bind (ceval t)) <> ", _2: " <> lam (bind $ stage 0 $ oeval t) <> "}"

-- --   App t u   -> cApp (ceval t) (ceval u)
-- --   Erased{}  -> "undefined"
-- --   Quote t   -> stage 1 $ oeval t  -- TODO: close environment!!!

-- -- oeval :: Lvl => Stage => Tm -> Out
-- -- oeval = \case


-- -- -- oeval :: OEnv => Lvl => Stage => Tm -> Open
-- -- -- oeval = \case
-- -- --   Var x      -> snd (?env !! coerce x)
-- -- --   Lam x t    -> OLam x (coerce \l v -> def x v $ lvl l $ oeval t)
-- -- --   Erased s   -> OErased s
-- -- --   Quote t    -> OQuote $ stage (?stage + 1) $ oeval t
-- -- --   Return t   -> OReturn (oeval t)
-- -- --   Bind x t u -> OBind x (oeval t) (NoShow \l -> lvl l $ oeval u)
-- -- --   Seq t u    -> OSeq (oeval t) (oeval u)
-- -- --   New t      -> ONew (oeval t)
-- -- --   Write t u  -> OWrite (oeval t) (oeval u)
-- -- --   Read t     -> ORead (oeval t)
-- -- --   CSP x t    -> OClosed x t
-- -- --   t -> case ?stage of
-- -- --     0 -> case t of
-- -- --       Let x t u    -> def x (oeval t) (oeval u)
-- -- --       App t u      -> case (oeval t, oeval u) of
-- -- --                         (OClosed x (CLam _ f _), OClosed x' u) -> OClosed Nothing (coerce f u)
-- -- --                         (OClosed x (CLam _ _ f), u           ) -> coerce f ?lvl u
-- -- --                         (OLam _ f              , u           ) -> coerce f ?lvl u
-- -- --                         (t                     , u           ) -> OApp t u
-- -- --       Splice t pos -> case oeval t of
-- -- --                         OQuote t -> env (idEnv ?lvl) $ oeval $ traceGen t pos
-- -- --                         t        -> OSplice t
-- -- --     _ -> case t of
-- -- --       Let x t u    -> OLet x (oeval t) (NoShow \l -> lvl l $ oeval u)
-- -- --       App t u      -> OApp (oeval t) (oeval u)
-- -- --       Splice t pos -> case stage (?stage - 1) $ oeval t of
-- -- --                         OQuote t -> t
-- -- --                         t        -> OSplice t








-- -- -- exec :: CEnv => Tm -> IO Closed
-- -- -- exec = \case
-- -- --   Var x        -> eRun (snd (?env !! coerce x))
-- -- --   Let x t u    -> def x (ceval t) (exec u)
-- -- --   Lam x t      -> impossible
-- -- --   App t u      -> eRun $ cApp (ceval t) (ceval u)
-- -- --   Erased{}     -> impossible
-- -- --   Quote t      -> impossible
-- -- --   Splice t pos -> eRun $ cSplice (ceval t) pos
-- -- --   Return t     -> pure $! ceval t
-- -- --   Bind x t u   -> do {t <- exec t; def x t $ exec u}
-- -- --   Seq t u      -> exec t >> exec u
-- -- --   New t        -> CRef <$!> (coerce (newIORef $! ceval (coerce t)))
-- -- --   Write t u    -> case ceval t of
-- -- --                     CRef r -> CErased "tt" <$ (writeIORef (coerce r) $! ceval u)
-- -- --                     _      -> impossible
-- -- --   Read t       -> case ceval t of
-- -- --                     CRef r -> readIORef (coerce r)
-- -- --                     _      -> impossible
-- -- --   CSP x t      -> eRun t











-- -- -- --------------------------------------------------------------------------------

-- -- -- type Tm    = Z.Tm Void
-- -- -- type Env   = (?env   :: [(Name, JS)])
-- -- -- type Lvl   = (?lvl   :: C.Lvl)
-- -- -- type Stage = (?stage :: Int)

-- -- -- data JS
-- -- --   = Var Name
-- -- --   | Lam Name (JS -> JS)
-- -- --   | App JS [JS]
-- -- --   | Obj [(Name, JS)]
-- -- --   | Field JS Name
-- -- --   | Arr [JS]
-- -- --   | Index JS JS
-- -- --   | Str String
-- -- --   | ConsoleLog [JS]
-- -- --   | Let Name JS (JS -> JS)
-- -- --   | Undefined

-- -- --   -- -- Open code is kinda an ADT in js
-- -- --   -- | OVar C.Lvl
-- -- --   -- | OLet Name JS (JS -> JS)
-- -- --   -- | OLam Name (NoShow (C.Lvl -> Open -> Open))
-- -- --   -- | OApp Open Open
-- -- --   -- | OErased String
-- -- --   -- | OQuote Open
-- -- --   -- | OSplice Open
-- -- --   -- | OReturn Open
-- -- --   -- | OBind Name Open (NoShow (C.Lvl -> Open))
-- -- --   -- | OSeq Open Open
-- -- --   -- | ONew Open
-- -- --   -- | OWrite Open Open
-- -- --   -- | ORead Open
-- -- --   -- | OClosed (Maybe Name) Closed
-- -- --   -- deriving Show

-- -- -- infixl 8 $$
-- -- -- ($$) :: JS -> [JS] -> JS
-- -- -- ($$) = App

-- -- -- stage :: Int -> (Stage => a) -> a
-- -- -- stage s act = let ?stage = s in act

-- -- -- lvl :: C.Lvl -> (Lvl => a) -> a
-- -- -- lvl l act = let ?lvl = l in act

-- -- -- --------------------------------------------------------------------------------

-- -- -- def :: Name -> JS -> (Env => a) -> (Env => a)
-- -- -- def x t act = let ?env = (x, t): ?env in act

-- -- -- eRun :: JS -> JS
-- -- -- eRun v = v $$ []

-- -- -- -- cSplice :: JS -> Maybe SourcePos -> JS
-- -- -- -- cSplice t pos = Case

-- -- --   -- CQuote t -> env [] $ lvl 0 $ ceval $ traceGen t pos
-- -- --   -- _        -> impossible

-- -- -- exec :: Env => Tm -> JS
-- -- -- exec = \case
-- -- --   Z.Var x        -> eRun $ snd (?env !! coerce x)
-- -- --   Z.Let x t u    -> Let x (ceval t) \v -> def x v $ exec u
-- -- --   Z.Lam x t      -> impossible
-- -- --   Z.App t u      -> eRun (ceval t $$ [ceval u])
-- -- --   Z.Erased{}     -> impossible
-- -- --   Z.Quote{}      -> impossible
-- -- --   -- Z.Splice t pos -> eRun $ cSplice (ceval t) pos

-- -- -- ceval :: Env => Tm -> JS
-- -- -- ceval = undefined
