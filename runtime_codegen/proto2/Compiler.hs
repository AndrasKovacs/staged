{-# language OverloadedStrings #-}
{-# options_ghc -Wno-unused-imports #-}

module Compiler (genTop) where

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

strLit :: String -> Out
strLit s = "'" <> str s <> "'"

instance IsString Out where fromString s = Out (\_ -> Chunk s)

-- indent :: Out -> Out
-- indent (Out f) = Out (\i -> f $! i + 4)

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
  (reverse (s^.top), t')



-- Code generation
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

cApp :: (Cxt => Out) -> (Cxt => Out) -> (Cxt => Out)
cApp t u = jReturn $ parens t <> "._1(" <> u <> ")"

parens :: Out -> Out
parens t = "(" <> t <> ")"

jApp :: (Cxt => Out) -> (Cxt => [Out]) -> (Cxt => Out)
jApp t args = jReturn $ t <> jTuple args

cRun :: (Cxt => Out) -> (Cxt => Out)
cRun t = jReturn $ t <> "()"

jLamEnv :: [Name] -> (Cxt => Out) -> (Cxt => Out)
jLamEnv []  t = t
jLamEnv env t = jLam env t

jAppEnv :: (Cxt => Out) -> (Cxt => [Out]) -> (Cxt => Out)
jAppEnv t args = jReturn $ case args of
  []   -> t
  args -> t <> jTuple args

closeVar :: Name -> Name
closeVar x = x ++ "c"

openVar :: Name -> Name
openVar x = x ++ "o"

execTop :: (TopClosures, Tm) -> Out
execTop (topcls, body) = tail $ go topcls where

  go :: Cxt => TopClosures -> Out
  go [] =
    exec body
  go ((n, env, arg, t):topcls) =
    jLet (closeVar n) (jLamEnv env $ jLam [arg] $ ceval t) $
    jLet (openVar n)  (jLamEnv env $ jLam [arg] $ stage 0 $ oeval t) $
    go topcls

exec :: Cxt => Tm -> Out
exec = \case
  Var x       -> cRun (str x)
  CSP {}      -> impossible
  Let x t u   -> jLet x (ceval t) (exec u)
  LiftedLam{} -> impossible
  Lam{}       -> impossible
  App t u     -> cRun (ceval t `cApp` ceval u)
  Erased{}    -> impossible
  Quote{}     -> impossible
  Splice t _  -> jApp "closedExec_" [ceval t]
  Return t    -> jReturn $ ceval t
  Bind x t u  -> jLet x (exec t) (exec u)
  Seq t u     -> jLet "_" (exec t) (exec u)
  New t       -> jReturn $ "{_1 : " <> ceval t <> "}"
  Write t u   -> nonTail $ ceval t <> "._1 = " <> ceval u
  Read t      -> jReturn $ ceval t <> "._1"

ceval :: Cxt => Tm -> Out
ceval = \case
  Var x           -> jReturn (str x)
  CSP{}           -> impossible
  Let x t u       -> jLet x (ceval t) (ceval u)
  LiftedLam x env -> jReturn $ "{ _1 : " <> jAppEnv (str (closeVar x)) (map str env) <>
                               ", _2 : " <> jAppEnv (str (openVar x))  (map str env) <> "}"
  Lam{}           -> impossible
  App t u         -> cApp (ceval t) (ceval u)
  Erased s        -> jReturn "undefined"
  Quote t         -> stage 1 $ oeval t
  Splice t _      -> jApp "closedEval_" [ceval t]
  t@Return{}      -> jLam [] $ exec t
  t@Bind{}        -> jLam [] $ exec t
  t@Seq{}         -> jLam [] $ exec t
  t@Write{}       -> jLam [] $ exec t
  t@Read{}        -> jLam [] $ exec t
  t@New{}         -> jLam [] $ exec t

oeval :: Cxt => Stage => Tm -> Out
oeval = \case
  Var x           -> jReturn (str x)
  CSP x           -> jApp "Closed_" [str x]
  Let x t u       -> case ?stage of
                       0 -> jLet x (oeval t) (oeval u)
                       _ -> jApp "Let_" [strLit x, oeval t, jLam [x] (oeval u)]
  LiftedLam x env -> jAppEnv (str (openVar x)) (map str env)
  Lam x t         -> jLam [x] (oeval t)
  App t u         -> case ?stage of
                       0 -> jApp "openApp_" [oeval t, oeval u]
                       _ -> jApp "App_" [oeval t, oeval u]
  Erased s        -> case ?stage of
                       0 -> jReturn "Erased_"
                       _ -> jReturn "undefined"
  Quote t         -> jApp "Quote_" [stage (?stage + 1) (oeval t)]
  Splice t _      -> case ?stage of
                       0 -> jApp "openSplice0_" [oeval t]
                       _ -> jApp "openSplice_" [oeval t]
  Return t        -> jApp "Return_" [oeval t]
  Bind x t u      -> jApp "Bind_" [strLit x, oeval t, jLam [x] (oeval u)]
  Seq t u         -> jApp "Seq_" [oeval t, oeval u]
  New t           -> jApp "New_" [oeval t]
  Write t u       -> jApp "Write_" [oeval t, oeval u]
  Read t          -> jApp "Read_" [oeval t]

rts :: String
rts = unlines [
   ""
  ,"'use strict';"
  ,""
  ,"function impossible(){"
  ,"    throw new Error('Impossible')"
  ,"}"
  ,""
  ,"const _Var    = 0"
  ,"const _Let    = 1"
  ,"const _Lam    = 2"
  ,"const _App    = 3"
  ,"const _Erased = 4"
  ,"const _Quote  = 5"
  ,"const _Splice = 6"
  ,"const _Return = 7"
  ,"const _Bind   = 8"
  ,"const _Seq    = 9"
  ,"const _New    = 10"
  ,"const _Write  = 11"
  ,"const _Read   = 12"
  ,"const _Closed = 13"
  ,""
  ,"function Var_    (x)       {return {tag: _Var, _1: x}}"
  ,"function Let_    (x, t, u) {return {tag: _Let, _1: x, _2: t, _3: u}}"
  ,"function Lam_    (x, t)    {return {tag: _Lam, _1: x, _2: t}}"
  ,"function App_    (t, u)    {return {tag: _App, _1: t, _2: t}}"
  ,"const    Erased_ =         {tag: _Erased}"
  ,"function Quote_  (t)       {return {tag: _Quote, _1: t}}"
  ,"function Splice_ (t)       {return {tag: _Splice, _1: t}}"
  ,"function Return_ (t)       {return {tag: _Return, _1: t}}"
  ,"function Bind_   (x, t, u) {return {tag: _Bind, _1: x, _2: t, _3: u}}"
  ,"function Seq_    (t, u)    {return {tag: _Seq, _1: t, _2: u}}"
  ,"function New_    (t)       {return {tag: _New, _1: t}}"
  ,"function Write_  (t, u)    {return {tag: _Write, _1: t, _2: u}}"
  ,"function Read_   (t)       {return {tag: _Read, _1: t}}"
  ,"function Closed_ (t)       {return {tag: _Closed, _1: t}}"
  ,""
  ,"function openApp_(t, u) {"
  ,"    if (t.tag == _Closed) {"
  ,"        if (u.tag == _Closed) {"
  ,"            return Closed_(t._1._1(u._1))"
  ,"        } else {"
  ,"            return t._1._2(u)"
  ,"        }"
  ,"    } else if (t.tag == _Lam) {"
  ,"        return t._1(u)"
  ,"    } else {"
  ,"        impossible()"
  ,"    }"
  ,"}"
  ,""
  ,"function openSplice0_(t) {"
  ,"    throw new Error('code generation not implemented')"
  ,"}"
  ,""
  ,"function openSplice_(t) {"
  ,"    if (t.tag == _Quote){"
  ,"        return t._1"
  ,"    } else {"
  ,"        return Splice_(t)"
  ,"    }"
  ,"}"
  ,""
  ,"function closedExec_(t){"
  ,"    throw new Error('code generation not implemented')"
  ,"}"
  ,""
  ,"function closedEval_(t){"
  ,"    throw new Error('code generation not implemented')"
  ,"}"
  ]

genTop :: Z.Tm Void -> String
genTop t =
  let t' = cconvTop t in
  trace "CCONV:" $ traceShow t' $ out $ execTop t'
  -- out (str rts <> newl <> newl <> execTop (cconvTop t))
