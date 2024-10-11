{-# language OverloadedStrings #-}
{-# options_ghc -Wno-unused-imports #-}

module Compiler (genTop, rts) where

import Common hiding (Lvl)
import ElabState
import Errors
import Pretty
import StringBuilder
import qualified Common as C
import qualified Zonk   as Z

import Control.Monad.State.Strict
import Data.IORef
import Data.String
import Data.Void
import Lens.Micro.Platform
import Prelude hiding (const, tail)
import System.IO.Unsafe
import qualified Data.Set as S


-- Closure conversion
--------------------------------------------------------------------------------

type ZTm = Z.Tm Void

data Top
  = TLet Name Tm Top
  | TBind Name Tm Top
  | TSeq Tm Top
  | TBody Tm
  | TClosure Name [Name] Name Tm Top -- name, env, arg, body
  deriving Show

data Tm
  = Var Name
  | CSP Name
  | Let Name Tm Tm
  | LiftedLam Name [Name] -- name, env application
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
  , sNextId   :: Int
  , sClosures :: TopClosures
  } deriving Show

makeFields ''S

type Env     = (?env     :: [(Bool, Bool, Name)])  -- is closed, is top-level, name
type Mode    = (?mode    :: Maybe Int)
type TopName = (?topName :: String)

freshenName :: Env => Name -> Name
freshenName x
  | any ((==x).(\(_, _, x) -> x)) ?env =
     freshenName $ x ++ show (length ?env)
  | otherwise = x

fresh :: Name -> (Env => Name -> a) -> Env => a
fresh x act = let x' = freshenName x in
              let ?env = (False, False, x') : ?env in
              act x'

-- create fresh name, run action, delete bound name from freevars of the result
bind :: Name -> (Env => Name -> State S a) -> Env => State S a
bind x act = fresh x \x -> do
  a <- act x
  s <- get
  freeVars %= S.delete x
  pure a

cconv :: Env => Mode => TopName => ZTm -> State S Tm
cconv = \case
  Z.Var x -> do
    (closed, top, x) <- pure $! ?env !! coerce x
    when (not top) $ freeVars %= S.insert x
    pure $! if closed then CSP x else Var x

  Z.Let x t u -> do
    t <- cconv t
    bind x \x -> Let x t <$> cconv u

  Z.Lam x t -> case ?mode of
    Nothing -> fresh x \x -> do
      old_fvs    <- use freeVars
      freeVars   .= S.empty
      t          <- cconv t
      captureSet <- S.delete x <$> use freeVars
      capture    <- pure $ S.toList captureSet
      clId       <- nextId <<%= (+1)
      clName     <- pure $! ?topName++show clId++"_"
      closures   %= ((clName, capture, x, t):)
      freeVars   .= S.union old_fvs captureSet
      pure $ LiftedLam clName capture
    Just{} ->
      bind x \x -> Lam x <$> cconv t

  Z.App t u    -> App <$> cconv t <*> cconv u
  Z.Erased s   -> pure $ Erased s
  Z.Quote t    -> case ?mode of
                    Nothing -> do
                      let ?mode = Just 1
                          ?env  = map (\(_, top, x) -> (True, top, x)) ?env
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

cconv0 :: Env => Mode => Name -> ZTm -> (Tm, TopClosures)
cconv0 x t =
  let ?topName = x in
  case runState (cconv t) (S mempty 0 []) of
    (t, S _ _ cs) -> (t, cs)

addClosures :: TopClosures -> Top -> Top
addClosures cs t =
  foldl' (\acc (x, env, arg, t) -> TClosure x env arg t acc) t cs

cconvTop :: Env => Mode => ZTm -> Top
cconvTop = \case
  Z.Let (freshenName -> x) t u ->
    let (t', cs) = cconv0 x t in
    let ?env     = (False, True, x) : ?env in
    addClosures cs $ TLet x t' (cconvTop u)
  Z.Bind (freshenName -> x) t u ->
    let (t', cs) = cconv0 x t in
    let ?env     = (False, True, x) : ?env in
    addClosures cs $ TBind x t' (cconvTop u)
  Z.Seq t u ->
    let (t', cs) = cconv0 "cl" t in
    addClosures cs $ TSeq t' (cconvTop u)
  t ->
    case cconv0 "cl" t of
      (t', cs) -> addClosures cs (TBody t')

runCConv :: ZTm -> Top
runCConv t = let ?env = []; ?mode = Nothing in cconvTop t


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
  Tail -> "const " <> str x <> " = " <> indent (nonTail t) <> ";" <> newl <> tail u
  _    -> "((" <> str x <> ") => " <> nonTail u <> ")(" <> nonTail t <> ")"

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

jLamExp :: Cxt => [Name] -> (Cxt => Out) -> Out
jLamExp xs t = jReturn $ jTuple (map str xs) <> " => " <> nonTail t

cApp :: (Cxt => Out) -> (Cxt => Out) -> (Cxt => Out)
cApp t u = jReturn $ parens t <> "._1(" <> u <> ")"

parens :: Out -> Out
parens t = "(" <> t <> ")"

jApp :: (Cxt => Out) -> (Cxt => [Out]) -> (Cxt => Out)
jApp t args = jReturn $ t <> jTuple args

cRun :: (Cxt => Out) -> (Cxt => Out)
cRun t = jReturn $ t <> "()"

jClosure :: [Name] -> Name -> (Cxt => Out) -> (Cxt => Out)
jClosure []  x t = jLam [x] t
jClosure env x t = jLamExp env $ jLam [x] t

jAppClosure :: (Cxt => Out) -> (Cxt => [Out]) -> (Cxt => Out)
jAppClosure t args = jReturn $ case args of
  []   -> t
  args -> t <> jTuple args

closeVar :: Name -> Name
closeVar x = x ++ "c"

openVar :: Name -> Name
openVar x = x ++ "o"

execTop :: Top -> Out
execTop t = tail $ go t where
  go :: Cxt => Top -> Out
  go = \case
    TLet x t u  ->
      jLet x (ceval t) (newl <> go u)
    TBind x t u ->
      jLet x (exec t) (newl <> go u)
    TSeq t u    ->
      jLet "_" (exec t) (newl <> go u)
    TClosure x env arg body t ->
      jLet (closeVar x) (jClosure env arg $ ceval body) $
      jLet (openVar x)  (jClosure env arg $ stage 0 $ oeval body) $
      go t

    -- finalize
    TBody t ->
      "const main_ = () => {" <> exec t <> "};" <> newl <>
      "console.log(main_())" <> newl

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
  LiftedLam x env -> jReturn $ "{ _1 : " <> jAppClosure (str (closeVar x)) (map str env) <>
                               ", _2 : " <> jAppClosure (str (openVar x))  (map str env) <> "}"
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
  LiftedLam x env -> jAppClosure (str (openVar x)) (map str env)
  Lam x t         -> case ?stage of
                       0 -> jLam [x] (oeval t)
                       _ -> jApp "Lam_" [strLit x, jLam [x] (oeval t)]
  App t u         -> case ?stage of
                       0 -> jApp "openApp_" [oeval t, oeval u]
                       _ -> jApp "App_" [oeval t, oeval u]
  Erased s        -> case ?stage of
                       0 -> jReturn "Erased_"
                       _ -> jReturn "undefined"
  Quote t         -> jApp "Quote_" [stage (?stage + 1) (oeval t)]
  Splice t _      -> case ?stage of
                       0 -> jApp "openSplice0_" [oeval t]
                       _ -> stage (?stage - 1) $ jApp "openSplice_" [oeval t]
  Return t        -> jApp "Return_" [oeval t]
  Bind x t u      -> jApp "Bind_" [strLit x, oeval t, jLam [x] (oeval u)]
  Seq t u         -> jApp "Seq_" [oeval t, oeval u]
  New t           -> jApp "New_" [oeval t]
  Write t u       -> jApp "Write_" [oeval t, oeval u]
  Read t          -> jApp "Read_" [oeval t]



--------------------------------------------------------------------------------

rts :: Out
rts = str $ unlines [
   ""
  ,"'use strict';"
  ,""
  ,"function impossible(){"
  ,"    throw new Error('Impossible')"
  ,"}"
  ,""
  ,"// const _Var    = 0"
  ,"// const _Let    = 1"
  ,"// const _Lam    = 2"
  ,"// const _App    = 3"
  ,"// const _Erased = 4"
  ,"// const _Quote  = 5"
  ,"// const _Splice = 6"
  ,"// const _Return = 7"
  ,"// const _Bind   = 8"
  ,"// const _Seq    = 9"
  ,"// const _New    = 10"
  ,"// const _Write  = 11"
  ,"// const _Read   = 12"
  ,"// const _Closed = 13"
  ,""
  ,"const _Var    = 'Var'"
  ,"const _Let    = 'Let'"
  ,"const _Lam    = 'Lam'"
  ,"const _App    = 'App'"
  ,"const _Erased = 'Erased'"
  ,"const _Quote  = 'Quote'"
  ,"const _Splice = 'Splice'"
  ,"const _Return = 'Return'"
  ,"const _Bind   = 'Bind'"
  ,"const _Seq    = 'Seq'"
  ,"const _New    = 'New'"
  ,"const _Write  = 'Write'"
  ,"const _Read   = 'Read'"
  ,"const _Closed = 'Closed'"
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

genTop :: Z.Tm Void -> Out
genTop = execTop . runCConv
