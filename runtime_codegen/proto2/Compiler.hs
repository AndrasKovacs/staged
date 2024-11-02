{-# language OverloadedStrings #-}
{-# options_ghc -Wno-unused-imports  #-}

module Compiler (genTop) where

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
import System.Directory

import Paths_rtcg


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
  | Let Name Tm Tm
  | LiftedLam Name Name [Name] -- function name, arg name, env application
  | Lam Name Tm
  | App Tm Tm
  | Erased String
  | Quote Tm
  | Splice Tm (Maybe String)
  | Return Tm
  | Bind Name Tm Tm
  | Seq Tm Tm
  | New Tm
  | Write Tm Tm
  | Read Tm
  | Log String
  | ReadNat
  | PrintNat Tm
  | Rec [(Name, Tm)]
  | Proj Tm Name
  | NatLit Integer
  | Suc Tm
  | NatElim Tm Tm Tm
  deriving Show

type TopClosures = [(Name, [Name], Name, Tm)]

data S = S {
    sFreeVars :: S.Set Name
  , sNextId   :: Int
  , sClosures :: TopClosures
  } deriving Show

makeFields ''S

type Env     = (?env     :: [(Bool, Name)])  -- is top-level, name
type Mode    = (?mode    :: Maybe Int)
type TopName = (?topName :: String)

mangle :: Name -> Name
mangle = concatMap \case
  '\'' -> "__"
  c    -> [c]

freshenName :: Env => Name -> Name
freshenName x = go (mangle x) where
  go :: Env => Name -> Name
  go x | any ((==x).(\(_, x) -> x)) ?env =
         go $ x ++ show (length ?env)
       | otherwise = "$" ++ x

fresh :: Name -> (Env => Name -> a) -> Env => a
fresh x act = let x' = freshenName x in
              let ?env = (False, x') : ?env in
              act x'

-- create fresh name, run action, delete bound name from freevars of the result
bind :: Name -> (Env => Name -> State S a) -> Env => State S a
bind x act = fresh x \x -> do
  a <- act x
  s <- get
  freeVars %= S.delete x
  pure a

-- desugar Open to iterated Let
ccOpen :: Env => Mode => TopName => [Name] -> ZTm -> ZTm -> State S Tm
ccOpen xs t u = do
  t <- cconv t
  bind "tmp" \tmp -> do
    let go :: Env => Mode => TopName => [Name] -> State S Tm
        go []     = cconv u
        go (x:xs) = bind x \x' -> Let x' (Proj (Var tmp) x) <$> go xs
    Let tmp t <$> go xs

cconv :: Env => Mode => TopName => ZTm -> State S Tm
cconv = \case
  Z.Var x -> do
    (top, x) <- pure $! ?env !! coerce x
    when (not top) $ freeVars %= S.insert x
    pure $ Var x

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
      pure $ LiftedLam clName x capture
    Just{} ->
      bind x \x -> Lam x <$> cconv t

  Z.App t u       -> App <$> cconv t <*> cconv u
  Z.Erased s      -> pure $ Erased s
  Z.Quote t       -> case ?mode of
                       Nothing -> do
                         let ?mode = Just 1
                         Quote <$> cconv t
                       Just s -> do
                         let ?mode = Just $! s + 1
                         Quote <$> cconv t
  Z.Splice t p    -> do let ?mode = case ?mode of
                              Nothing -> Nothing
                              Just 0  -> Just 0
                              Just s  -> Just $! s - 1
                        Splice <$> cconv t <*> pure p

  Z.Return t      -> Return <$> cconv t
  Z.Bind x t u    -> do {t <- cconv t; bind x \x -> Bind x t <$> cconv u}
  Z.Seq t u       -> Seq <$> cconv t <*> cconv u
  Z.New t         -> New <$> cconv t
  Z.Write t u     -> Write <$> cconv t <*> cconv u
  Z.Read t        -> Read <$> cconv t
  Z.NatLit n      -> pure $ NatLit n
  Z.Suc t         -> Suc <$> cconv t
  Z.NatElim s z n -> NatElim <$> cconv s <*> cconv z <*> cconv n
  Z.Rec ts        -> Rec <$> traverse (traverse cconv) ts
  Z.Proj t x      -> Proj <$> cconv t <*> pure x
  Z.Open xs t u   -> ccOpen xs t u
  Z.PrintNat t    -> PrintNat <$> cconv t
  Z.ReadNat       -> pure ReadNat
  Z.Log s         -> pure $ Log s


cconv0 :: Env => Mode => Name -> ZTm -> (Tm, TopClosures)
cconv0 x t =
  let ?topName = x in
  case runState (cconv t) (S mempty 0 []) of
    (t, S _ _ cs) -> (t, cs)

addClosures :: TopClosures -> Top -> Top
addClosures cs t =
  foldl' (\acc (x, env, arg, t) -> TClosure x env arg t acc) t cs

-- desugar Open to iterated Let
ccOpenTop :: Env => Mode => [Name] -> ZTm -> ZTm -> Top
ccOpenTop xs t u =
  let tmp = freshenName "tmp" in
  let (t', cs) = cconv0 tmp t in
  let ?env = (True, tmp) : ?env in

  let go :: Env => Mode => [Name] -> Top
      go []     = cconvTop u
      go (x:xs) = fresh x \x' -> TLet x' (Proj (Var tmp) x) (go xs)

  in addClosures cs $ TLet tmp t' $ go xs

cconvTop :: Env => Mode => ZTm -> Top
cconvTop = \case
  Z.Let (freshenName -> x) t u ->
    let (t', cs) = cconv0 x t in
    let ?env     = (True, x) : ?env in
    addClosures cs $ TLet x t' (cconvTop u)
  Z.Bind (freshenName -> x) t u ->
    let (t', cs) = cconv0 x t in
    let ?env     = (True, x) : ?env in
    addClosures cs $ TBind x t' (cconvTop u)
  Z.Seq t u ->
    let (t', cs) = cconv0 "$cl" t in
    addClosures cs $ TSeq t' (cconvTop u)
  Z.Open xs t u ->
    ccOpenTop xs t u
  t ->
    case cconv0 "$cl" t of
      (t', cs) -> addClosures cs (TBody t')

runCConv :: ZTm -> Top
runCConv t = let ?env = []; ?mode = Nothing in cconvTop t


-- Code generation
--------------------------------------------------------------------------------

type Stage   = (?stage :: Int)
data IsTail' = Tail | NonTail deriving Show
type IsTail  = (?isTail :: IsTail')
type Cxt     = (?cxt :: [(Name, Bool)]) -- (Name, is closed)
                                        -- We need to box if we reference a closed value from open code

stage :: Int -> (Stage => a) -> a
stage s act = let ?stage = s in act

tail :: (IsTail => Out) -> Out
tail act = let ?isTail = Tail in act

nonTail :: (IsTail => Out) -> Out
nonTail act = let ?isTail = NonTail in act

jLet :: Name -> Bool -> (IsTail => Cxt => Out) -> (IsTail => Cxt => Out) -> (IsTail => Cxt => Out)
jLet x closed t u = case ?isTail of
  Tail -> let u' = (let ?cxt = (x, closed): ?cxt in tail u) in
          "const " <> str x <> " = " <> indent (nonTail t) <> ";" <> newl <> u'
  _    -> let u' = (let ?cxt = (x, closed): ?cxt in nonTail u) in
            "((" <> str x <> ") => " <> parens u' <> ")(" <> nonTail t <> ")"

-- cOpen :: [Name] -> Bool -> (IsTail => Cxt => Out) -> (IsTail => Cxt => Out) -> (IsTail => Cxt => Out)
-- cOpen xs closed t u = case ?isTail of
--   Tail -> let u' = (let ?cxt = foldl' _ ?cxt xs in tail u) in
--           "const " <> str x <> " = " <> indent (nonTail t) <> ";" <> newl <> u'
--   _    -> _

-- cOpen :: [Name] -> Closed -> (CEnv => a) -> CEnv => a
-- cOpen xs t u = let ?env = foldl' (\env x -> ((x,) $! cProj t x) : env) ?env xs
--                in seq ?env u

jSeq :: IsTail => Cxt => (IsTail => Out) -> (IsTail => Out) -> Out
jSeq t u = case ?isTail of
  Tail -> indent (nonTail t) <> ";" <> newl <> tail u
  _    -> "(_) => " <> parens (nonTail u) <> ")(" <> nonTail t <> ")"

jTuple :: [Out] -> Out
jTuple xs = "(" <> go xs <> ")" where
  go = \case
    []     -> mempty
    [x]    -> x
    (x:xs) -> x <> ", " <> go xs

jReturn :: (IsTail => Out) -> (IsTail => Out)
jReturn t = case ?isTail of
  Tail -> "return " <> nonTail t
  _    -> t

jLam :: Cxt => [Name] -> Bool -> (IsTail => Cxt => Out) -> (IsTail => Cxt => Out)
jLam xs closed t =
  let t' = (let ?cxt = map (,closed) xs ++ ?cxt in tail t) in
  jReturn $ jTuple (map str xs) <> " => {" <> t' <> "}"

jLamExp :: [Name] -> Bool -> (IsTail => Cxt => Out) -> (IsTail => Cxt => Out)
jLamExp xs closed t =
  let t' = (let ?cxt = map (,closed) xs ++ ?cxt in nonTail t) in
  jReturn $ jTuple (map str xs) <> " => " <> t'

cApp :: (IsTail => Out) -> (IsTail => Out) -> (IsTail => Out)
cApp t u = jReturn $ parens t <> "._1(" <> u <> ")"

parens :: Out -> Out
parens t = "(" <> t <> ")"

jApp :: (IsTail => Out) -> (IsTail => [Out]) -> (IsTail => Out)
jApp t args = jReturn $ t <> jTuple args

cRun :: (IsTail => Out) -> (IsTail => Out)
cRun t = jReturn $ t <> "()"

cProj :: (IsTail => Out) -> Name -> (IsTail => Out)
cProj t x = jReturn $ t <> "." <> str x

jClosure :: [Name] -> Name -> Bool -> (Cxt => IsTail => Out) -> (Cxt => IsTail => Out)
jClosure []  x closed t = jLam [x] closed t
jClosure env x closed t = jLamExp env closed $ jLam [x] closed t

jAppClosure :: (IsTail => Out) -> (IsTail => [Out]) -> (IsTail => Out)
jAppClosure t args = jReturn $ case args of
  []   -> t
  args -> t <> jTuple args

closeVar :: Name -> Name
closeVar x = x ++ "c"

openVar :: Name -> Name
openVar x = x ++ "o"

cRec :: Cxt => [(Name, Tm)] -> Out
cRec []          = mempty
cRec [(x, t)]    = str x <> ": " <> nonTail (ceval t)
cRec ((x, t):ts) = str x <> ": " <> nonTail (ceval t) <> ", " <> cRec ts

spliceLoc :: Maybe String -> Out
spliceLoc = \case
  Nothing -> "undefined"
  Just loc -> "[" <> go (lines loc) <> "]" where
    go []     = ""
    go [l]    = strLit l
    go (l:ls) = strLit l <> ", " <> go ls

execTop :: Cxt => Top -> Out
execTop t = tail $ go t where
  go :: IsTail => Cxt => Top -> Out
  go = \case
    TLet x t u  ->
      jLet x True (ceval t) (newl <> go u)
    TBind x t u ->
      jLet x True (exec t) (newl <> go u)
    TSeq t u    ->
      jSeq (exec t) (newl <> go u)
    TClosure x env arg body t ->
      jLet (closeVar x) True (jClosure env arg True $ ceval body) $
      jLet (openVar x)  True (jClosure env arg False $ stage 0 $ oeval body) $
      go t

    -- finalize
    TBody t ->
      "const main_ = () => {" <> exec t <> "};" <> newl <>
      "console.log('RESULT:');console.log(main_())" <> newl

exec :: IsTail => Cxt => Tm -> Out
exec = \case
  Var x         -> cRun (str x)
  Let x t u     -> jLet x True (ceval t) (exec u)
  LiftedLam{}   -> impossible
  Lam{}         -> impossible
  App t u       -> cRun (ceval t `cApp` ceval u)
  Erased{}      -> impossible
  Quote{}       -> impossible
  Splice t loc  -> jApp "codegenExec_" [ceval t, spliceLoc loc]
  Return t      -> jReturn $ ceval t
  Bind x t u    -> jLet x True (exec t) (exec u)
  Seq t u       -> jSeq (exec t) (exec u)
  New t         -> jReturn $ "{_1 : " <> ceval t <> "}"
  Write t u     -> nonTail $ ceval t <> "._1 = " <> ceval u
  Read t        -> jReturn $ ceval t <> "._1"
  Log s         -> jApp "console.log" [strLit s]
  ReadNat       -> jApp "readNat_" []
  PrintNat t    -> jApp "printNat_" [ceval t]
  Rec{}         -> impossible
  Proj t x      -> cRun (cProj (ceval t) x)
  Suc{}         -> impossible
  NatLit{}      -> impossible
  NatElim s z n -> jApp "cNatElim_" [ceval s, ceval z, ceval n]

oevalVar :: Cxt => IsTail => Name -> Out
oevalVar x = case lookup x ?cxt of
  Nothing    -> impossible
  Just True  -> jApp "CSP_" [str x, strLit x]
  Just False -> jReturn (str x)

oRec :: Cxt => Stage => [(Name, Tm)] -> Out
oRec []         = mempty
oRec [(x,t)]    = "[" <> strLit x <> ", " <> nonTail (oeval t) <> "]"
oRec ((x,t):ts) = "[" <> strLit x <> ", " <> nonTail (oeval t) <> "], " <> oRec ts

ceval :: IsTail => Cxt => Tm -> Out
ceval = \case
  Var x             -> jReturn (str x)
  Let x t u         -> jLet x True (ceval t) (ceval u)
  LiftedLam f x env -> jReturn $ nonTail $
                          "{ _1 : " <> jAppClosure (str (closeVar f)) (map str env) <>
                          ", _2 : " <> jAppClosure (str (openVar f))  (map oevalVar env) <> "}"
  Lam{}           -> impossible
  App t u         -> cApp (ceval t) (ceval u)
  Erased s        -> jReturn "undefined"
  Quote t         -> stage 1 $ oeval t
  Splice t loc    -> jApp "codegenClosed_" [ceval t, spliceLoc loc]
  t@Return{}      -> jLam [] True $ exec t
  t@Bind{}        -> jLam [] True $ exec t
  t@Seq{}         -> jLam [] True $ exec t
  t@Write{}       -> jLam [] True $ exec t
  t@Read{}        -> jLam [] True $ exec t
  t@New{}         -> jLam [] True $ exec t
  t@Log{}         -> jLam [] True $ exec t
  t@ReadNat{}     -> jLam [] True $ exec t
  t@PrintNat{}    -> jLam [] True $ exec t
  Rec ts          -> jReturn $ "{" <> cRec ts <> "}"
  Proj t x        -> cProj (ceval t) x
  NatLit n        -> str (show n)
  Suc t           -> jApp "cSuc_" [ceval t]
  NatElim s z n   -> jApp "cNatElim_" [ceval s, ceval z, ceval n]

oeval :: IsTail => Cxt => Stage => Tm -> Out
oeval = \case
  Var x             -> oevalVar x
  Let x t u         -> case ?stage of
                         0 -> jLet x False (oeval t) (oeval u)
                         _ -> jApp "Let_" [strLit x, oeval t, jLam [x] False (oeval u)]
  LiftedLam f x env -> jApp "Lam_" [strLit x, jAppClosure (str (openVar f)) (map str env)]
  Lam x t           -> jApp "Lam_" [strLit x, jLam [x] False (oeval t)]
  App t u           -> case ?stage of
                         0 -> jApp "app_" [oeval t, oeval u]
                         _ -> jApp "App_" [oeval t, oeval u]
  Erased s          -> jReturn "CSP_undefined_"
  Quote t           -> jApp "quote_" [stage (?stage + 1) (oeval t)]
  Splice t loc      -> case ?stage of
                         0 -> jApp "codegenOpen_" [oeval t, spliceLoc loc]
                         _ -> stage (?stage - 1) $ jApp "splice_" [oeval t]
  Return t          -> jApp "Return_" [oeval t]
  Bind x t u        -> jApp "Bind_" [strLit x, oeval t, jLam [x] False (oeval u)]
  Seq t u           -> jApp "Seq_" [oeval t, oeval u]
  New t             -> jApp "New_" [oeval t]
  Write t u         -> jApp "Write_" [oeval t, oeval u]
  Read t            -> jApp "Read_" [oeval t]
  Log s             -> jApp "Log_" [strLit s]
  ReadNat           -> jReturn "ReadNat_"
  PrintNat t        -> jApp "PrintNat_" [oeval t]
  Rec ts            -> jApp "Rec_" [ "new Map([" <> oRec ts <> "])" ]
  Proj t x          -> case ?stage of
                         0 -> jApp "proj_" [oeval t, strLit x]
                         _ -> jApp "Proj_" [oeval t, strLit x]
  NatLit n          -> jApp "CSP_" [str (show n), strLit (show n)]

  Suc t             -> jApp "Suc_" [oeval t]
  NatElim s z n     -> case ?stage of
                         0 -> jApp "natElim_" [oeval s, oeval z, oeval n]
                         _ -> jApp "NatElim_" [oeval s, oeval z, oeval n]

genTop :: Z.Tm Void -> IO Out
genTop t = do
  src <- do
    exec_path <- getDataFileName "rts.js"
    doesFileExist exec_path >>= \case
      -- True -> readFile exec_path
      _    -> readFile "rts.js"
  let ?cxt = []
  return $! str src <> newl <> newl <> execTop (runCConv t)
