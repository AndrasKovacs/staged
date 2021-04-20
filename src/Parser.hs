
module Parser (parse) where

import Prelude hiding (pi)
import Data.Foldable
import FlatParse.Stateful hiding (Parser, runParser, string, char, cut)
import qualified Data.ByteString as B


import Lexer
import Common hiding (U(..), CV(..))
import Presyntax

-- import Debug.Trace

--------------------------------------------------------------------------------

semi      = $(symbol ";")
colon     = $(symbol ":")
braceL    = $(symbol "{")
bracketR  = $(symbol "]")
comma     = $(symbol ",")
dot       = $(symbol ".")
eq        = $(symbol "=")
assign    = $(symbol ":=")
arrow     = token $(switch [| case _ of "->" -> pure (); "→" -> pure () |])
tilde     = $(symbol "~")
semi'     = $(symbol' ";")
colon'    = $(symbol' ":")
parR'     = $(symbol' ")")
braceR'   = $(symbol' "}")
bracketR' = $(symbol' "]")
dot'      = $(symbol' ".")
eq'       = $(symbol' "=")
assign'   = $(symbol' ":=")
angleR'   = $(symbol' ">")


--------------------------------------------------------------------------------

isKeyword :: Span -> Parser ()
isKeyword span = inSpan span do
  $(switch [| case _ of
     "let"  -> pure ()
     "λ"    -> pure ()
     "fix"  -> pure ()
     "case" -> pure ()
     "of"   -> pure ()
     "VTy"  -> pure ()
     "CTy"  -> pure ()
     "data" -> pure ()
     "MTy"  -> pure () |])
  eof

identBase :: Parser Span
identBase = spanned (identStartChar >> manyIdents) \_ x -> do
  fails (isKeyword x)
  ws
  pure x

ident :: Parser Span
ident = lvl >> identBase

ident' :: Parser Span
ident' = lvl' >> identBase `cut'` Msg "identifier"

-- Atomic expressions
--------------------------------------------------------------------------------

recOrRec :: Pos -> Parser Tm
recOrRec l =
  optioned (getPos <* bracketR)
    (\r -> pure (EmptyRec (Span l r)))
    (optioned ident
      (\x -> do
         lvl >> $(switch [| case _ of
           ":" -> ws >> restOfRec l x
           "=" -> ws >> restOfRecCon l x
           "," -> ws >> restOfTuple l (Var x)
           "]" -> do {r <- getPos; ws; pure (Tuple (Span l r) [Var x])}
           |]) `cut` [":", "=", ","])
      (do
         t <- tm'
         lvl >> $(switch [| case _ of
           "," -> ws >> restOfTuple l t
           "]" -> ws >> pure t
           |])) `cut` [",", "]"])

restOfRec :: Pos -> Span -> Parser Tm
restOfRec l x = do
  a <- tm'
  let field = (,) <$> (ident <* colon') <*> tm'
  fs <- many (comma *> field)
  r <- getPos
  bracketR'
  pure $ Rec (Span l r) ((x, a):fs)

restOfTuple :: Pos -> Tm ->  Parser Tm
restOfTuple l t = do
  ts <- (:) <$> tm' <*> many (comma *> tm')
  r  <- getPos
  bracketR'
  pure $ Tuple (Span l r) ts

restOfRecCon :: Pos -> Span ->  Parser Tm
restOfRecCon l x = do
  t <- tm'
  let assign = (,) <$> (ident <* eq') <*> tm'
  ts <- many (comma *> assign)
  r  <- getPos
  bracketR'
  pure $ RecCon (Span l r) ((x, t):ts)

skipToVar :: Pos -> (Pos -> Parser Tm) -> Parser Tm
skipToVar l k = branch identChar
  (do {manyIdents; r <- getPos; ws; pure $ Var (Span l r)})
  (do {r <- getPos; ws; k r})
{-# inline skipToVar #-}

up :: Pos -> Parser Tm
up l = do
  t <- tm'
  r <- getPos
  angleR'
  pure $ Up (Span l r) t

atomBase :: Parser Tm
atomBase = do
  -- !lvl <- ask
  l <- getPos
  $(switch [| case _ of
    "["    -> ws *> recOrRec l
    "<"    -> ws *> up l
    "("    -> ws *> tm' <* parR'
    "_"    -> ws >> pure (Hole l)
    "let"  -> skipToVar l \_ -> empty
    "λ"    -> skipToVar l \_ -> empty
    "of"   -> skipToVar l \_ -> empty
    "fix"  -> skipToVar l \_ -> empty
    "case" -> skipToVar l \_ -> empty
    "data" -> skipToVar l \_ -> empty
    "VTy"  -> skipToVar l \r -> pure $ Ty (Span l r) (U0 V)
    "CTy"  -> skipToVar l \r -> pure $ Ty (Span l r) (U0 C)
    "MTy"  -> skipToVar l \r -> pure $ Ty (Span l r) U1
    _      -> do {identChar; manyIdents; r <- getPos; ws; pure (Var (Span l r))} |])

atom :: Parser Tm
atom = lvl >> atomBase

atom' :: Parser Tm
atom' = lvl' >> atomBase `cut` [Msg "atomic expression"]

--------------------------------------------------------------------------------

goProj :: Parser Tm -> Parser Tm
goProj p = chainl Field p (dot *> ident')

proj :: Parser Tm
proj = goProj atom'


--------------------------------------------------------------------------------

goApp :: Tm -> Parser Tm
goApp t = branch braceL
  (do optioned (ident <* eq')
        (\x -> do
          u <- tm'
          braceR'
          goApp (App t u (Named x)))
        (do
          u <- tm'
          braceR'
          goApp (App t u (NoName Impl))))
  (do optioned atom
        (\u -> do
          u <- goProj (pure u)
          goApp (App t u (NoName Expl)))
        (pure t))

app' :: Parser Tm
app' = do
  pos <- getPos
  lvl' >> $(switch [| case _ of
    "~" -> do {ws; p <- proj; goApp (Down pos p)}
    "^" -> do {ws; p <- proj; goApp (Lift pos p)}
    _   -> do {goApp =<< proj} |])

--------------------------------------------------------------------------------

pi' :: Parser Tm
pi' = do
  l <- getPos
  lvl' >> $(switch [| case _ of

    "{" -> ws >>
      some ident >>= \(x:xs) -> do
      holepos <- getPos
      a <- optioned (colon *> tm') pure (pure (Hole holepos))
      braceR'
      optional_ arrow
      b <- pi'
      let !res = foldr' (\x@(Span x1 x2) -> Pi x1 (Bind x) Impl a) b xs
      pure $! Pi l (Bind x) Impl a res

    "(" -> ws >> do
      optioned (some ident <* colon)
        (\(x:xs) -> do
            a <- tm' <* parR'
            optional_ arrow
            b <- pi'
            let !res = foldr' (\x@(Span x1 x2) -> Pi x1 (Bind x) Expl a) b xs
            pure $! Pi l (Bind x) Expl a res)
        (do t <- tm' <* parR'
            branch arrow (Pi l DontBind Expl t <$> pi') (pure t))

    _   -> ws >> do
      t <- app'
      branch arrow (Pi l DontBind Expl t <$> pi') (pure t)
    |])

--------------------------------------------------------------------------------

bind :: Parser Bind
bind = branch $(symbol "_") (pure DontBind) (Bind <$> identBase)

bind' :: Parser Bind
bind' = branch $(symbol' "_") (pure DontBind) (Bind <$> identBase)
        `cut'` Msg "binder"

goLam :: Parser Tm
goLam = do
  pos <- getPos
  lvl' >> $(switch [| case _ of

    "{" -> ws >> do
      branch $(symbol "_")
        (Lam pos DontBind (NoName Impl) <$> optional (colon *> tm') <*> (braceR' *> goLam))
        (do x <- ident'
            branch eq
              (do y <- ident'
                  Lam pos (Bind y) (Named x) Nothing <$> (braceR' *> goLam))
              (Lam pos (Bind x) (NoName Impl) <$> optional (colon *> tm') <*> (braceR' *> goLam)))

    "(" -> ws >> do
      x <- ident'
      a <- colon' *> tm' <* parR'
      Lam pos (Bind x) (NoName Expl) (Just a) <$> goLam

    "." -> ws >> tm'
    "_" -> ws >> Lam pos DontBind (NoName Expl) Nothing <$> goLam

    _ -> ws >> do
      -- !_ <- ask
      x <- lvl' >> identBase `cut` [Msg "binder", "."]
      Lam pos (Bind x) (NoName Expl) Nothing <$> goLam
      |])

lam' :: Pos -> Parser Tm
lam' pos = lvl' >> $(switch [| case _ of

  "{" -> ws >>
    branch $(symbol "_")
      (Lam pos DontBind (NoName Impl) <$> optional (colon *> tm') <*> (braceR' *> goLam))
      (do x <- ident'
          branch eq
            (do y <- ident'
                Lam pos (Bind y) (Named x) Nothing <$> (braceR' *> goLam))
            (Lam pos (Bind x) (NoName Impl) <$> optional (colon *> tm') <*> (braceR' *> goLam)))

  "(" -> ws >> do
    x <- ident'
    a <- colon' *> tm' <* parR'
    Lam pos (Bind x) (NoName Expl) (Just a) <$> goLam

  "_" -> ws >> Lam pos DontBind (NoName Expl) Nothing <$> goLam
  _ -> ws >> do
    x <- lvl' >> identBase `cut` [Msg "binder", "."]
    Lam pos (Bind x) (NoName Expl) Nothing <$> goLam
    |])

--------------------------------------------------------------------------------

pLet' :: Pos -> Parser Tm
pLet' l = do
  x <- ident'
  a <- optional (colon `notFollowedBy` eq *> tm')
  $(switch [| case _ of
    "=" -> ws >> do
      t <- tm'
      semi'
      u <- tm'
      pure $ Let1 l x a t u
    ":=" -> ws >> do
      t <- tm'
      semi'
      u <- tm'
      pure $ Let0 l x a t u |]) `cut` ["=", ":="]

clause :: Parser (Span, [Bind], Tm)
clause = do
  con <- ident
  bs  <- many bind
  dot'
  rhs <- tm'
  pure $ (con, bs, rhs)

pCase' :: Pos -> Parser Tm
pCase' l = do
  t <- tm'
  $(keyword' "of")
  r <- getPos
  optioned clause
   (\cl -> Case l t r . (cl:) <$> many (semi *> clause))
   (pure $ Case l t r [])

fix' :: Pos -> Parser Tm
fix' l = Fix l <$> bind' <*> bind' <*> (dot' *> tm')

skipToApp :: Pos -> (Pos -> Parser Tm) -> Parser Tm
skipToApp l p = branch identChar
  (do {manyIdents; r <- getPos; ws; goApp =<< goProj (pure (Var (Span l r)))})
  (do {r <- getPos; ws; p r})
{-# inline skipToApp #-}

tmBase :: Parser Tm
tmBase = do
  l <- getPos
  $(switch [| case _ of
    "λ"    -> skipToApp l \_ -> lam' l
    "\\"   -> ws >> lam' l
    "let"  -> skipToApp l \_ -> pLet' l
    "case" -> skipToApp l \_ -> pCase' l
    "fix"  -> skipToApp l \_ -> fix' l
    _      -> ws >> pi' |])
{-# inline tmBase #-}

tm' :: Parser Tm
tm' = lvl' >> tmBase `cut` [Msg "lambda expression", "let", "case", "fix"]

--------------------------------------------------------------------------------

topDef :: Span -> Parser TopLevel
topDef x = local (const 1) do
  a <- optional (colon `notFollowedBy` eq *> tm')
  $(switch [| case _ of
    "=" -> ws >> do
      rhs <- tm'
      local (const 0) (Definition1 x a rhs <$> top)
    ":=" -> ws >> do
      rhs <- tm'
      local (const 0) (Definition0 x a rhs <$> top)|])
    `cut` ["=", ":="]

sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy pa psep = ((:) <$!> pa <*!> many (psep *> pa)) <|> pure []
{-# inline sepBy #-}

dataDecl :: Parser TopLevel
dataDecl = do
  pos <- getPos
  $(keyword "data")
  local (const 1) do
    tycon  <- ident'
    params <- many ((,) <$> ident <*> optional (colon *> tm'))
    eq'
    datacons <- sepBy ((,) <$> ident' <*> many atom) $(symbol "|")
    local (const 0) (DataDecl pos tycon params datacons <$!> top)

top :: Parser TopLevel
top =  (exactLvl 0 >> (ident >>= topDef))
   <|> (exactLvl 0 >> dataDecl)
   <|> (Nil <$ eof)

--------------------------------------------------------------------------------

src :: Parser TopLevel
src = ws *> top

--------------------------------------------------------------------------------

parse :: B.ByteString -> Result Error TopLevel
parse = runParser src

--------------------------------------------------------------------------------

p1 = unlines [

  "data List A = Nil | Cons A (List A)",

  "map : {A B : VTy} -> (A -> B) -> List A -> List B",
  " = λ f xs. xs"
  ]
