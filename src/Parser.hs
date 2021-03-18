
module Parser where

import Prelude hiding (pi)
import Data.Foldable
import FlatParse.Stateful hiding (Parser, runParser, string, char, cut)

import Lexer
import Presyntax
import Common

import Debug.Trace
--------------------------------------------------------------------------------

semi        = $(symbol ";")
colon       = $(symbol ":")
parL        = $(symbol "(")
parR        = $(symbol ")")
braceL      = $(symbol "{")
braceR      = $(symbol "}")
bracketL    = $(symbol "[")
bracketR    = $(symbol "]")
comma       = $(symbol ",")
dot         = $(symbol ".")
eq          = $(symbol "=")
arrow       = token $(switch [| case _ of "->" -> pure (); "→" -> pure () |])
tilde       = $(symbol "~")
angleL      = $(symbol "<")
angleR      = $(symbol ">")

cutSemi     = $(cutSymbol ";")
cutColon    = $(cutSymbol ":")
cutParL     = $(cutSymbol "(")
cutParR     = $(cutSymbol ")")
cutBraceL   = $(cutSymbol "{")
cutBraceR   = $(cutSymbol "}")
cutBracketL = $(cutSymbol "[")
cutBracketR = $(cutSymbol "]")
cutComma    = $(cutSymbol ",")
cutDot      = $(cutSymbol ".")
cutEq       = $(cutSymbol "=")
cutArrow    = arrow `cut'` Msg "\"->\" or \"→\""
cutTilde    = $(cutSymbol "~")
cutAngleL   = $(cutSymbol "<")
cutAngleR   = $(cutSymbol ">")

--------------------------------------------------------------------------------

isKeyword :: Span -> Parser ()
isKeyword span = inSpan span do
  $(switch [| case _ of
     "let"  -> pure ()
     "λ"    -> pure ()
     "fix"  -> pure ()
     "case" -> pure ()
     "VTy"  -> pure ()
     "CTy"  -> pure ()
     "MTy"  -> pure () |])
  eof

ident :: Parser Span
ident = token $
  spanned (identStartChar >> manyIdents) \_ x ->
  x <$ fails (isKeyword x)

cutIdent :: Parser Span
cutIdent = ident `cut` [Msg "identifier"]

-- Atomic expressions
--------------------------------------------------------------------------------

recOrRec :: Pos -> Parser Tm
recOrRec l =
  optioned (getPos <* bracketR)
    (\r -> pure (EmptyRec (Span l r)))
    (optioned ident
      (\x -> do
         guardLvl >> $(switch [| case _ of
           ":" -> ws >> restOfRec l x
           "=" -> ws >> restOfRecCon l x
           "," -> ws >> restOfTuple l (Var x)
           |]) `cut` [":", "=", ","])
      (do
         t <- tm
         guardLvl >> $(switch [| case _ of
           "," -> ws >> restOfTuple l t
           "]" -> ws >> pure t
           _   -> undefined |])))

restOfRec :: Pos -> Span -> Parser Tm
restOfRec l x = do
  a <- tm
  let field = (,) <$> (ident <* cutColon) <*> tm
  fs <- many (comma *> field)
  r <- getPos
  cutBracketR
  pure $ Rec (Span l r) ((x, a):fs)

restOfTuple :: Pos -> Tm ->  Parser Tm
restOfTuple l t = do
  ts <- (:) <$> tm <*> many (comma *> tm)
  r  <- getPos
  cutBracketR
  pure $ Tuple (Span l r) ts

restOfRecCon :: Pos -> Span ->  Parser Tm
restOfRecCon l x = do
  t <- tm
  let assign = (,) <$> (ident <* cutEq) <*> tm
  ts <- many (comma *> assign)
  r  <- getPos
  cutBracketR
  pure $ RecCon (Span l r) ((x, t):ts)

skipToVar :: Pos -> (Pos -> Parser Tm) -> Parser Tm
skipToVar l k = branch identChar
  (do {manyIdents; r <- getPos; ws; pure $ Var (Span l r)})
  (do {r <- getPos; ws; k r})
{-# inline skipToVar #-}

up :: Pos -> Parser Tm
up l = do
  t <- tm
  r <- getPos
  cutAngleR
  pure $ Up (Span l r) t

atom :: Parser Tm
atom = do
  l <- getPos
  guardLvl >> $(switch [| case _ of
    "["    -> ws *> recOrRec l
    "<"    -> ws *> up l
    "("    -> ws *> tm <* cutParR
    "_"    -> ws >> pure (Hole l)
    "let"  -> skipToVar l \_ -> empty
    "λ"    -> skipToVar l \_ -> empty
    "fix"  -> skipToVar l \_ -> empty
    "case" -> skipToVar l \_ -> empty
    "VTy"  -> skipToVar l \r -> pure $ Ty (Span l r) UVal
    "CTy"  -> skipToVar l \r -> pure $ Ty (Span l r) UComp
    "MTy"  -> skipToVar l \r -> pure $ Ty (Span l r) UMeta
    _      -> do {identChar; manyIdents; r <- getPos; ws; pure (Var (Span l r))} |])

cutAtom :: Parser Tm
cutAtom = atom `cut` [Msg "atomic expression"]

--------------------------------------------------------------------------------

goProj :: Parser Tm -> Parser Tm
goProj p = chainl Field p (dot *> cutIdent)

proj :: Parser Tm
proj = goProj cutAtom

--------------------------------------------------------------------------------

goApp :: Tm -> Parser Tm
goApp t = branch braceL
  (do optioned (ident <* eq)
        (\x -> do
          u <- tm
          cutBraceR
          goApp (App t u (Named x)))
        (do
          u <- tm
          cutBraceR
          goApp (App t u (NoName Impl))))
  (do optioned atom
        (\u -> do
          u <- goProj (pure u)
          goApp (App t u (NoName Expl)))
        (pure t))

app :: Parser Tm
app =
  optioned (getPos <* tilde)
    (\pos -> goApp =<< (Down pos <$> proj))
    (goApp =<< proj)

--------------------------------------------------------------------------------

pi :: Parser Tm
pi = do
  l <- getPos
  guardLvl >> $(switch [| case _ of

    "{" -> ws >> do
      some ident >>= \(x:xs) -> do
      holepos <- getPos
      a <- optioned (colon *> tm) pure (pure (Hole holepos))
      cutBraceR
      optional_ arrow
      b <- pi
      let !res = foldr' (\x@(Span x1 x2) -> Pi x1 (Bind x) Impl a) b xs
      pure $! Pi l (Bind x) Impl a res

    "(" -> ws >> do
      optioned (some ident <* colon)
        (\(x:xs) -> do
            a <- tm <* cutParR
            optional_ arrow
            b <- pi
            let !res = foldr' (\x@(Span x1 x2) -> Pi x1 (Bind x) Expl a) b xs
            pure $! Pi l (Bind x) Expl a res)
        (do t <- tm <* cutParR
            branch arrow (Pi l DontBind Expl t <$> pi) (pure t))

    _   -> ws >> do
      t <- app
      branch arrow (Pi l DontBind Expl t <$> pi) (pure t)
    |])

--------------------------------------------------------------------------------

bind :: Parser Bind
bind = branch $(symbol "_") (pure DontBind) (Bind <$> ident)

cutBind :: Parser Bind
cutBind = bind `cut'` Msg "binder"

lam' :: Parser Tm
lam' = do
  pos <- getPos
  guardLvl >> $(switch [| case _ of

    "{" -> ws >>
      branch $(symbol "_")
        (Lam pos DontBind (NoName Impl) <$> optional (colon *> tm) <*> (cutBraceR *> lam'))
        (do x <- cutIdent
            branch eq
              (do y <- cutIdent
                  Lam pos (Bind y) (Named x) Nothing <$> (cutBraceR *> lam'))
              (Lam pos (Bind x) (NoName Impl) <$> optional (colon *> tm) <*> (cutBraceR *> lam')))

    "(" -> ws >> do
      x <- cutIdent
      a <- cutColon *> tm <* cutParR
      Lam pos (Bind x) (NoName Expl) (Just a) <$> lam'

    "." -> ws >> tm

    _ -> ws >> do
      x <- cutBind
      Lam pos x (NoName Expl) Nothing <$> lam'
      |])

lam :: Pos -> Parser Tm
lam pos = guardLvl >> $(switch [| case _ of

  "{" -> ws >>
    branch $(symbol "_")
      (Lam pos DontBind (NoName Impl) <$> optional (colon *> tm) <*> (cutBraceR *> lam'))
      (do x <- cutIdent
          branch eq
            (do y <- cutIdent
                Lam pos (Bind y) (Named x) Nothing <$> (cutBraceR *> lam'))
            (Lam pos (Bind x) (NoName Impl) <$> optional (colon *> tm) <*> (cutBraceR *> lam')))

  "(" -> ws >> do
    x <- cutIdent
    a <- cutColon *> tm <* cutParR
    Lam pos (Bind x) (NoName Expl) (Just a) <$> lam'

  _ -> ws >> do
    x <- cutBind
    Lam pos x (NoName Expl) Nothing <$> lam'
    |])


--------------------------------------------------------------------------------

pLet :: Pos -> Parser Tm
pLet l = do
  x <- cutIdent
  a <- optional (colon *> tm)
  cutEq
  t <- tm
  cutSemi
  u <- tm
  pure $ Let l x a t u

--------------------------------------------------------------------------------

skipToApp :: Pos -> (Pos -> Parser Tm) -> Parser Tm
skipToApp l p = branch identChar
  (do {manyIdents; r <- getPos; ws; goApp =<< goProj (pure (Var (Span l r)))})
  (do {r <- getPos; ws; p r})
{-# inline skipToApp #-}

tm :: Parser Tm
tm = do
  l <- getPos
  guardLvl >> $(switch [| case _ of
    "λ"   -> skipToApp l \_ -> lam l
    "\\"  -> ws >> lam l
    "let" -> skipToApp l \_ -> pLet l
    _     -> ws >> pi |])

src :: Parser Tm
src = ws *> tm <* eof
