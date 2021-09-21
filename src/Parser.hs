
module Parser (parse, parseString, Result(..)) where

import Prelude hiding (pi)
import Data.Foldable
import FlatParse.Stateful hiding (Parser, runParser, string, cut)
import qualified Data.ByteString as B
import Data.Char (ord)

import Lexer
import Common
import Presyntax

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
     "case" -> pure ()
     "of"   -> pure ()
     "CV"   -> pure ()
     "data" -> pure ()
     "Val"  -> pure ()
     "Comp" -> pure ()
     "U0"   -> pure ()
     "U1"   -> pure ()
     "Int"  -> pure ()
     |])
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

-- TODO: clean this up
recOrRec :: Pos -> Parser Tm
recOrRec l =
  optioned (getPos <* bracketR)
    (\r -> pure (EmptyRec (Span l r)))
    (optioned (ident <* $(symbol ":"))
      (restOfRec l)
      (optioned (ident <* $(symbol "="))
        (restOfRecCon l)
        (optioned (ident <* $(symbol ","))
          (\x -> restOfTuple l (Var x))
          (optioned (ident <* $(symbol "]"))
            (\x -> do {r <- getPos; ws; pure (Tuple (Span l r) [Var x])})
            (do t <- tm'
                lvl >> $(switch [| case _ of
                  "," -> ws >> restOfTuple l t
                  "]" -> ws >> pure t
                  |])) `cut` [",", "]"]))))

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
  pure $ Tuple (Span l r) (t:ts)

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

-- todo: speed up
intLit :: Parser (Span, Int)
intLit = do
  l      <- getPos
  sign   <- ((-1) <$ $(char '-')) <|> pure 1
  digits <- chainl
    (flip (:)) ((:[]) <$!> satisfyASCII isDigit) (satisfyASCII isDigit)
  r <- getPos
  ws
  let go place acc (d:ds) = go (place*10) (acc + (ord d - ord '0')*place) ds
      go place acc []     = acc
  let n = sign * go 1 0 digits
  pure (Span l r, n)

atomBase :: Parser Tm
atomBase = do
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
    "Val"  -> skipToVar l \r -> pure $ Val (Span l r)
    "Comp" -> skipToVar l \r -> pure $ Comp (Span l r)
    "CV"   -> skipToVar l \r -> pure $ CV (Span l r)
    "U0"   -> skipToVar l \r -> empty
    "U1"   -> skipToVar l \r -> pure $ U1 (Span l r)
    "Int"  -> skipToVar l \r -> pure $ Int (Span l r)
    _      ->     do {identStartChar; manyIdents; r <- getPos; ws; pure (Var (Span l r))}
              <|> (uncurry IntLit <$!> intLit)
            |])

atom :: Parser Tm
atom = lvl >> atomBase

atom' :: Parser Tm
atom' = lvl' >> atomBase `cut` [Msg "atomic expression"]

--------------------------------------------------------------------------------

down' :: Parser Tm
down' = do
  pos <- getPos
  $(switch [| case _ of
    "~" -> do {ws; Down pos <$!> atom'}
    _   -> atom' |])

down :: Parser Tm
down = do
  pos <- getPos
  $(switch [| case _ of
    "~" -> do {ws; Down pos <$!> atom}
    _   -> atom |])

--------------------------------------------------------------------------------

-- goField :: Tm -> Either Span -> Tm
-- goField t x = _

-- goProj :: Parser Tm -> Parser Tm
-- goProj p = chainl  p (dot *> ident')

goProj :: Tm -> Parser Tm
goProj t =
  branch dot
    (do {(x, n) <- intLit; goProj (Field t (ProjIx x n))} <|>
     do {x <- ident'; goProj (Field t (ProjName x))})
    (pure t)

proj :: Parser Tm
proj = goProj =<< down'

--------------------------------------------------------------------------------

goApp :: Tm -> Parser Tm
goApp t = branch braceL
  (do optioned (ident <* eq)
        (\x -> do
          u <- tm'
          braceR'
          goApp (App t u (Named x)))
        (do
          u <- tm'
          braceR'
          goApp (App t u (NoName Impl))))
  (do optioned down
        (\u -> do
          u <- goProj u
          goApp (App t u (NoName Expl)))
        (pure t))

app' :: Parser Tm
app' = do
  pos <- getPos
  lvl' >> $(switch [| case _ of
    "^"  -> do {ws; p <- proj; goApp (Lift pos p)}
    "U0" -> do {ws; p <- proj; goApp (U0 pos p)}
    _    -> do {goApp =<< proj} |])

--------------------------------------------------------------------------------

mul' :: Parser Tm
mul' = chainl Mul app' ($(symbol "*") *> app')

add' :: Parser Tm
add' = chainl Add mul' ($(symbol "+") *> mul')

sub' :: Parser Tm
sub' = chainl Sub add' ($(symbol "-") *> add')

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
            branch arrow
              (Pi l DontBind Expl t <$> pi')
              (goProj t))

    _   -> ws >> do
      t <- sub'
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

skipToApp :: Pos -> (Pos -> Parser Tm) -> Parser Tm
skipToApp l p = branch identChar
  (do {manyIdents; r <- getPos; ws; goApp =<< goProj (Var (Span l r))})
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
    _      -> ws >> pi' |])
{-# inline tmBase #-}

tm' :: Parser Tm
tm' = lvl' >> tmBase `cut` [Msg "lambda expression", "let"]

--------------------------------------------------------------------------------

-- TODO: fix shitty parse errors for top entries

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
   <|> (Nil <$ eof `cut` [Msg "end of input"])

--------------------------------------------------------------------------------

src :: Parser TopLevel
src = ws *> top

--------------------------------------------------------------------------------

parse :: B.ByteString -> Result Error TopLevel
parse = runParser src

parseString :: String -> (RawName, Result Error TopLevel)
parseString (packUTF8 -> str) = (coerce str, parse str)
{-# inline parseString #-}

--------------------------------------------------------------------------------

p1 = unlines [
  "foo = λ ma s. (ma s).fst"
  -- "bar = λ x. <f ~x>   ",
  -- "  "
  -- "bar : U1 = U0 Comp"
  -- "foo : Int = 300 + 500 * 10"
  -- "bar = let x = Comp; let foo := mallc; U0 x"
  -- "f = [t u, g x]"
  -- "id = foo{bar}"
  -- "id : MTy = {A : CTy} → A → A",
  -- "= λ x. x"

  -- "data List A = Nil | Cons A (List A)",

  -- "map : {A B : VTy} -> (A -> B) -> List A -> List B",
  -- " = λ f xs. xs"
  ]
