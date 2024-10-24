
module Parser (parseString, parseStdin) where

import Prelude hiding (pi)
import Control.Applicative hiding (many, some)
import Control.Monad
import Data.Char
import Data.Void
import Data.Foldable
import System.Exit
import Text.Megaparsec
import Data.IORef

import qualified Text.Megaparsec.Char       as C
import qualified Text.Megaparsec.Char.Lexer as L

import Common
import Presyntax
import ElabState

--------------------------------------------------------------------------------

type Parser = Parsec Void String

ws :: Parser ()
ws = L.space C.space1 (L.skipLineComment "--") (L.skipBlockComment "{-" "-}")

withPos :: Parser Tm -> Parser Tm
withPos p = do
  pos <- getSourcePos
  a <- p
  case a of
    SrcPos{} -> pure a
    _        -> pure $ SrcPos pos a

lexeme     = L.lexeme ws
symbol s   = lexeme (C.string s)
char c     = lexeme (C.char c)
parens p   = char '(' *> p <* char ')'
braces p   = char '{' *> p <* char '}'
arrow      = symbol "→" <|> symbol "->"
bind       = ident <|> symbol "_"

isKeyword :: String -> Bool
isKeyword x =
     x == "let" || x == "λ" || x == "U"
  || x == "return" || x == "do" || x == "Code"
  || x == "Eff" || x == "Top" || x == "tt"
  || x == "Ref" || x == "new" || x == "read" || x == "write"

isIdentRestChar :: Char -> Bool
isIdentRestChar c = c == '\'' || c == '-' || isAlphaNum c

ident :: Parser Name
ident = try $ do
  x <- (:) <$> C.letterChar <*> takeWhileP Nothing isIdentRestChar
  guard (not (isKeyword x))
  x <$ ws

keyword :: String -> Parser ()
keyword kw = do
  C.string kw
  (satisfy isIdentRestChar *> empty) <|> ws

atom :: Parser Tm
atom =
      withPos (    (Var    <$> ident)
               <|> (U      <$  char 'U')
               <|> (Unit   <$  char '⊤')
               <|> (Unit   <$  keyword "Top")
               <|> (Tt     <$  keyword "tt")
               <|> (Quote  <$> (char '<' *> tm <* char '>'))
               <|> (Hole   <$  char '_'))
  <|> parens tm

splice :: Parser Tm
splice =
      (do p <- getSourcePos
          Splice <$> (char '~' *> splice) <*> pure p)
  <|> atom

explArg :: Parser Tm
explArg = splice <|> lam

arg :: Parser (Either Name Icit, Tm)
arg =   (try $ braces $ do {x <- ident; char '='; t <- tm; pure (Left x, t)})
    <|> ((Right Impl,) <$> (char '{' *> tm <* char '}'))
    <|> ((Right Expl,) <$> explArg)

spine :: Parser Tm
spine =
  (Box <$> (char '□' *> splice))
  <|>
  (Box <$> (keyword "Code" *> splice))
  <|>
  (Eff <$> (keyword "Eff" *> splice))
  <|>
  (Return <$> (keyword "return" *> explArg))
  <|>
  (Ref <$> (keyword "Ref" *> splice))
  <|>
  (New <$> (keyword "new" *> explArg))
  <|>
  (Write <$> (keyword "write" *> splice) <*> explArg)
  <|>
  (Read <$> (keyword "read" *> explArg))
  <|>
  do h <- splice
     args <- many arg
     pure $ foldl' (\t (i, u) -> App t u i) h args

lamBinder :: Parser (Name, Either Name Icit, Maybe Tm)
lamBinder =
      parens ((,Right Expl,) <$> bind <*> optional (char ':' *> tm))
  <|> try (braces ((,Right Impl,) <$> bind <*> optional (char ':' *> tm)))
  <|> braces (do {x <- ident; char '='; y <- bind; pure (y, Left x, Nothing)})
  <|> ((,Right Expl,Nothing) <$> bind)

lam :: Parser Tm
lam = do
  char 'λ' <|> char '\\'
  xs <- some lamBinder
  char '.'
  t <- tm
  pure $ foldr' (\(!x, !i, !ma) -> Lam x i ma) t xs

piBinder :: Parser ([Name], Tm, Icit)
piBinder =
      braces ((,,Impl) <$> some bind
                       <*> ((char ':' *> tm) <|> pure Hole))
  <|> parens ((,,Expl) <$> some bind
                       <*> (char ':' *> tm))
pi :: Parser Tm
pi = do
  dom <- some piBinder
  arrow
  cod <- tm
  pure $! foldr' (\(!xs, !a, !i) t -> foldr' (\x -> Pi x i a) t xs) cod dom

apps :: Parser Tm
apps = do
  t <- spine
  (char '$' *> (App t <$> apps <*> pure (Right Expl))) <|> pure t

funOrApps :: Parser Tm
funOrApps = do
  sp <- apps
  optional arrow >>= \case
    Nothing -> pure sp
    Just _  -> Pi "_" Expl sp <$> tm

-- | Desugar Coq-style definition arguments.
desugarIdentArgs :: [([Name], Tm, Icit)] -> Maybe Tm -> Tm -> (Tm, Maybe Tm)
desugarIdentArgs args mty rhs = case mty of

  -- if there's no return type annotation, we desugar to annotated lambdas
  Nothing -> let
    tm = foldr' (\(xs, a, i) t -> foldr' (\x -> Lam x (Right i) (Just a)) t xs) rhs args
    in (tm, Nothing)

  -- otherwise we pull out the iterated Pi type to get an annotation on a "let".
  Just a  -> let
    tm = foldr' (\(xs, a, i) t -> foldr' (\x -> Lam x (Right i) Nothing) t xs) rhs args
    ty = foldr' (\(xs, a, i) b -> foldr' (\x -> Pi x i a) b xs) a args
    in (tm, Just ty)

pLet :: Parser Tm
pLet = do
  keyword "let"
  x <- ident
  args <- many piBinder
  ann <- optional (char ':' *> tm)
  char '='
  t <- tm
  (t, ann) <- pure $ desugarIdentArgs args ann t
  char ';'
  u <- tm
  pure $ Let x ann t u

pDo :: Parser Tm
pDo = do
  keyword "do"
  optional (try (ident <* (symbol "<-" <|> symbol "←"))) >>= \case
    Nothing -> do
      t <- tm
      char ';'
      u <- tm
      pure $ Seq t u
    Just x  -> do
      t <- tm
      char ';'
      u <- tm
      pure $ Bind x t u

tm :: Parser Tm
tm = withPos (
      lam
  <|> pLet
  <|> pDo
  <|> try pi
  <|> funOrApps)

topLet :: Parser Tm
topLet = do
  (x, args, ann) <- try do
    x <- ident
    args <- many piBinder
    ann <- optional (char ':' *> tm)
    char '='
    pure (x, args, ann)
  t <- tm
  (t, ann) <- pure $ desugarIdentArgs args ann t
  char ';'
  u <- top
  pure $ Let x ann t u

topDo :: Parser Tm
topDo = do
  keyword "do"
  optional (try (ident <* (symbol "<-" <|> symbol "←"))) >>= \case
    Nothing -> do
      t <- tm
      char ';'
      u <- top
      pure $ Seq t u
    Just x  -> do
      t <- tm
      char ';'
      u <- top
      pure $ Bind x t u

top :: Parser Tm
top = withPos (topLet <|> topDo <|> tm)

src :: Parser Tm
src = ws *> top <* eof

parseString :: String -> IO Tm
parseString str =
  case parse src "(stdin)" str of
    Left e -> do
      putStrLn $ errorBundlePretty e
      exitFailure
    Right t -> do
      writeIORef sourceCode str
      pure t

parseStdin :: IO (Tm, String)
parseStdin = do
  str <- getContents
  t <- parseString str
  pure (t, str)
