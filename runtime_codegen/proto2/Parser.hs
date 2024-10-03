
module Parser (parseString, parseStdin) where

import Prelude hiding (pi)
import Control.Applicative hiding (many, some)
import Control.Monad
import Data.Char
import Data.Void
import Data.Foldable
import System.Exit
import Text.Megaparsec

import qualified Text.Megaparsec.Char       as C
import qualified Text.Megaparsec.Char.Lexer as L

import Common
import Presyntax

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

ident :: Parser Name
ident = try $ do
  x <- (:) <$> C.letterChar <*> takeWhileP Nothing (\c -> c == '\'' || isAlphaNum c)
  guard (not (isKeyword x))
  x <$ ws

keyword :: String -> Parser ()
keyword kw = do
  C.string kw
  (satisfy (\c -> c == '\'' || isAlphaNum c) *> empty) <|> ws

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
splice = (Splice <$> (char '~' *> splice)) <|> atom

arg :: Parser (Either Name Icit, Tm)
arg =   (try $ braces $ do {x <- ident; char '='; t <- tm; pure (Left x, t)})
    <|> ((Right Impl,) <$> (char '{' *> tm <* char '}'))
    <|> ((Right Expl,) <$> splice)

spine :: Parser Tm
spine =
  (Box <$> (char '□' *> splice))
  <|>
  (Box <$> (keyword "Code" *> splice))
  <|>
  (Eff <$> (keyword "Eff" *> splice))
  <|>
  (Return <$> (keyword "return" *> splice))
  <|>
  (Ref <$> (keyword "Ref" *> splice))
  <|>
  (New <$> (keyword "new" *> splice))
  <|>
  (Write <$> (keyword "write" *> splice) <*> splice)
  <|>
  (Read <$> (keyword "read" *> splice))
  <|>
  do h <- atom
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
  pure $ foldr' (\(x, i, ma) -> Lam x i ma) t xs

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
  pure $! foldr' (\(xs, a, i) t -> foldr' (\x -> Pi x i a) t xs) cod dom

funOrSpine :: Parser Tm
funOrSpine = do
  sp <- spine
  optional arrow >>= \case
    Nothing -> pure sp
    Just _  -> Pi "_" Expl sp <$> tm

pLet :: Parser Tm
pLet = do
  keyword "let"
  x <- ident
  ann <- optional (char ':' *> tm)
  char '='
  t <- tm
  char ';'
  u <- tm
  pure $ Let x (maybe Hole id ann) t u

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
  <|> funOrSpine)

src :: Parser Tm
src = ws *> tm <* eof

parseString :: String -> IO Tm
parseString str =
  case parse src "(stdin)" str of
    Left e -> do
      putStrLn $ errorBundlePretty e
      exitFailure
    Right t ->
      pure t

parseStdin :: IO (Tm, String)
parseStdin = do
  str <- getContents
  t <- parseString str
  pure (t, str)
