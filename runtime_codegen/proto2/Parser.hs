
module Parser (parseString, parseStdin) where

import Prelude hiding (pi)
import Control.Applicative hiding (many, some)
import Control.Monad
import Data.Char
import Data.Void
import Data.Foldable
import Data.Set (Set)
import System.Exit
import Text.Megaparsec
import Data.IORef

import qualified Data.Set as Set
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

natural :: Parser Integer
natural = lexeme $ do
  ds <- takeWhile1P Nothing isDigit
  pure $! foldl' (\acc d -> 10*acc + fromIntegral (digitToInt d)) 0 ds

keywords :: Set String
keywords = Set.fromList [
    "Code"
  , "Eff"
  , "Nat"
  , "NatElim"
  , "Ref"
  , "U"
  , "do"
  , "let"
  , "new"
  , "Rec"
  , "read"
  , "return"
  , "suc"
  , "write"
  , "zero"
  , "Σ"
  , "λ"
  , "ℕ"
  , "ℕElim"
  , "open"
  ]

isIdentRestChar :: Char -> Bool
isIdentRestChar c = c == '\'' || c == '-' || isAlphaNum c

ident :: Parser Name
ident = try $ do
  x <- (:) <$> C.letterChar <*> takeWhileP Nothing isIdentRestChar
  guard (Set.notMember x keywords)
  x <$ ws

keyword :: String -> Parser ()
keyword kw = try $ do
  C.string kw
  (satisfy isIdentRestChar *> empty) <|> ws

recTy :: Parser [(Name, Tm)]
recTy = do
  keyword "Σ" <|> keyword "Rec"
  char '('
  res <- sepBy ((,) <$> ident <*> (char ':' *> lamLet)) (char ',')
  char ')'
  pure res

atom :: Parser Tm
atom =
      withPos (    (Var     <$> ident)
               <|> (NatLit  <$> natural)
               <|> (U       <$  char 'U')
               <|> (Rec []  <$  try (char '(' *> char ')'))
               <|> (Box     <$  ((() <$ char '□') <|> keyword "Code"))
               <|> (Eff     <$  (keyword "Eff"    ))
               <|> (Return  <$  (keyword "return" ))
               <|> (Ref     <$  (keyword "Ref"    ))
               <|> (New     <$  (keyword "new"    ))
               <|> (Write   <$  (keyword "write"  ))
               <|> (Read    <$  (keyword "read"   ))
               <|> (Suc     <$  (keyword "suc"    ))
               <|> (NatElim <$  (keyword "ℕElim" <|> keyword "NatElim"))
               <|> (Nat     <$  (keyword "Nat" <|> keyword "ℕ"))
               <|> (Zero    <$  keyword "zero")
               <|> (Quote   <$> (char '<' *> tm <* char '>'))
               <|> (RecTy   <$> recTy)
               <|> (Hole    <$  char '_'))
  <|> parens tm

splice :: Parser Tm
splice =
      (do p <- getSourcePos
          Splice <$> (char '~' *> splice) <*> pure p)
  <|> atom

proj :: Parser Tm
proj = do
  let go t = do {char '.'; x <- ident; go (Proj t x)} <|> pure t
  go =<< splice

arg :: Parser (Either Name Icit, Tm)
arg =   (try $ braces $ do {x <- ident; char '='; t <- tm; pure (Left x, t)})
    <|> ((Right Impl,) <$> (char '{' *> tm <* char '}'))
    <|> ((Right Expl,) <$> (proj <|> lam <|> pLet <|> pDo))

spine :: Parser Tm
spine = do
  h <- proj
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
  t <- lamLet
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
  cod <- lamLet
  pure $! foldr' (\(!xs, !a, !i) t -> foldr' (\x -> Pi x i a) t xs) cod dom

apps :: Parser Tm
apps = do
  t <- spine
  (char '$' *> (App t <$> lamLet <*> pure (Right Expl))) <|> pure t

funOrApps :: Parser Tm
funOrApps = do
  sp <- apps
  optional arrow >>= \case
    Nothing -> pure sp
    Just _  -> Pi "_" Expl sp <$> lamLet

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

letArg :: Parser ([Name], Tm, Icit)
letArg =
      braces ((,,Impl) <$> some bind
                       <*> ((char ':' *> tm) <|> pure Hole))
  <|> parens ((,,Expl) <$> some bind
                       <*> (char ':' *> tm))
  <|> (do {x <- ident; pure ([x], Hole, Expl)})

pLet :: Parser Tm
pLet = do
  keyword "let"
  x <- ident
  args <- many letArg
  ann <- optional (char ':' *> lamLet)
  char '='
  t <- lamLet
  (t, ann) <- pure $ desugarIdentArgs args ann t
  char ';'
  u <- lamLet
  pure $ Let x ann t u

pDo :: Parser Tm
pDo = do
  keyword "do"
  optional (try (ident <* (symbol "<-" <|> symbol "←"))) >>= \case
    Nothing -> do
      t <- tm
      char ';'
      u <- lamLet
      pure $ Seq t u
    Just x  -> do
      t <- tm
      char ';'
      u <- lamLet
      pure $ Bind x t u

open :: Parser Tm
open = do
  keyword "open"
  t <- tm
  char ';'
  u <- lamLet
  pure $ Open t u

lamLet :: Parser Tm
lamLet = do
  withPos (
       lam
   <|> pLet
   <|> open
   <|> pDo
   <|> try pi
   <|> funOrApps)

recField :: Parser (Maybe Name, Tm)
recField = (,) <$> optional (try (ident <* char '=')) <*> lamLet

tm :: Parser Tm
tm = do
  optional (try (ident <* char '=')) >>= \case
    Nothing -> do
      t <- lamLet
      let mkRec = do char ','
                     rest <- sepBy recField (char ',')
                     pure (Rec ((Nothing, t):rest))
      mkRec <|> pure t
    Just x -> do
      t <- lamLet
      rest <- many (char ',' *> recField)
      pure $ Rec ((Just x, t):rest)

-- dbg :: String -> Parser ()
-- dbg msg = do
--   l <- lookAhead takeRest
--   traceM (msg ++ "|" ++ l)

topLet :: Parser Tm
topLet = do
  (x, args, ann) <- do
    x <- ident
    args <- many letArg
    ann  <- optional (char ':' *> lamLet)
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

topen :: Parser Tm
topen = do
  keyword "open"
  t <- tm
  char ';'
  u <- top
  pure $ Open t u

top :: Parser Tm
top = withPos (topLet <|> topDo <|> topen <|> tm)

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
