
module Parser (parseString, parseStdin) where

import Control.Applicative hiding (many, some)
import Data.Char
import Data.Void
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
  p >>= \case
    t@Pos{} -> pure t
    t       -> pure $ Pos (coerce pos) t

lexeme     = L.lexeme ws
symbol s   = lexeme (C.string s)
char c     = lexeme (C.char c)
parens p   = char '(' *> p <* char ')'
braces p   = char '{' *> p <* char '}'
pArrow     = symbol "→" <|> symbol "->"
pBind      = pIdent <|> symbol "_"

keyword :: String -> Bool
keyword x = x == "let"
         || x == "λ"
         || x == "U0"
         || x == "U1"
         || x == "Nat0"
         || x == "zero0"
         || x == "suc0"
         || x == "NatElim0"
         || x == "Nat1"
         || x == "zero1"
         || x == "suc1"
         || x == "NatElim1"

pIdent :: Parser Name
pIdent = try $ do
  o <- getOffset
  x <- takeWhile1P Nothing isAlphaNum
  if keyword x
    then do {setOffset o; fail $ "unexpected keyword: " ++ x}
    else x <$ ws

pKeyword :: String -> Parser ()
pKeyword kw = do
  C.string kw
  (takeWhile1P Nothing isAlphaNum *> empty) <|> ws

pAtom :: Parser Tm
pAtom  =
      parens pTm
  <|> withPos (    (U S0   <$  symbol "U0")
               <|> (U S1   <$  symbol "U1")
               <|> (Hole   <$  char '_')
               <|> (Quote  <$> (char '<' *> pTm <* char '>'))
               <|> (Splice <$> (char '[' *> pTm <* char ']'))
               <|> (Nat     S0 <$ (pKeyword "Nat0"    ))
               <|> (Zero    S0 <$ (pKeyword "zero0"   ))
               <|> (Suc     S0 <$ (pKeyword "suc0"    ))
               <|> (NatElim S0 <$ (pKeyword "NatElim0"))
               <|> (Nat     S1 <$ (pKeyword "Nat1"    ))
               <|> (Zero    S1 <$ (pKeyword "zero1"   ))
               <|> (Suc     S1 <$ (pKeyword "suc1"    ))
               <|> (NatElim S1 <$ (pKeyword "NatElim1"))
               <|> (Var <$> (takeWhile1P Nothing isAlphaNum <* ws)))


pArg :: Parser (Either Name Icit, Tm)
pArg =  (try $ braces $ do {x <- pIdent; char '='; t <- pTm; pure (Left x, t)})
    <|> ((Right Impl,) <$> (char '{' *> pTm <* char '}'))
    <|> ((Right Expl,) <$> pAtom)

pApps :: Parser Tm
pApps = do
  h <- pAtom
  args <- many pArg
  pure $ foldl (\t (i, u) -> App t u i) h args

pSpine :: Parser Tm
pSpine =
      (Lift <$> (char '^' *> pAtom))
  <|> pApps

pLamBinder :: Parser (Name, Maybe Tm, Either Name Icit)
pLamBinder =
      ((,Nothing,Right Expl) <$> pBind)
  <|> parens ((,,Right Expl) <$> pBind <*> optional (char ':' *> pTm))
  <|> try (braces ((,,Right Impl) <$> pBind <*> optional (char ':' *> pTm)))
  <|> braces (do {x <- pIdent; char '='; y <- pBind; pure (y, Nothing, Left x)})

pLam :: Parser Tm
pLam = do
  char 'λ' <|> char '\\'
  xs <- some pLamBinder
  char '.'
  t <- pTm
  pure $ foldr (\(x, ann, ni) t -> Lam x ann ni t) t xs

pPiBinder :: Parser ([Name], Tm, Icit)
pPiBinder =
      braces ((,,Impl) <$> some pBind
                       <*> ((char ':' *> pTm) <|> pure Hole))
  <|> parens ((,,Expl) <$> some pBind
                       <*> (char ':' *> pTm))
pPi :: Parser Tm
pPi = do
  dom <- some pPiBinder
  pArrow
  cod <- pTm
  pure $ foldr (\(xs, a, i) t -> foldr (\x -> Pi x i a) t xs) cod dom

pFunOrSpine :: Parser Tm
pFunOrSpine = do
  sp <- pSpine
  optional pArrow >>= \case
    Nothing -> pure sp
    Just _  -> Pi "_" Expl sp <$> pTm

pLet :: Parser Tm
pLet = do
  pKeyword "let"
  x   <- pIdent
  ann <- optional (try (char ':' *> notFollowedBy (char '=')) *> pTm)
  st  <- (S0 <$ symbol ":=") <|> (S1 <$ symbol "=")
  t   <- pTm
  symbol ";"
  u   <- pTm
  pure $ Let st x ann t u

pTm :: Parser Tm
pTm = withPos (pLam <|> pLet <|> try pPi <|> pFunOrSpine)

pSrc :: Parser Tm
pSrc = ws *> pTm <* eof

parseString :: String -> IO Tm
parseString src =
  case parse pSrc "(stdin)" src of
    Left e -> do
      putStrLn $ errorBundlePretty e
      exitFailure
    Right t ->
      pure t

parseStdin :: IO (Tm, String)
parseStdin = do
  src <- getContents
  t <- parseString src
  pure (t, src)
