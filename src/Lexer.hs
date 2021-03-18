

module Lexer where

import FlatParse.Stateful hiding (Parser, runParser, string, char, cut, err)

import qualified FlatParse.Stateful as FP
import qualified Data.ByteString as B
import Language.Haskell.TH

import qualified Data.Set as S
import Data.Char
import Data.String

-- import Debug.Trace

--------------------------------------------------------------------------------

-- | An expected item which is displayed in error messages.
data Expected
  = Lit String  -- ^ An expected literal string.
  | Msg String  -- ^ A description of what's expected.
  deriving (Eq, Show, Ord)

instance IsString Expected where fromString = Lit

-- | A parsing error, without source position.
data Error'
  = Precise Expected     -- ^ A precisely known error, like leaving out "in" from "let".
  | ExactIndent Int      -- ^ Expected indentation exactly the given `Int`.
  | IndentMore  Int      -- ^ Expected indentation at least the given `Int`.
  | Imprecise [Expected] -- ^ An imprecise error, when we expect a number of different things,
                         --   but parse something else.
  deriving Show

-- | A source-annotated error.
data Error = Error !Pos !Error'
  deriving Show

-- | Merge two errors. Imprecise errors are merged by appending lists of expected items.  If we have
--   a precise and an imprecise error, we throw away the imprecise one. If we have two precise
--   errors, we choose the left one, which is by convention the one throw by an inner parser.
--
--   The point of prioritizing inner and precise errors is to suppress the deluge of "expected"
--   items, and instead try to point to a concrete issue to fix.
merge :: Error -> Error -> Error
merge err@(Error p e) err'@(Error p' e') = case (e, e') of
  (ExactIndent _, _)            -> err
  (_, ExactIndent _)            -> err'
  (IndentMore _, _)             -> err
  (_, IndentMore _)             -> err'
  (Precise _, _)                -> err
  (_, Precise _)                -> err'
  (Imprecise ss, Imprecise ss') -> Error p (Imprecise (ss ++ ss'))
   -- note: we never recover from errors, so all merged errors will in fact have exactly the same
   -- Pos. So we can simply throw away one of the two here.
{-# noinline merge #-} -- merge is "cold" code, so we shouldn't inline it.

type Parser = FP.Parser Int Error

-- | Pretty print an error. The `B.ByteString` input is the source file. The offending line from the
--   source is displayed in the output.
prettyError :: B.ByteString -> Error -> String
prettyError b (Error pos e) =

  let ls       = FP.lines b
      [(l, c)] = posLineCols b [pos]
      line     = if l < length ls then ls !! l else ""
      linum    = show l
      lpad     = map (const ' ') linum

      expected (Lit s) = show s
      expected (Msg s) = s

      err (Precise exp)     = "expected " ++ expected exp
      err (Imprecise exps)  = "expected " ++ (imprec $ S.toList $ S.fromList exps)
      err (IndentMore col)  = "expected token indented to column " ++ show col ++ " or more"
      err (ExactIndent col) = "expected token indented to column " ++ show col

      imprec :: [Expected] -> String
      imprec []     = error "impossible"
      imprec [s]    = expected s
      imprec (s:ss) = expected s ++ go ss where
        go []     = ""
        go [s]    = " or " ++ expected s
        go (s:ss) = ", " ++ expected s ++ go ss

  in show l ++ ":" ++ show c ++ ":\n" ++
     lpad   ++ "|\n" ++
     linum  ++ "| " ++ line ++ "\n" ++
     lpad   ++ "| " ++ replicate c ' ' ++ "^\n" ++
     "parse error: " ++
     err e

-- | Throw an error.
err :: Error' -> Parser a
err err = do
  p <- getPos
  FP.err $ Error p err

-- | Imprecise cut: we slap a list of expected things on inner errors.
cut :: Parser a -> [Expected] -> Parser a
cut p exps = do
  pos <- getPos
  FP.cutting p (Error pos (Imprecise exps)) merge

-- | Precise cut: we propagate at most a single expected thing.
cut' :: Parser a -> Expected -> Parser a
cut' p exp = do
  pos <- getPos
  FP.cutting p (Error pos (Precise exp)) merge

runParser :: Parser a -> B.ByteString -> Result Error a
runParser p = FP.runParser p 0 0

-- | Run parser, print pretty error on failure.
testParser :: Show a => Parser a -> String -> IO ()
testParser p str = case packUTF8 str of
  b -> case runParser p b of
    Err e    -> putStrLn $ prettyError b e
    OK a _ _ -> print a
    Fail     -> putStrLn "uncaught parse error"

-- | Consume whitespace. We track the number of whitespace characters read since the start of the
--   current line. We don't need to track column numbers precisely! Relevant indentation consists of
--   only whitespace at the start of a line. For simplicity, whitespace parsing counts characters
--   all the time, although it would be a possible optimization to only count characters after the
--   start of a newline.
ws :: Parser ()
ws = $(switch [| case _ of
  " "  -> modify (+1) >> ws
  "\n" -> put 0 >> ws
  "\t" -> modify (+1) >> ws
  "\r" -> modify (+1) >> ws
  "--" -> lineComment
  "{-" -> modify (+2) >> multilineComment
  _    -> pure () |])

-- | Parse a line comment.
lineComment :: Parser ()
lineComment =
  optioned anyWord8
    (\case 10 -> put 0 >> ws
           _  -> modify (+1) >> lineComment)
    (pure ())

-- | Parse a potentially nested multiline comment.
multilineComment :: Parser ()
multilineComment = go (1 :: Int) where
  go 0 = ws
  go n = $(switch [| case _ of
    "\n" -> put 0       >> go n
    "-}" -> modify (+2) >> go (n - 1)
    "{-" -> modify (+2) >> go (n + 1)
    _    -> branch anyWord8 (modify (+1) >> go n) (pure ()) |])

-- | Query the current indentation level, fail if it's smaller than the current expected level.
guardLvl :: Parser Int
guardLvl = do
  lvl <- ask
  currentLvl <- get
  if currentLvl < lvl
    then empty
    else pure currentLvl
{-# inline guardLvl #-}

-- | Same as `guardLvl` except we throw an error on mismatch.
cutLvl :: Parser Int
cutLvl = do
  lvl <- ask
  currentLvl <- get
  if currentLvl < lvl
    then err $ IndentMore lvl
    else pure currentLvl
{-# inline cutLvl #-}

-- -- | Throw error if two levels aren't the same.
-- cutLvlEq :: Int -> Int -> Parser ()
-- cutLvlEq l l' | l == l' = pure ()
-- cutLvlEq l l' = err (ExactIndent l)

guardExactLvl :: Int -> Parser ()
guardExactLvl l = do
  l' <- get
  if l == l' then pure () else empty

-- | Throw error if the current level is not the expected one.
cutExactLvl :: Int -> Parser ()
cutExactLvl l = do
  l' <- get
  if l == l' then pure () else err (ExactIndent l)

-- | We check indentation first, then read the token, then read trailing whitespace.
token :: Parser a -> Parser a
token p = guardLvl *> p <* ws
{-# inline token #-}

moreIndented :: Parser a -> (a -> Parser b) -> Parser b
moreIndented pa k = do
  lvl <- get
  a <- pa
  local (\_ -> lvl + 1) (k a)
{-# inline moreIndented #-}

-- | Read a starting character of an identifier.
identStartChar :: Parser Char
identStartChar = fusedSatisfy
  isLatinLetter
  (\c -> isGreekLetter c || isLetter c)
  isLetter
  isLetter

-- | Read a non-starting character of an identifier.
identChar :: Parser Char
identChar = fusedSatisfy
  (\c -> isLatinLetter c || FP.isDigit c)
  (\c -> isGreekLetter c || isLetter c)
  isLetter
  isLetter

inlineIdentChar :: Parser Char
inlineIdentChar = fusedSatisfy
  (\c -> isLatinLetter c || FP.isDigit c)
  (\c -> isGreekLetter c || isLetter c)
  isLetter
  isLetter
{-# inline inlineIdentChar #-}

manyIdents :: Parser ()
manyIdents = many_ inlineIdentChar

-- | Parse a non-keyword string.
symbol :: String -> Q Exp
symbol str = [| token $(FP.string str) |]

-- | Parser a non-keyword string, throw precise error on failure.
cutSymbol :: String -> Q Exp
cutSymbol str =
  [| cutLvl >> ($(FP.string str) `cut'` Lit str) >> ws |]

-- | Parse a keyword string.
keyword :: String -> Q Exp
keyword str =
  [| token ($(FP.string str) `notFollowedBy` identChar) |]

-- | Parse a keyword string, throw precise error on failure.
cutKeyword :: String -> Q Exp
cutKeyword str =
  [| cutLvl >> (($(FP.string str) `notFollowedBy` identChar) `cut'` Lit str) >> ws |]
