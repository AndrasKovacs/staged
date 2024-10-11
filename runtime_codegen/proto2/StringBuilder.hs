
module StringBuilder where

import Data.String

data Build = Chunk String | Append Build Build | Newline Int | Empty

instance IsString Build  where fromString = Chunk
instance Semigroup Build where (<>) = Append
instance Monoid Build    where mempty = Empty

newtype Out = Out (Int -> Build)
  deriving (Semigroup, Monoid) via (Int -> Build)

str :: String -> Out
str = fromString

strLit :: String -> Out
strLit s = "'" <> str s <> "'"

instance IsString Out where fromString s = Out (\_ -> Chunk s)

indent :: Out -> Out
indent (Out f) = Out (\i -> f $! i + 4)

newl :: Out
newl = Out Newline

build :: Out -> String
build (Out f) = go (f 0) "" where
  go :: Build -> String -> String
  go b acc = case b of
    Chunk s      -> s ++ acc
    Append b1 b2 -> go b1 (go b2 acc)
    Newline i    -> let acc' = indent i acc in '\n':acc'
    Empty        -> acc

  indent :: Int -> String -> String
  indent 0 acc = acc
  indent i acc = indent (i - 1) (' ':acc)
