
module CFTT.Split where

import CFTT.Up
import CFTT.Gen

class Split a b | a -> b, b -> a where
  splitGen :: Up a -> Gen b

split :: MonadGen m => Split a b => Up a -> m b
split a = liftGen (splitGen a)

caseM :: (MonadGen m) => Split a b => Up a -> (b -> m c) -> m c
caseM a f = split a >>= f

instance Split Bool Bool where
  splitGen x = Gen \k -> [|| case $$x of
    True -> $$(k True)
    _    -> $$(k False) ||]

instance Split [a] (Maybe (Up a, Up [a])) where
  splitGen x = Gen \k -> [|| case $$x of
    []   -> $$(k Nothing)
    a:as -> $$(k (Just ([||a||], [||as||]))) ||]

instance Split (a, b) (Up a, Up b) where
  splitGen x = Gen \k -> [|| case $$x of
    (a, b) -> $$(k ([||a||], [||b||])) ||]

instance Split (Pair a b) (Pair (Up a) (Up b)) where
  splitGen x = Gen \k -> [|| case $$x of
    Pair a b -> $$(k (Pair [||a||] [||b||])) ||]

instance Split (Either a b) (Either (Up a) (Up b)) where
  splitGen x = Gen \k -> [|| case $$x of
    Left a  -> $$(k (Left [||a||]))
    Right b -> $$(k (Right [||b||])) ||]

instance Split (Maybe a) (Maybe (Up a)) where
  splitGen x = Gen \k -> [|| case $$x of
    Nothing -> $$(k Nothing)
    Just a  -> $$(k (Just [||a||])) ||]
