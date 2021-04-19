
{-# language
  DeriveFunctor, GADTs, TypeFamilies, AllowAmbiguousTypes, TypeApplications, Strict, LambdaCase,
  DataKinds, PolyKinds, RankNTypes, ScopedTypeVariables, BlockArguments #-}

module Fusion where

-- import GHC.Exts

import Prelude hiding (sum, foldl, foldr, zipWith, reverse, map, filter)

data HF a b =
  forall s. Pack (s -> forall r. (b -> r) -> ((a -> b) -> s -> r) -> r) s

lift :: (a -> b) -> HF a b
lift f = Pack (\s left right -> right f s) ()

comp :: HF b c -> HF a b -> HF a c
comp (Pack f s) (Pack f' s') = Pack
  (\(s, s') left right -> f s
    (\c -> left c)
    (\k s -> f' s'
      (\b -> left (k b))
      (\k' s' -> right (k . k') (s, s'))
    )
  )
  (s, s')

fix :: (a -> a) -> a
fix f = let x = f x in x

run :: HF a a -> a
run (Pack f s) =
  (fix $ \go s -> f s (\a -> a) (\k s -> k (go s))) s

-- call pattern spec still needed!
appendHF :: HF a b -> HF a b -> HF a b
appendHF (Pack f s) (Pack f' s') = Pack
  (\s end step  -> case s of
      Left s   -> f s   (\b     -> step (\_ -> b) (Right s'))
                        (\ab s  -> step ab (Left s))
      Right s' -> f' s' (\b     -> end b)
                        (\ab s' -> step ab (Right s'))
  )
  (Left s)

cons :: (a -> b) -> HF a b -> HF a b
cons f (Pack step s) = Pack
  (\x left right -> case x of
      Nothing -> right f (Just s)
      Just s  -> step s
        (\b -> left b)
        (\g s -> right g (Just s))
  )
  Nothing

newtype Stream a = Stream {unStream :: forall b c. (a -> b -> c) -> c -> HF b c}

map :: (a -> b) -> Stream a -> Stream b
map f (Stream g) = Stream $ \cons nil -> g (\a b -> cons (f a) b) nil

append :: Stream a -> Stream a -> Stream a
append (Stream xs) (Stream ys) = Stream $ \cons nil ->
  appendHF (xs cons nil) (ys cons nil)

-- bind :: Stream a -> (a -> Stream b) -> Stream b
-- bind (Stream f) g = Stream $ \cons nil ->
--   f (\a bs -> unStream (g a) _ _) nil

-- filter :: (a -> Bool) -> Stream a -> Stream a
-- filter f (Stream g) = Stream $ \cons nil -> _

zip :: Stream a -> Stream b -> Stream (a, b)
zip (Stream as) (Stream bs) = Stream $ \cons nil ->
  _


fromList :: [a] -> Stream a
fromList as = Stream $ \cons nil ->
  Pack (\as end step -> case as of
           []   -> end nil
           a:as -> step (cons a) as)
       as

unfoldr :: (b -> Maybe (a, b)) -> b -> Stream a
unfoldr f s = Stream $ \cons nil -> Pack
  (\s end step -> case f s of
      Nothing     -> end nil
      Just (a, s) -> step (cons a) s)
  s

range :: Int -> Int -> Stream Int
range x y =
  unfoldr (\x -> if x >= y then Nothing else Just (x, x + 1)) x

foldr :: (a -> b -> b) -> b -> Stream a -> b
foldr cons nil (Stream f) = run (f cons nil)

toList :: Stream a -> [a]
toList = foldr (:) []

foldl :: (b -> a -> b) -> b -> Stream a -> b
foldl f b g = foldr (\a k b -> k (f b a)) (\b -> b) g b

sum :: Stream Int -> Int
sum = foldl (+) 0
