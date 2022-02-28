
{-# language
  StandaloneDeriving, DeriveFunctor, GADTs, TypeFamilies, AllowAmbiguousTypes, TypeApplications,
  Strict, LambdaCase, DataKinds, PolyKinds, RankNTypes, ScopedTypeVariables, BlockArguments,
  PartialTypeSignatures
  #-}

{-# options_ghc -Wno-unused-imports #-}

module Fusion (foo)  where

import Control.Monad
import Prelude hiding (zipWith, foldr, foldl, sum, zip, reverse)
import qualified Prelude as P
import GHC.Exts hiding (toList)

{-
Summary:

  - fold fusion
    + easy, optimal speed without compiler magic, full bind, works on all inductive types
    - no zip

  - stream fusion
    + has zip, perhaps easier to freely extend with ad-hoc capabilities
      (e.g. random access, end access, ranges)
    - requires either compiler magic (call pattern spec) or mutation (see strymonas)
      for optimal performance
    - full bind is incompatible with zip (in the absence of closures)
      -> weaker bind: Stream s a -> (a -> Stream s' b) -> Stream (Bind a b s s') b

  - hyperfunctions: just a different flavor of stream fusion, not particularly worth it

Questions:

  - how to get rid of call pattern spec with staging
  - how to mix push/pull in the same system (and do we even want that)

-}



newtype Push a = Push {fold :: forall l. (a -> l -> l) -> l -> l}

instance Functor Push where
  fmap f (Push as) = Push \c n -> as (\a bs -> (c $! f a) bs) n
  {-# inline fmap #-}

data Pull a = forall s. Pull (s -> Maybe (a, s)) s
deriving instance Functor Pull

instance Applicative Push where
  pure = return
  (<*>) = ap

instance Monad Push where
  return a = Push \cons nil -> cons a nil
  Push as >>= f = Push \c n -> as (\a -> fold (f a) c) n

push :: Foldable t => t a -> Push a
push ta = Push \cons nil -> P.foldr cons nil ta

range :: Int -> Int -> Pull Int
range x y = Pull (\x -> (x, x + 1) <$ guard (x < y)) x

zip :: Pull a -> Pull b -> Pull (a, b)
zip (Pull step s) (Pull step' s') = Pull
  (\(s, s') -> do
      (a, s)  <- step s
      (b, s') <- step' s'
      pure ((a, b), (s, s')))
  (s, s')

zipWith :: (a -> b -> c) -> Pull a -> Pull b -> Pull c
zipWith f xs ys = uncurry f <$> zip xs ys

zipL :: Pull a -> Push b -> Push (a, b)
zipL (Pull step s) (Push bs) = Push $ \cons nil ->
  bs (\b hyp s -> case step s of
         Nothing     -> nil
         Just (a, s) -> cons (a, b) (hyp s))
     (\s -> nil)
     s

zipR :: Push a -> Pull b -> Push (a, b)
zipR as bs = (\(a, b) -> (b, a)) <$> zipL bs as

-- push :: Push a -> Pull a
-- push (Push as) = _

pull :: Pull a -> Push a
pull (Pull step s) = Push \cons nil ->
  let go s = case step s of
        Nothing     -> nil
        Just (a, s) -> cons a (go s)
  in go s

foldr :: (a -> b -> b) -> b -> Push a -> b
foldr c n as = fold as c n

foldl :: (b -> a -> b) -> b -> Push a -> b
foldl f b as = foldr (\a k -> oneShot(\b -> k (f b a))) id as b

sum :: Num a => Push a -> a
sum = foldl (+) 0

toList :: Push a -> [a]
toList as = fold as (:) []

-- foo x y = sum $ do
--   i <- pull $ range 0 10
--   j <- pull $ range 0 20
--   pure $! i + j

reverse :: Push a -> Push a
reverse (Push as) = Push \cons nil ->
  as (\a hyp -> oneShot (\acc -> hyp (cons a acc))) id nil

foo :: Int -> Int -> [Int]
foo x y = toList $ fmap (+20) $ reverse $ pull $ range x y

-- foo :: Int -> Int -> Int
-- foo x y = sum $ pull $ zipWith (*) (range 0 x) (range 0 y)

-- foo' :: Int -> Int -> Int
-- foo' x y = P.sum $ P.zipWith (*) [0..x] [0..y]






{-
newtype List a = List {fold :: forall l. (a -> l -> l) -> l -> l}

up :: [a] -> List a
up as = List $ \c n ->
  let go []     = n
      go (a:as) = c a (go as)
  in go as

down :: List a -> [a]
down f = fold f (:) []

map' :: (a -> b) -> List a -> List b
map' f (List as) = List $ \c n -> as (\a bs -> c (f a) bs) n
{-# inline map' #-}

range :: Int -> Int -> List Int
range x y = List $ oneShot $ \c -> oneShot $ \n ->
  let go x y | x >= y = n
      go x y = (c $! x) $! go (x + 1) y
  in go x y

bind :: List a -> (a -> List b) -> List b
bind (List as) f = List $ \c n -> as (\a bs -> fold (f a) c bs) n
{-# inline bind #-}

single :: a -> List a
single a = List $ \c n -> c a n
{-# inline single #-}

append :: List a -> List a -> List a
append (List xs) (List ys) = List $ \c n -> xs c (ys c n)
{-# inline append #-}

filter' :: (a -> Bool) -> List a -> List a
filter' f (List as) = List $ \cons nil ->
  as (\a as -> if f a then cons a as else as) nil

tail' :: List a -> List a
tail' (List as) = List $ oneShot $ \c -> oneShot $ \n ->
  as (\a hyp b -> if b then hyp False else (c $! a) $! hyp False)
     (\_ -> n)
     True
{-# inline tail' #-}

take' :: Int -> List a -> List a
take' n (List as) = List $ \cons nil ->
  as (\a as (i :: Int) -> if i < n then cons a (as (i + 1)) else nil )
     (\_ -> nil)  0

drop' :: Int -> List a -> List a
drop' n (List as) = List $ \cons nil ->
  as (\a as i -> if i >= n then cons a (as i) else as (i + 1)) (\_ -> nil) 0

sum' :: List Int -> Int
sum' (List as) = as (\a f -> oneShot (\acc -> f $! (a + acc))) (\acc -> acc) 0

foo :: Int -> Int -> Int
foo x y = sum' $ filter' odd $ map' (\x -> x * x) $ range x y

-- foo :: Int -> Int -> [Int]
-- foo x y = down $ bind (range x y) $ \i -> bind (range x y) $ \j -> single $! i + j

-- foo :: Int -> Int -> [Int]
-- foo x y = down $ tail' $ append (range x y) (range x y)

{-
range :: Int -> Int -> List Int
range x y c n =
  let go x y | x >= y = n
      go x y = c x (go (x + 1) y)
  in go x y

tail' :: List a -> List a
tail' as c n =
  as (\a hyp b -> if b then hyp False else c a (hyp False))
     (\_ -> n)
     True

range :: Int -> Int -> List Int
range x y c n =
  let go x y | x >= y = n
      go x y = c x (go (x + 1) y)
  in go x y

tail' $ range 0 10

range 0 10 =
  \c n ->
    let go x y | x >= y = n
        go x y = c x (go (x + 1) y)
    in go 0 10

tail' $ range 0 10 =
  \c n ->
    let go x y | x >= y = \_ -> n
        go x y = (\a hyp b -> if b then hyp False else c a (hyp False)) x (go (x + 1) y)
    in go 0 10 True

tail' $ range 0 10 =
  \c n ->
    let go x y b | x >= y = n
        go x y True  = go (x + 1) y False
        go x y False = c x (go (x + 1) y False)
    in go 0 10 True

tail' $ range 0 10 =
  \c n ->
    let goT x y | x >= y = n
        goT x y = goF (x + 1) y

        goF x y | x >= y = n
        goF x y = c x (goF (x + 1) y)

    in goT 0 10

down $ tail' $ range 0 10 =
  let goT x y | x >= y = []
      goT x y = goF (x + 1) y

      goF x y | x >= y = []
      goF x y = x : goF (x + 1) y

  in goT 0 10

Step A S = (Step : ^U0) → (A → S → Step) → Step → Step
Stream A = [S : ^U0, step : ^S → Step A S, s : ^S]

count : Int → Stream Int
count n = [
  S    = Int,
  step = λ n Step yield done. case n of 0 → done; _ → yield (n - 1) n,
  s    = n ]

sum : Stream Int → ^Int
sum [S, step, s] = <(fix go s. λ acc. step s (\a s. go s (a + acc)) acc) ~s 0>


bind : Stream S A → (A → Stream' S' B) → Stream (S × Maybe (A × S')) B
bind [step, s] f = [
  step = λ (s, s'). case s' of
           Nothing → case step s of
             Stop → Stop
             Skip s → Skip (s, Nothing)
             Yield a s → Skip (s, Just (a, (f a).s))
           Just (a, s') → case (f a).step s' of
             Stop       → Skip (s, Nothing)
             Skip s'    → Skip (s, Just (a, s'))
             Yield b s' → Yield b (s, Just (a, s'))
  s = (s, Nothing)
  ]

-}

data Step s a = Stop | Skip s | Yield a s
data Stream s a = Stream {step :: s -> Step s a, state :: s}

countS :: Int -> Stream Int Int
countS n = Stream go n where
  go n | n < 0 = Stop
  go n = Yield n (n - 1)

singleS :: a -> Stream Bool a
singleS a = Stream (\case True -> Yield a False; _ -> Stop) True

nilS :: Stream () a
nilS = Stream (\_ -> Stop) ()

bindS :: Stream s a -> (a -> Stream s' b) -> Stream (s, Maybe (a, s')) b
bindS (Stream st s) f =
  Stream
    (\case (s, Nothing) -> case st s of
             Stop      -> Stop
             Skip s    -> Skip (s, Nothing)
             Yield a s -> Skip (s, Just (a, state (f a)))
           (s, Just (a, s')) -> case step (f a) s' of
             Stop       -> Skip (s, Nothing)
             Skip s'    -> Skip (s, Just (a, s'))
             Yield b s' -> Yield b (s, Just (a, s')))
    (s, Nothing)

downS :: Stream s a -> [a]
downS (Stream step s) = case step s of
  Stop      -> []
  Skip s    -> downS (Stream step s)
  Yield a s -> a : downS (Stream step s)
-}
