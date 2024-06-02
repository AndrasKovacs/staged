
module CFTT.Examples where

import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.State.Strict hiding (modify')
import qualified Control.Monad.State.Strict as State
import Control.Monad.Trans.Maybe
import Language.Haskell.TH

import CFTT.Gen
import CFTT.Split
import CFTT.Join
import CFTT.Up
import CFTT.Pull
import CFTT.Improve
import CFTT.Tree
import qualified CFTT.Up as Up
import qualified CFTT.Pull as P

--------------------------------------------------------------------------------

printUp :: Up a -> IO ()
printUp a = do
  x <- unType <$> runQ (examineCode a)
  print $ ppr x


--------------------------------------------------------------------------------

-- Code size exponential in the number of caseM-s here, because
-- everything gets inlined in case branches.
exM1 :: Int -> StateT Int (MaybeT Identity) ()
exM1 x = $$(down do
  caseM ([||x||] Up.== 10) \case
    True -> modify'  (+10)
    False -> modify' (+20)
  caseM ([||x||] Up.== 10) \case
    True -> modify'  (+10)
    False -> modify' (+20)
  caseM ([||x||] Up.== 10) \case
    True -> modify'  (+10)
    False -> modify' (+20)
  caseM ([||x||] Up.== 10) \case
    True  -> modify' (+10)
    False -> modify' (+20)
  )


-- Code size is linear.
exM2 :: Int -> StateT Int (MaybeT Identity) ()
exM2 x = $$(down do
  join $ caseM ([||x||] Up.== 10) \case
    True  -> modify  (+10)
    False -> modify (+20)
  join $ caseM ([||x||] Up.== 10) \case
    True -> modify  (+10)
    False -> modify (+20)
  join $ caseM ([||x||] Up.== 10) \case
    True -> modify  (+10)
    False -> modify (+20)
  caseM ([||x||] Up.== 10) \case
    True  -> modify' (+10)
    False -> modify' (+20)
  )

type M = StateT Int (ExceptT () Identity)

-- The "fail" branches jump immediately to the "catch" code
exM3 :: Int -> M ()
exM3 x = $$(down do
  catchError (join $ do
    caseM ([||x||] Up.== 10) \case
      True  -> modify' (+10)
      False -> throwError [||()||]
    caseM ([||x||] Up.== 15) \case
      True  → modify' (+10)
      False → throwError [||()||])
    \_ -> modify' (+ 11))

-- staged non-tail recursion
exM4 :: [Int] -> M ()
exM4 ns = $$(down $ do
  caseM [||ns||] \case
    Nothing      -> pure Up.tt
    Just (n, ns) -> caseM (n Up.== 10) \case
      True  -> throwError [||()||]
      False -> do {modify' (+20); up [|| exM4 $$ns ||]}
  )

-- vanilla mtl
exM4' :: [Int] -> M ()
exM4' !ns = do
  case ns of
    [] -> pure ()
    n:ns -> case (n Prelude.== 10) of
      True -> throwError ()
      _    -> do {State.modify' (+20); exM4' ns}

-- Section 3.6. example from the paper
exM5 :: Tree Int -> StateT [Int] (ExceptT () Identity) (Tree Int)
exM5 t = $$(down do
  caseM [||t||] \case
    Nothing        -> pure [||Leaf||]
    Just (n, l, r) -> do
      caseM (n Up.== 0) \case
        True  -> throwError Up.tt
        False -> pure Up.tt
      ns <- get
      n <- join $ caseM ns \case
        Nothing      -> pure n
        Just (n, ns) -> put' ns >> pure n
      l <- up [|| exM5 $$l ||]
      r <- up [|| exM5 $$r ||]
      pure [|| Node $$n $$l $$r ||]
  )

-- vanilla mtl
exM5' :: Tree Int -> StateT [Int] (ExceptT () Identity) (Tree Int)
exM5' t = do
  case t of
    Leaf       -> pure Leaf
    Node n l r -> do
      case (n Prelude.== 0) of
        True  -> throwError ()
        False -> pure ()
      ns <- get
      n <- case ns of
        []   -> pure n
        n:ns -> put ns >> pure n
      l <- exM5' l
      r <- exM5' r
      pure $ Node n l r

filterM :: forall f m a. Improve f m => (Up a -> m Bool) -> Up ([a] -> f [a])
filterM f = [||
   let go as = $$(down @f @m $ caseM [||as||] \case
                   Nothing      -> pure [||[]||]
                   Just (a, as) -> do
                     as <- up [|| go $$as ||]
                     f a >>= \case
                       True  -> pure [||$$a : $$as||]
                       False -> pure as
                 )
   in go
   ||]

-- I inline the filterM definition here because of TH module restrictions.
exM6 :: [Int] -> StateT Int (ExceptT () Identity) [Int]
exM6 as = $$(down $ caseM [||as||] \case
                   Nothing      -> pure [||[]||]
                   Just (a, as) -> caseM (a Up.== 0) \case
                     True -> throwError Up.tt
                     _ -> do
                       as <- up [||exM6 $$as||]
                       caseM (a Up.< 20) \case
                         True -> pure [||$$a : $$as||]
                         _    -> pure as
                 )

-- Streams
--------------------------------------------------------------------------------

exS1 :: [Int]
exS1 = $$(toList $ cons 10 $ cons 20 mempty)

exS2 :: [Int]
exS2 = $$(toList $ (forEach (P.take 20 count) \x -> (P.take 20 count) <&> (\y -> x + y)))

exS3 :: [Int]
exS3 = $$(toList $
       forEach (P.take 10 count) \x ->
       forEach (P.take 20 count) \y ->
       forEach (P.take 30 count) \z ->
       single (x + y + z))

exS4 :: [(Int, Int)]
exS4 = $$(toList $ P.zip
      (forEach (P.take 20 count) \x -> (P.take 20 count) <&> (\y -> x + y))
       P.count)

exS5 :: [Int]
exS5 = $$(toList ((*) <$> P.take 10 count <:> P.take 10 (countFrom 20)))

-- Section 4.4 example in paper:
exS6 :: [Int]
exS6 = $$(toList $
       forEach (P.take 100 (countFrom 0)) \x ->
       genPull (x * 2) \y ->
       casePull (x Up.< 50) \case
         True  -> P.take y (countFrom x)
         False -> single y)

-- Medium-sized zip of two loops
exS7 :: [(Int, Int)]
exS7 = $$(toList $ P.zip

      (forEach (P.take 100 (countFrom 0)) \x ->
       genPull (x * 2) \y ->
       casePull (x Up.< 50) \case
         True  -> P.take y (countFrom x)
         False -> single y)


      (forEach (P.take 20 count) \x ->
        (P.take 20 count) <&> (\y -> x + y)))
