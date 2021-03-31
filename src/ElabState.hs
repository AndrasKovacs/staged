

module ElabState where

import IO
import qualified Data.Array.Dynamic.L as D
import qualified Data.Array.LM        as A

import Common
import qualified Syntax as S
import qualified Values as V

-- Top scope
--------------------------------------------------------------------------------

data TopEntry

  -- ^ Type, type val, rhs, rhs val, CV, name, source pos
  = TEDef0 S.Ty V.Ty S.Tm0 V.Val0 CV Name Pos

  -- ^ Type, type val, rhs, rhs val, name, source pos
  | TEDef1 S.Ty V.Ty S.Tm1 V.Val1 Name Pos

  -- ^ Type, Type val, constructors, name, source pos
  | TETyCon S.Ty V.Ty [Lvl] Name Pos

  -- ^ Type, type val, universe, parent type constructor, name, source pos
  | TEDataCon S.Ty V.Ty U Lvl Name Pos


-- TODO: we'll implement top resizing and allocation later
topSize :: Int
topSize = 50000

top :: A.Array TopEntry
top = runIO (A.new topSize (error "top: undefined entry"))
{-# noinline top #-}

readTop :: Lvl -> IO TopEntry
readTop (Lvl x) | 0 <= x && x < topSize = A.read top x
                | otherwise             = error "index out of bounds"
{-# inline readTop #-}

-- Metacontext
--------------------------------------------------------------------------------

data MetaEntry
  = Unsolved ~V.Ty -- ^ Closed type val
  | Solved V.Val1 V.Ty

metaCxt :: D.Array MetaEntry
metaCxt = runIO D.empty
{-# noinline metaCxt #-}

readMeta :: MetaVar -> IO MetaEntry
readMeta (MetaVar i) = D.read metaCxt i
{-# inline readMeta #-}

-- | Args: closed type value.
newMeta :: V.Ty -> IO MetaVar
newMeta ~a = do
  s <- D.size metaCxt
  D.push metaCxt (Unsolved a)
  pure (MetaVar s)
{-# inline newMeta #-}

unsolvedMetaTy :: MetaVar -> IO V.Ty
unsolvedMetaTy m = readMeta m >>= \case
  Unsolved a -> pure a
  _          -> impossible
{-# inline unsolvedMetaTy #-}


-- CV metacontext
--------------------------------------------------------------------------------

data CVMetaEntry = CVUnsolved | CVSolved CV

cvCxt :: D.Array CVMetaEntry
cvCxt = runIO D.empty
{-# noinline cvCxt #-}

readCVMeta :: CVMetaVar -> IO CVMetaEntry
readCVMeta (CVMetaVar i) = D.read cvCxt i
{-# inline readCVMeta #-}

newCVMeta :: IO CVMetaVar
newCVMeta = do
  s <- D.size cvCxt
  D.push cvCxt CVUnsolved
  pure (CVMetaVar s)
{-# inline newCVMeta #-}
