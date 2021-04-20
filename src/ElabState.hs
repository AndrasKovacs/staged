

module ElabState where

import IO
import qualified Data.Array.Dynamic.L as D
import qualified Data.Array.LM        as A
import qualified Data.HashMap.Strict  as M
import Data.IORef

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

  -- ^ Type, type val, parent type constructor, con index, name, source pos
  | TEDataCon S.Ty V.Ty Lvl Int Name Pos

-- TODO: resizing and allocation
topSize :: Int
topSize = 50000

undefTopEntry :: a
undefTopEntry = error "top: undefined entry"
{-# noinline undefTopEntry #-}

initTop :: IO (A.Array TopEntry)
initTop = A.new topSize undefTopEntry

top :: A.Array TopEntry
top = runIO initTop
{-# noinline top #-}

resetTop :: IO ()
resetTop = A.set top undefTopEntry

readTop :: Lvl -> IO TopEntry
readTop (Lvl x) | 0 <= x && x < topSize = A.read top x
                | otherwise             = error "index out of bounds"
{-# inline readTop #-}

newTop :: Lvl -> TopEntry -> IO ()
newTop x e = A.write top (coerce x) e
{-# inline newTop #-}

type TopNames = M.HashMap RawName Lvl

initTopNames :: IO (IORef TopNames)
initTopNames = newIORef mempty

topNames :: IORef TopNames
topNames = runIO initTopNames
{-# noinline topNames #-}

newTopName :: RawName -> Lvl -> IO ()
newTopName x l = modifyIORef' topNames (M.insert x l)
{-# inline newTopName #-}

resetTopNames :: IO ()
resetTopNames = writeIORef topNames mempty

lookupTopName :: RawName -> IO (Maybe Lvl)
lookupTopName x = do
  ns <- readIORef topNames
  pure $! M.lookup x ns

-- Metacontext
--------------------------------------------------------------------------------

data MetaEntry
  = Unsolved V.Ty -- ^ Closed type val
  | Solved V.Val1 V.Ty

initMetaCxt :: IO (D.Array MetaEntry)
initMetaCxt = D.empty

metaCxt :: D.Array MetaEntry
metaCxt = runIO initMetaCxt
{-# noinline metaCxt #-}

resetMetaCxt :: IO ()
resetMetaCxt = D.clear metaCxt

readMeta :: MetaVar -> IO MetaEntry
readMeta (MetaVar i) = D.read metaCxt i
{-# inline readMeta #-}

-- | Args: closed type value.
newMeta :: V.Ty -> IO MetaVar
newMeta a = do
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

initCvCxt :: IO (D.Array CVMetaEntry)
initCvCxt = D.empty

cvCxt :: D.Array CVMetaEntry
cvCxt = runIO initCvCxt
{-# noinline cvCxt #-}

resetCvCxt :: IO ()
resetCvCxt = D.clear cvCxt

readCVMeta :: CVMetaVar -> IO CVMetaEntry
readCVMeta (CVMetaVar i) = D.read cvCxt i
{-# inline readCVMeta #-}

newCVMeta :: IO CVMetaVar
newCVMeta = do
  s <- D.size cvCxt
  D.push cvCxt CVUnsolved
  pure (CVMetaVar s)
{-# inline newCVMeta #-}

--------------------------------------------------------------------------------

reset :: IO ()
reset = do
  resetTop
  resetTopNames
  resetMetaCxt
  resetCvCxt
