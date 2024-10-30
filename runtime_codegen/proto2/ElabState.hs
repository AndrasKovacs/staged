
module ElabState where

import Data.IORef
import System.IO.Unsafe
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS

import qualified Presyntax as P
import Cxt.Type
import Common
import Value
import Syntax

--------------------------------------------------------------------------------

-- The context for checking problems is mostly just a duplication of the code
-- for the plain metacontext. It would be possible to merge them into a single
-- data structure, but that would be somewhat less "well-typed", and would make
-- it less efficient to iterate over only metas or only checking problems.

-- | In (Unchecked Γ t A m), we postpone checking t with A in Γ and create m
--   as a fresh meta, which is a "placedholder" for the eventual checking
--   result. After we actually perform the checking, we have to unify the
--   result with the placeholder.
data Unchecked = Unchecked Cxt P.Tm VTy MetaVar

instance Show Unchecked where
  show (Unchecked _ t _ _) = show $ P.stripPos t

type UncheckedCxt = IM.IntMap Unchecked
type CheckedCxt   = IM.IntMap Tm

uncheckedCxt :: IORef UncheckedCxt
uncheckedCxt = unsafeDupablePerformIO $ newIORef mempty
{-# noinline uncheckedCxt #-}

checkedCxt :: IORef CheckedCxt
checkedCxt = unsafeDupablePerformIO $ newIORef mempty

nextCheckVar :: IORef CheckVar
nextCheckVar = unsafeDupablePerformIO $ newIORef 0
{-# noinline nextCheckVar #-}

newCheck :: Cxt -> P.Tm -> VTy -> MetaVar -> IO CheckVar
newCheck cxt t a m = do
  c <- readIORef nextCheckVar
  writeIORef nextCheckVar $! c + 1
  modifyIORef' uncheckedCxt $ IM.insert (coerce c) $ Unchecked cxt t a m
  pure c

readCheck :: CheckVar -> IO (Either Tm Unchecked)
readCheck c = do
  ucs <- readIORef uncheckedCxt
  cs  <- readIORef checkedCxt
  case IM.lookup (coerce c) ucs of
    Just e -> pure (Right e)
    _      -> case IM.lookup (coerce c) cs of
      Just t -> pure $ Left t
      _      -> impossible

lookupCheck :: CheckVar -> Either Tm Unchecked
lookupCheck = unsafeDupablePerformIO . readCheck

writeChecked :: CheckVar -> Tm -> IO ()
writeChecked c t = do
  modifyIORef' uncheckedCxt $ IM.delete (coerce c)
  modifyIORef' checkedCxt   $ IM.insert (coerce c) t

addBlocking :: CheckVar -> MetaVar -> IO ()
addBlocking blocked blocks =
  modifyMeta blocks $ \case
    Unsolved bs ns oa a p -> Unsolved (IS.insert (coerce blocked) bs) ns oa a p
    _                     -> impossible

--------------------------------------------------------------------------------

-- | Set of checking problems.
type Blocking = IS.IntSet

data MetaEntry
  -- ^ Unsolved meta which may block checking problems.
  = Unsolved Blocking Cxt ~VTy ~VTy SourcePos

  -- ^ Contains value and type of solution.
  | Solved Val ~VTy

nextMetaVar :: IORef MetaVar
nextMetaVar = unsafeDupablePerformIO $ newIORef 0
{-# noinline nextMetaVar #-}

alterMeta :: MetaVar -> (Maybe MetaEntry -> Maybe MetaEntry) -> IO ()
alterMeta m f = modifyIORef' mcxt (IM.alter f (coerce m))

modifyMeta :: MetaVar -> (MetaEntry -> MetaEntry) -> IO ()
modifyMeta m f = alterMeta m (maybe (error "impossible") (Just . f))

writeMeta :: MetaVar -> MetaEntry -> IO ()
writeMeta m e = modifyMeta m (const e)

newRawMeta :: Blocking -> Cxt -> VTy -> VTy -> SourcePos -> IO MetaVar
newRawMeta bs cxt openA ~a p = do
  m <- readIORef nextMetaVar
  writeIORef nextMetaVar $! m + 1
  alterMeta m (maybe (Just (Unsolved bs cxt openA a p)) impossible)
  pure m

type MCxt = IM.IntMap MetaEntry

mcxt :: IORef MCxt
mcxt = unsafeDupablePerformIO $ newIORef mempty
{-# noinline mcxt #-}

readMeta :: MetaVar -> IO MetaEntry
readMeta m = do
  ms <- readIORef mcxt
  case IM.lookup (coerce m) ms of
    Just e  -> pure e
    Nothing -> impossible

readUnsolved :: MetaVar -> IO (Blocking, VTy, SourcePos)
readUnsolved m = readMeta m >>= \case
  Unsolved bs _ _ ty p -> pure (bs, ty, p)
  _                    -> impossible

lookupMeta :: MetaVar -> MetaEntry
lookupMeta = unsafeDupablePerformIO . readMeta

--------------------------------------------------------------------------------

sourceCode :: IORef String
sourceCode = unsafeDupablePerformIO $ newIORef ""
{-# noinline sourceCode #-}

--------------------------------------------------------------------------------

-- | Reset all mutable refs to initial state.
reset :: IO ()
reset = do
  writeIORef nextMetaVar 0
  writeIORef mcxt mempty
  writeIORef nextCheckVar 0
  writeIORef uncheckedCxt mempty
  writeIORef checkedCxt mempty
  writeIORef sourceCode ""
