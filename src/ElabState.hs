

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
  -- ^ Rhs value, rhs type, rhs value, rhs type, name, source position
  = TEDef ~V.Val V.Ty U S.Tm (Maybe S.Ty) Name Pos
  -- ^ Type, type, link to data constructors, name, pos
  | TETyCon V.Ty S.Ty [Lvl] Name Pos
  -- ^ Type, type, link to type constructor, name, pos
  | TEDataCon V.Ty S.Ty Lvl Name Pos

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
  = MEUnsolved V.Ty U
  | MESolved V.Val V.Ty U

metaCxt :: D.Array MetaEntry
metaCxt = runIO D.empty
{-# noinline metaCxt #-}

readMeta :: MetaVar -> IO MetaEntry
readMeta (MetaVar i) = D.read metaCxt i
{-# inline readMeta #-}

newMeta :: V.Ty -> U -> IO MetaVar
newMeta a u = do
  s <- D.size metaCxt
  D.push metaCxt (MEUnsolved a u)
  pure (MetaVar s)
{-# inline newMeta #-}

-- Universe metacontext
--------------------------------------------------------------------------------

data UMetaEntry
  = UMEUnsolved
  | UMESolved U

uCxt :: D.Array UMetaEntry
uCxt = runIO D.empty
{-# noinline uCxt #-}

readUMeta :: UMetaVar -> IO UMetaEntry
readUMeta (UMetaVar i) = D.read uCxt i
{-# inline readUMeta #-}

newUMeta :: IO UMetaVar
newUMeta = do
  s <- D.size uCxt
  D.push uCxt UMEUnsolved
  pure (UMetaVar s)
{-# inline newUMeta #-}
