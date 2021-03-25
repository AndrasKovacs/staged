

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

  -- ^ Rhs, type, rhs val, type val, name, source pos
  = TEDef0 S.Tm0 S.Ty ~V.Val0 V.Ty Name Pos

  -- ^ Rhs, type, rhs val, type val, name, source pos
  | TEDef1 S.Tm1 S.Ty ~V.Val1 V.Ty Name Pos

  -- ^ Type, type val, data constructors, name, source pos
  | TETyCon S.Ty V.Ty [Lvl] Name Pos

  -- ^ Type, Type value, parent tycon, name, source pos
  | TEDataCon S.Ty V.Ty Lvl Name Pos


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
  = MEUnsolved V.Ty
  | MESolved V.Val1 V.Ty

metaCxt :: D.Array MetaEntry
metaCxt = runIO D.empty
{-# noinline metaCxt #-}

readMeta :: MetaVar -> IO MetaEntry
readMeta (MetaVar i) = D.read metaCxt i
{-# inline readMeta #-}

newMeta :: V.Ty -> IO MetaVar
newMeta a = do
  s <- D.size metaCxt
  D.push metaCxt (MEUnsolved a)
  pure (MetaVar s)
{-# inline newMeta #-}

unsolvedMetaTy :: MetaVar -> IO V.Ty
unsolvedMetaTy m = readMeta m >>= \case
  MEUnsolved a -> pure a
  _            -> impossible
{-# inline unsolvedMetaTy #-}

-- -- Universe metacontext
-- --------------------------------------------------------------------------------

-- data UMetaEntry
--   = UMEUnsolved
--   | UMESolved U

-- uCxt :: D.Array UMetaEntry
-- uCxt = runIO D.empty
-- {-# noinline uCxt #-}

-- readUMeta :: UMetaVar -> IO UMetaEntry
-- readUMeta (UMetaVar i) = D.read uCxt i
-- {-# inline readUMeta #-}

-- newUMeta :: IO UMetaVar
-- newUMeta = do
--   s <- D.size uCxt
--   D.push uCxt UMEUnsolved
--   pure (UMetaVar s)
-- {-# inline newUMeta #-}
