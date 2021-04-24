
-- | This module exists because the `hashable` in my stackage snapshot uses the wrong
--   hash function: https://github.com/haskell-unordered-containers/hashable/issues/190

module FNV164 where

import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B
import GHC.ForeignPtr
import GHC.Exts

-- TODO: switch to something faster, like
--   http://hackage.haskell.org/package/murmur-hash-0.1.0.9/docs/src/Data-Digest-Murmur64.html
--   (but fix its performance bugs first)
fnv164 :: B.ByteString -> Int -> Int
fnv164 (B.PS (ForeignPtr ptr _) (I# offset) (I# len)) (I# salt) = let
  go :: Addr# -> Addr# -> Int# -> Int#
  go ptr end hash = case eqAddr# ptr end of
    1# -> hash
    _  -> go (plusAddr# ptr 1#) end
             (xorI# (hash *# 1099511628211#) (indexInt8OffAddr# ptr 0#))
  start = plusAddr# ptr offset
  end   = plusAddr# start len
  in I# (go start end salt)
