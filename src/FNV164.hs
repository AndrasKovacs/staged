
module FNV164 where

import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B
import GHC.ForeignPtr
import GHC.Exts

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
