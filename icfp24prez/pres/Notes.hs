
module Notes where

import Control.Monad.Reader

f :: Reader Bool Int -> Mallac
f = do
  b <- ask
  if b then return 10
       else return 20
