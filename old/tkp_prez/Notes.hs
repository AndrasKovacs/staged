
{-# language TemplateHaskell #-}
{-# options_ghc -Wno-unused-imports #-}

import Language.Haskell.TH
import Language.Haskell.TH.Syntax



type Code' = Code IO
printCode' x = ppr . unType <$> examineCode x

b :: Bool
b = True

code :: Code' Bool
code = [|| b ||]

code2 :: Code' Bool
code2 = if b then [|| False ||] else [|| True ||]

-- x :: B
-- x = 100 * 100

-- f :: Int -> Code' Int
-- f x =

-- g :: Code' (Int -> Int)
-- g = [|| \z -> $$(f x) ||]

-- f :: Bool -> Code Bool
-- f b = [|| b ||]
