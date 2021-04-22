
module Pretty where

import Common
import Syntax


-- -- | Wrap in parens if expression precedence is lower than
-- --   enclosing expression precedence.
-- par :: Int -> Int -> ShowS -> ShowS
-- par p p' = showParen (p' < p)
