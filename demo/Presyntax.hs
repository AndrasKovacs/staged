
module Presyntax where

import Common

data Tm
  = Var Name                                   -- x
  | Lam Name (Maybe Tm) (Either Name Icit) Tm  -- \x. t  |  \{x}. t  |  \{x = y}. t
  | App Tm Tm (Either Name Icit)               -- t u  |  t {u}  |  t {x = u}
  | U Stage                                    -- U i
  | Pi Name Icit Tm Tm                         -- (x : A) -> B  |  {x : A} -> B
  | Let Stage Name (Maybe Tm) Tm Tm            -- let x : A := t; u  |  let x : A = t; u
  | Pos (DontShow SourcePos) Tm                -- source position for error reporting
  | Hole                                       -- _
  | Lift Tm                                    -- ^A
  | Quote Tm                                   -- <t>
  | Splice Tm                                  -- [t]

  | Nat Stage
  | Zero Stage
  | Suc Stage
  | NatElim Stage
  deriving Show
