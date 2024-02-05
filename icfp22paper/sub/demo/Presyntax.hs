
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

  | Nat Stage                                  -- Nat0 | Nat1
  | Zero Stage                                 -- zero0 | zero1
  | Suc Stage                                  -- suc0 | suc1
  | NatElim Stage                              -- NatElim0 | NatElim1
  deriving Show
