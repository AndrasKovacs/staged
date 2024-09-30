
module Presyntax where

import Common

data Tm
  = Var Name                                  -- x
  | Lam Name (Either Name Icit) (Maybe Tm) Tm -- \x. t | \{x}. t | \{x = y}. t |
                                              -- \(x : A). t | \{x : A}. t
  | App Tm Tm (Either Name Icit)              -- t u  | t {u} | t {x = u}
  | U                                         -- U
  | Pi Name Icit Tm Tm                        -- (x : A) -> B | {x : A} -> B
  | Let Name Tm Tm Tm                         -- let x : A = t; u
  | SrcPos SourcePos Tm                       -- source position for error reporting

  | Box Tm                                    -- □
  | Quote Tm                                  -- <t>
  | Splice Tm                                 -- ~t

  | Eff Tm                                    -- Eff
  | Return Tm                                 -- return
  | Bind Name Tm Tm                           -- do x <- t; u
  | ConstBind Tm Tm                           -- do t; u

  | Unit                                      -- ⊤, Top
  | Tt                                        -- tt

  | Hole                                      -- _
  deriving Show

-- | Get rid of source positions, for better debug printing.
stripPos :: Tm -> Tm
stripPos = \case
  Var x         -> Var x
  Lam x i ma t  -> Lam x i (stripPos <$> ma) (stripPos t)
  App t u i     -> App (stripPos t) (stripPos u) i
  U             -> U
  Pi x i a b    -> Pi x i (stripPos a) (stripPos b)
  Let x a t u   -> Let x (stripPos a) (stripPos t) (stripPos u)
  SrcPos _ t    -> stripPos t
  Hole          -> Hole
  Box t         -> Box (stripPos t)
  Quote t       -> Quote (stripPos t)
  Splice t      -> Splice (stripPos t)
  Eff t         -> Eff (stripPos t)
  Return t      -> Return (stripPos t)
  Bind x t u    -> Bind x (stripPos t) (stripPos u)
  ConstBind t u -> ConstBind (stripPos t) (stripPos u)
  Unit          -> Unit
  Tt            -> Tt
