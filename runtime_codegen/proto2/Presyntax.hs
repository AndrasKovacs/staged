
module Presyntax where

import Common

data Tm
  = Var Name                                  -- x
  | Lam Name (Either Name Icit) (Maybe Tm) Tm -- \x. t | \{x}. t | \{x = y}. t |
                                              -- \(x : A). t | \{x : A}. t
  | App Tm Tm (Either Name Icit)              -- t u  | t {u} | t {x = u}
  | U                                         -- U
  | Pi Name Icit Tm Tm                        -- (x : A) -> B | {x : A} -> B
  | Let Name (Maybe Tm) Tm Tm                 -- let x : A = t; u
  | SrcPos SourcePos Tm                       -- source position for error reporting

  | Box                                       -- □
  | Quote Tm                                  -- <t>
  | Splice Tm SourcePos                       -- ~t

  | Eff                                       -- Eff
  | Return                                    -- return
  | Bind Name Tm Tm                           -- do x <- t; u
  | Seq Tm Tm                                 -- do t; u

  | Unit                                      -- ⊤ | Top
  | Tt                                        -- tte

  | Ref                                       -- Ref t
  | New                                       -- new t
  | Write                                     -- write t u
  | Read                                      -- read t

  | Nat                                       -- ℕ | Nat
  | Zero                                      -- zero
  | Suc                                       -- suc
  | NatLit Integer                            -- numeral
  | NatElim                                   -- NatElim | ℕElim

  | RecTy [(Name, Tm)]                        -- Σ(a : A, b : B ...) <|> Rec(a : A, b : B ...)
  | Rec [(Maybe Name, Tm)]                    -- (a = t, b = u, v, ...)
  | Proj Tm Name                              -- t.field

  | Hole                                      -- _
  deriving Show

pattern AppE t u = App t u (Right Expl)
pattern AppI t u = App t u (Right Impl)

-- | Get rid of source positions, for better debug printing.
stripPos :: Tm -> Tm
stripPos = \case
  Var x         -> Var x
  Lam x i ma t  -> Lam x i (stripPos <$> ma) (stripPos t)
  App t u i     -> App (stripPos t) (stripPos u) i
  U             -> U
  Pi x i a b    -> Pi x i (stripPos a) (stripPos b)
  Let x a t u   -> Let x (stripPos <$> a) (stripPos t) (stripPos u)
  SrcPos _ t    -> stripPos t
  Hole          -> Hole
  Box           -> Box
  Quote t       -> Quote (stripPos t)
  Splice t p    -> Splice (stripPos t) p
  Eff           -> Eff
  Return        -> Return
  Bind x t u    -> Bind x (stripPos t) (stripPos u)
  Seq t u       -> Seq (stripPos t) (stripPos u)
  Unit          -> Unit
  Tt            -> Tt
  Ref           -> Ref
  New           -> New
  Write         -> Write
  Read          -> Read
  Nat           -> Nat
  Zero          -> Zero
  Suc           -> Suc
  NatLit n      -> NatLit n
  NatElim       -> NatElim
  RecTy fs      -> RecTy ((stripPos <$>) <$> fs)
  Rec fs        -> Rec   ((stripPos <$>) <$> fs)
  Proj t x      -> Proj (stripPos t) x
