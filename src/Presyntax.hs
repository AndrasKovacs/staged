{-# options_ghc -funbox-strict-fields #-}

module Presyntax where

import Common

data TopLevel
  = Nil
  | DataDecl Pos Span Tm [(Span, Tm)] TopLevel
  | Definition Span (Maybe Tm) Tm TopLevel
  deriving Show

data Bind
  = Bind Span
  | DontBind

instance Show Bind where
  show (Bind x) = show x
  show DontBind = "_"

data Tm
  = Var Span
  | Let Pos Span (Maybe Tm) Tm Tm
  | Pi Pos Bind Icit Tm Tm
  | Lam Pos Bind ArgInfo (Maybe Tm) Tm
  | App Tm Tm ArgInfo
  | Ty Span U
  | Lift Pos Tm
  | Up   Span Tm
  | Down Pos Tm
  | Rec    Span [(Span, Tm)]
  | RecCon Span [(Span, Tm)]
  | EmptyRec Span                 -- overloads both tt and Top
  | Tuple Span [Tm]
  | Field Tm Span
  | Fix Pos Bind Bind Tm
  | Case Pos Tm Pos [(Span, [Bind], Tm)]
  | Hole Pos
  deriving Show

span :: Tm -> Span
span t = Span (left t) (right t) where
  left :: Tm -> Pos
  left = \case
    Var (Span l _)      -> l
    Let l _ _ _ _       -> l
    Pi l _ _ _ _        -> l
    Lam l _ _ _ _       -> l
    App t u _           -> left t
    Ty (Span l _) _     -> l
    Lift l _            -> l
    Up (Span l _) _     -> l
    Down l _            -> l
    Rec (Span l _) _    -> l
    RecCon (Span l _) _ -> l
    EmptyRec (Span l _) -> l
    Tuple (Span l _) _  -> l
    Field t _           -> left t
    Fix l _ _ _         -> l
    Case l _ _ _        -> l
    Hole l              -> l

  right :: Tm -> Pos
  right = \case
    Var (Span _ r)      -> r
    Let _ _ _ _ t       -> right t
    Pi _ _ _ _ t        -> right t
    Lam _ _ _ _ t       -> right t
    App _ t _           -> right t
    Ty (Span _ r) _     -> r
    Lift _ t            -> right t
    Up (Span _ r) _     -> r
    Down _ t            -> right t
    Rec (Span _ r) _    -> r
    RecCon (Span _ r) _ -> r
    EmptyRec (Span _ r) -> r
    Tuple (Span _ r) _  -> r
    Field _ (Span _ r)  -> r
    Fix _ _ _ t         -> right t
    Case _ _ r []       -> r
    Case _ _ r ts       -> case last ts of (_, _, t) -> right t
    Hole r              -> r
