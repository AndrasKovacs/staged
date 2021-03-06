{-# options_ghc -funbox-strict-fields #-}

module Presyntax where

import Common

data ArgInfo
  = NoName Icit
  | Named {-# unpack #-} Span
  deriving (Show)

data TopLevel
  = Nil

  -- src pos, tycon name, params, data constructors
  | DataDecl Pos Span [(Span, Maybe Tm)] [(Span, [Tm])] TopLevel

  | Definition0 Span (Maybe Tm) Tm TopLevel
  | Definition1 Span (Maybe Tm) Tm TopLevel
  deriving Show

data Bind
  = Bind Span
  | DontBind

data Proj
  = ProjIx Span Int
  | ProjName Span
  deriving Show

instance Show Bind where
  show (Bind x) = show x
  show DontBind = "_"

data Tm
  = Var Span
  | Let0 Pos Span (Maybe Tm) Tm Tm
  | Let1 Pos Span (Maybe Tm) Tm Tm
  | Pi Pos Bind Icit Tm Tm
  | Lam Pos Bind ArgInfo (Maybe Tm) Tm
  | App Tm Tm ArgInfo
  | U0 Pos Tm
  | U1 Span
  | CV Span
  | Comp Span
  | Val Span
  | Lift Pos Tm
  | Up   Span Tm
  | Down Pos Tm
  | Rec    Span [(Span, Tm)]
  | RecCon Span [(Span, Tm)]
  | EmptyRec Span                 -- overloads both tt and Top
  | Tuple Span [Tm]
  | Field Tm Proj
  | Case Pos Tm Pos [(Span, [Bind], Tm)]
  | Hole Pos
  | Int Span
  | IntLit Span Int
  | Add Tm Tm
  | Mul Tm Tm
  | Sub Tm Tm

deriving instance Show Tm

span :: Tm -> Span
span t = Span (left t) (right t) where

  left :: Tm -> Pos
  left = \case
    Var (Span l _)      -> l
    Let0 l _ _ _ _      -> l
    Let1 l _ _ _ _      -> l
    Pi l _ _ _ _        -> l
    Lam l _ _ _ _       -> l
    App t u _           -> left t
    Lift l _            -> l
    Up (Span l _) _     -> l
    Down l _            -> l
    Rec (Span l _) _    -> l
    RecCon (Span l _) _ -> l
    EmptyRec (Span l _) -> l
    Tuple (Span l _) _  -> l
    Field t pr          -> left t
    Case l _ _ _        -> l
    Hole l              -> l
    IntLit (Span l _) _ -> l
    Add l r             -> left l
    Mul l r             -> left l
    Sub l r             -> left l
    Int (Span l r)      -> l
    CV (Span l r)       -> l
    U0 l t              -> l
    U1 (Span l r)       -> l
    Comp (Span l r)     -> l
    Val (Span l r)      -> l

  right :: Tm -> Pos
  right = \case
    Var (Span _ r)                 -> r
    Let0 _ _ _ _ t                 -> right t
    Let1 _ _ _ _ t                 -> right t
    Pi _ _ _ _ t                   -> right t
    Lam _ _ _ _ t                  -> right t
    App _ t _                      -> right t
    Lift _ t                       -> right t
    Up (Span _ r) _                -> r
    Down _ t                       -> right t
    Rec (Span _ r) _               -> r
    RecCon (Span _ r) _            -> r
    EmptyRec (Span _ r)            -> r
    Tuple (Span _ r) _             -> r
    Field _ (ProjIx (Span l r) ix) -> r
    Field _ (ProjName (Span l r))  -> r
    Case _ _ r []                  -> r
    Case _ _ r ts                  -> case last ts of (_, _, t) -> right t
    Hole r                         -> r
    IntLit (Span _ r) _            -> r
    Add _ r                        -> right r
    Mul _ r                        -> right r
    Sub _ r                        -> right r
    Int (Span l r)                 -> r
    CV (Span l r)                  -> r
    U0 l t                         -> right t
    U1 (Span l r)                  -> r
    Comp (Span l r)                -> r
    Val (Span l r)                 -> r
