
module Values where

import Common
import qualified Syntax as S

data Close a = Close Env a
data Env = Nil | Snoc Env ~Val

data Spine
  = Id
  | App1 Spine Val Icit
  | Field1 Spine Name Int

data BlockedOn
  = Unsolved MetaVar           -- unsolved (lvl1) meta
  | Eval Env S.Tm UMetaVar     -- unknown eval       (at unknown lvl)
  | WeakLift UMetaVar Ty       -- unknown type lift  (lvl 1)
  | WeakUp UMetaVar Val        -- unknown term lift  (lvl 1)

data UnfoldHead
  = Solved MetaVar  -- solved (lvl1) meta
  | Top1 Lvl        -- top lvl1 definition

type Ty = Val

data Val
  -- suspended computation
  = Unfold UnfoldHead Spine ~Val
  | Blocked BlockedOn Spine

  -- structural
  | Var Lvl
  | Top Lvl
  | Let Name Ty Val {-# unpack #-} (Close S.Tm)

  -- known lifts
  | Lift Val
  | Up Val
  | Down Val

  -- ADTs
  | TyCon Lvl
  | DataCon Lvl Int
  | Case Val {-# unpack #-} (Close (Cases S.Tm))

  -- functions
  | Pi  Name Icit Ty {-# unpack #-} (Close S.Ty)
  | App Val Val Icit
  | Lam Name Icit Ty {-# unpack #-} (Close S.Tm)

  -- records
  | Rec (Fields Ty)
  | RecCon (Fields Val)
  | Field Val Name Int

  -- universes
  | U U
