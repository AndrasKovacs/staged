
module Values where

import Common
import qualified Syntax as S

data Close a = Close Env a deriving Show
data Env = Nil | Snoc1 Env ~Val1 | Snoc0 Env Lvl deriving Show

wk1Env :: Env -> Env
wk1Env = \case Snoc1 env _ -> env; _ -> impossible
{-# inline wk1Env #-}

wk0Env :: Env -> Env
wk0Env = \case Snoc0 env _ -> env; _ -> impossible
{-# inline wk0Env #-}

envLen :: Env -> Int
envLen = go 0 where
  go acc Nil = acc
  go acc (Snoc1 env _) = go (acc + 1) env
  go acc (Snoc0 env _) = go (acc + 1) env

type Ty = Val1
type CV = Val1

data Spine
  = Id
  | App1 Spine Val1 Icit
  | Field1 Spine Name Int
  deriving Show

reverseSpine :: Spine -> Spine
reverseSpine = go Id where
  go acc Id               = acc
  go acc (App1 sp t i)    = go (App1 acc t i) sp
  go acc (Field1 sp x ix) = go (Field1 acc x ix) sp

spineLen :: Spine -> Lvl
spineLen = go 0 where
  go acc Id              = acc
  go acc (App1 sp _ _)   = go (acc + 1) sp
  go acc (Field1 sp _ _) = go (acc + 1) sp


data UnfoldHead
  = Top1 Lvl
  | Solved MetaVar
  deriving Show

data RigidHead
  = RHVar1 Lvl
  | RHTyCon Lvl
  | RHDataCon Lvl Int
  deriving (Eq, Show)

data Val0
  = Var0 Lvl
  | Top0 Lvl
  | Let0 Name Ty Val0 {-# unpack #-} (Close S.Tm0)
  | App0 Val0 Val0
  | Case Val0 {-# unpack #-} (Close (Cases S.Tm0))
  | Down Val1
  | Field0 Val0 Name Int
  | RecCon0 (Fields Val0)
  | Lam0 Name Ty {-# unpack #-} (Close S.Tm0)
  | Add Val0 Val0
  | Sub Val0 Val0
  | Mul Val0 Val0
  | IntLit Int
  deriving Show

data Val1
  = Unfold UnfoldHead Spine ~Val1  -- delayed top-level def unfolding
  | Flex MetaVar Spine
  | Rigid RigidHead Spine
  | Pi Name Icit Ty {-# unpack #-} (Close S.Ty)
  | Lam1 Name Icit Ty {-# unpack #-} (Close S.Tm1)
  | Fun Ty Ty CV
  | Lift CV Ty
  | Up Val0
  | Rec0 (Fields Ty)
  | Rec1 (Close (Fields S.Ty))
  | RecCon1 (Fields Val1)
  | U0 CV
  | U1
  | CV
  | Comp
  | Val
  | Int
  deriving Show

pattern Var1 :: Lvl -> Val1
pattern Var1 x = Rigid (RHVar1 x) Id
