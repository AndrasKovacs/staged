
module CFTT.Join where

import Data.Kind
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Trans.Maybe

import CFTT.Up
import CFTT.Gen
import CFTT.SOP

-- Functions from Elₛ to Up
--------------------------------------------------------------------------------

type family FunSU (a ∷ Uₛ) (b ∷ Type) ∷ Type where
  FunSU '[]       b = ()
  FunSU (a ': as) b = (Up (Funₚₜ a b), FunSU as b)

tabulate ∷ ∀ a b. Sing a → (Elₛ a → Up b) → FunSU a b
tabulate SNil           f = ()
tabulate (SCons as ass) f = (lamₚₜ as (f . Here), tabulate ass (f . There))

index ∷ FunSU a b → Elₛ a → Up b
index fs (Here x)  = appₚₜ (fst fs) x
index fs (There x) = index (snd fs) x

genFunSU ∷ ∀ a b. Sing a → FunSU a b → Gen (FunSU a b)
genFunSU SNil         fs = pure ()
genFunSU (SCons a as) fs = (,) <$> gen (fst fs) <*> genFunSU @_ @b as (snd fs)

class MonadJoin m where
  join :: IsSOP a => m a -> m a

instance MonadJoin Gen where
  join ma = Gen \(k :: a -> Up r) -> runGen do
    let sa = singRep @a
    conts <- genFunSU @_ @r sa (tabulate sa (k . decode))
    a <- ma
    pure $ index conts (encode a)

instance (IsSOP e, MonadJoin m) => MonadJoin (ExceptT e m) where
  join (ExceptT ma) = ExceptT $ join ma

instance (MonadJoin m, IsSOP s) => MonadJoin (StateT s m) where
  join (StateT ma) = StateT (join . ma)

instance MonadJoin m => MonadJoin (MaybeT m) where
  join (MaybeT ma) = MaybeT $ join ma

instance (MonadJoin m) => MonadJoin (ReaderT r m) where
  join (ReaderT ma) = ReaderT (join . ma)
