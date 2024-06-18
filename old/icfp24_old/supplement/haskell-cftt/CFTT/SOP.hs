
-- GHC's coverage checker is buggy, we get some superfluous warnings that we
-- want to disable this way. It's a good idea though to enable the warnings
-- when we modify this module.
{-# options_ghc -Wno-incomplete-patterns #-}

module CFTT.SOP where

import Data.Kind
import CFTT.Up

-- Singletons
--------------------------------------------------------------------------------

data family Sing (a ∷ k)

data instance Sing (a ∷ Type) where
  SType ∷ Sing a

data instance Sing (a ∷ [k]) where
  SNil  ∷ Sing '[]
  SCons ∷ ∀ x xs. Sing x → Sing xs → Sing (x ': xs)

class                           SingI a          where sing ∷ Sing a
instance                        SingI (a ∷ Type) where sing = SType
instance                        SingI '[]        where sing = SNil
instance (SingI a, SingI as) => SingI (a ': as)  where sing = SCons sing sing

type family (++) (xs ∷ [k]) (ys ∷ [k]) ∷ [k] where
  (++) '[]       ys = ys
  (++) (x ': xs) ys = x ': (++) xs ys

infixr 5 %++
(%++) ∷ Sing xs → Sing ys → Sing (xs ++ ys)
(%++) SNil         ys = ys
(%++) (SCons x xs) ys = SCons x (xs %++ ys)

--------------------------------------------------------------------------------

type Uₚ = [Type]
type Uₛ = [Uₚ]

data Elₚ ∷ Uₚ → Type where
  Nil  ∷ Elₚ '[]
  Cons ∷ ∀ a as. Up a → Elₚ as → Elₚ (a ': as)

data Elₛ ∷ Uₛ → Type where
  Here  ∷ ∀ as ass. Elₚ as  → Elₛ (as ': ass)
  There ∷ ∀ as ass. Elₛ ass → Elₛ (as ': ass)

-- Product type in Uₚ
--------------------------------------------------------------------------------

type Prodₚ ∷ Uₚ → Uₚ → Uₚ
type Prodₚ as bs = as ++ bs

sProdₚ ∷ Sing a → Sing b → Sing (Prodₚ a b)
sProdₚ = (%++)

pairₚ ∷ Elₚ a → Elₚ b → Elₚ (Prodₚ a b)
pairₚ Nil         ys = ys
pairₚ (Cons x xs) ys = Cons x (pairₚ xs ys)

fstₚ ∷ ∀ a b. Sing a → Elₚ (Prodₚ a b) → Elₚ a
fstₚ SNil         xs          = Nil
fstₚ (SCons a as) (Cons x xs) = Cons x (fstₚ @_ @b as xs)

sndₚ ∷ ∀ a b. Sing a → Elₚ (Prodₚ a b) → Elₚ b
sndₚ SNil         xs          = xs
sndₚ (SCons a as) (Cons x xs) = sndₚ @_ @b as xs

-- Sum type in Uₛ
--------------------------------------------------------------------------------

type Sumₛ ∷ Uₛ → Uₛ → Uₛ
type Sumₛ a b = a ++ b

sSumₛ ∷ Sing a → Sing b → Sing (Sumₛ a b)
sSumₛ = (%++)

leftₛ ∷ ∀ a b. Elₛ a → Elₛ (Sumₛ a b)
leftₛ (Here xs)  = Here xs
leftₛ (There xs) = There (leftₛ @_ @b xs)

rightₛ ∷ ∀ a b. Sing a → Elₛ b → Elₛ (Sumₛ a b)
rightₛ SNil         y = y
rightₛ (SCons x xs) y = There (rightₛ xs y)

eitherₛ ∷ ∀ a b. Sing a → Elₛ (Sumₛ a b) → Either (Elₛ a) (Elₛ b)
eitherₛ SNil         y         = Right y
eitherₛ (SCons x xs) (Here y)  = Left (Here y)
eitherₛ (SCons x xs) (There y) = case eitherₛ @_ @b xs y of
  Left y  → Left (There y)
  Right y → Right y

-- Product of an Uₚ and an Uₛ
--------------------------------------------------------------------------------

type family Prodₚₛ (a ∷ Uₚ) (b ∷ Uₛ) ∷ Uₛ where
  Prodₚₛ a '[]       = '[]
  Prodₚₛ a (b ': bs) = Prodₚ a b ': Prodₚₛ a bs

sProdₚₛ ∷ Sing a → Sing b → Sing (Prodₚₛ a b)
sProdₚₛ a SNil         = SNil
sProdₚₛ a (SCons b bs) = SCons (sProdₚ a b) (sProdₚₛ a bs)

pairₚₛ ∷ Elₚ a → Elₛ b → Elₛ (Prodₚₛ a b)
pairₚₛ xs (Here ys)  = Here (pairₚ xs ys)
pairₚₛ xs (There ys) = There (pairₚₛ xs ys)

unpairₚₛ ∷ Sing a → Sing b → Elₛ (Prodₚₛ a b) → (Elₚ a, Elₛ b)
unpairₚₛ a (SCons @bs _ bss) ys = case ys of
  Here ys  → (fstₚ @_ @bs a ys, Here (sndₚ @_ @bs a ys))
  There ys → case unpairₚₛ a bss ys of (x, y) → (x, There y)

-- Product in Uₛ
--------------------------------------------------------------------------------

type family Prodₛ (a ∷ Uₛ) (b ∷ Uₛ) ∷ Uₛ where
  Prodₛ '[]       b = '[]
  Prodₛ (a ': as) b = Prodₚₛ a b ++ Prodₛ as b

sProdₛ ∷ Sing a → Sing b → Sing (Prodₛ a b)
sProdₛ SNil         b = SNil
sProdₛ (SCons a as) b = sProdₚₛ a b %++ sProdₛ as b

pairₛ ∷ Sing a → Sing b → Elₛ a → Elₛ b → Elₛ (Prodₛ a b)
pairₛ a (b ∷ Sing b) xs ys = case a of
  SCons @as @a as a → case xs of
    Here xs  → leftₛ  @(Prodₚₛ as b) @(Prodₛ a b) (pairₚₛ xs ys)
    There xs → rightₛ @_ @(Prodₛ a b) (sProdₚₛ as b) (pairₛ a b xs ys)

unpairₛ ∷ Sing a → Sing bss → Elₛ (Prodₛ a bss) → (Elₛ a, Elₛ bss)
unpairₛ a (b ∷ Sing b) xs = case a of
  SCons @as @a as a → case eitherₛ @_ @(Prodₛ a b) (sProdₚₛ as b) xs of
    Left x  → case unpairₚₛ as b x of (x, y) → (Here x, y)
    Right x → case unpairₛ a b x   of (x, y) → (There x, y)

-- Functions from P to Up
--------------------------------------------------------------------------------

type family Funₚₜ (as ∷ Uₚ) (b ∷ Type) ∷ Type where
  Funₚₜ '[]             b = () → b
  Funₚₜ (a ': '[])      b = a → b
  Funₚₜ (a ': a2 ': as) b = a → Funₚₜ (a2 ': as) b

lamₚₜ ∷ ∀ a b. Sing a → (Elₚ a → Up b) → Up (Funₚₜ a b)
lamₚₜ SNil                   f = [|| \_ -> $$(f Nil) ||]
lamₚₜ (SCons a SNil)         f = [|| \x -> $$(f (Cons [||x||] Nil)) ||]
lamₚₜ (SCons a as@(SCons{})) f = [|| \x -> $$(lamₚₜ @_ @b as \xs -> f (Cons [||x||] xs)) ||]

appₚₜ ∷ ∀ a b. Up (Funₚₜ a b) → Elₚ a → Up b
appₚₜ f Nil                  = [|| $$f () ||]
appₚₜ f (Cons x Nil)         = [|| $$f $$x ||]
appₚₜ f (Cons x xs@(Cons{})) = [|| $$(appₚₜ [|| $$f $$x ||] xs) ||]

--------------------------------------------------------------------------------

class IsSOP (a ∷ Type) where
  type Rep a ∷ Uₛ
  singRep ∷ Sing (Rep a)
  encode  ∷ a → Elₛ (Rep a)
  decode  ∷ Elₛ (Rep a) → a

instance IsSOP (Up a) where
  type Rep (Up a) = '[ '[ a ]]
  singRep = sing
  encode x = Here (Cons x Nil)
  decode (Here (Cons x Nil)) = x

instance SingI as => IsSOP (Elₚ as) where
  type Rep (Elₚ as) = '[ as ]
  singRep = sing
  encode x = Here x
  decode (Here x) = x

instance SingI ass => IsSOP (Elₛ ass) where
  type Rep (Elₛ ass) = ass
  singRep = sing
  encode = id
  decode = id

instance IsSOP () where
  type Rep () = '[ '[] ]
  singRep = sing
  encode _ = Here Nil
  decode _ = ()

instance IsSOP a => IsSOP (Maybe a) where
  type Rep (Maybe a) = Sumₛ (Rep ()) (Rep a)
  singRep = SCons SNil (singRep @a)
  encode Nothing    = Here Nil
  encode (Just a)   = There (encode a)
  decode (Here Nil) = Nothing
  decode (There a)  = Just (decode a)

instance (IsSOP a, IsSOP b) => IsSOP (Either a b) where
  type Rep (Either a b) = Sumₛ (Rep a) (Rep b)
  singRep = sSumₛ (singRep @a) (singRep @b)
  encode (Left  a) = leftₛ  @(Rep a) @(Rep b) (encode a)
  encode (Right b) = rightₛ @(Rep a) @(Rep b) (singRep @a) (encode b)
  decode x = case eitherₛ @(Rep a) @(Rep b) (singRep @a) x of
    Left a  → Left (decode a)
    Right b → Right (decode b)

instance (IsSOP a, IsSOP b) => IsSOP (a, b) where
  type Rep (a, b) = Prodₛ (Rep a) (Rep b)
  singRep = sProdₛ (singRep @a) (singRep @b)
  encode (a, b) = pairₛ (singRep @a) (singRep @b) (encode a) (encode b)
  decode x = case unpairₛ (singRep @a) (singRep @b) x of
    (a, b) → (decode a, decode b)

instance (IsSOP a, IsSOP b, IsSOP c) => IsSOP (a, b, c) where
  type Rep (a, b, c) = Prodₛ (Rep a) (Rep (b, c))
  singRep = sProdₛ (singRep @a) (singRep @(b, c))
  encode (a, b, c) = pairₛ (singRep @a) (singRep @(b, c)) (encode a) (encode (b, c))
  decode x = case unpairₛ (singRep @a) (singRep @(b, c)) x of
    (a, bc) → case decode bc of (b, c) → (decode a, b, c)

instance IsSOP Bool where
  type Rep Bool = [ '[], '[] ]
  singRep = sing
  encode True = Here Nil
  encode _    = There (Here Nil)
  decode (Here Nil) = True
  decode _          = False

instance (IsSOP a, IsSOP b) => IsSOP (Pair a b) where
  type Rep (Pair a b) = Rep (a, b)
  singRep = singRep @(a, b)
  encode (Pair a b) = encode (a, b)
  decode x = case decode x of (a, b) → Pair a b
