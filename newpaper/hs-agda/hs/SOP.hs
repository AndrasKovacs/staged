
{-# language
  DataKinds, PolyKinds, GADTs, TypeFamilies, TypeApplications,
  ScopedTypeVariables, TypeOperators, AllowAmbiguousTypes,
  TypeFamilyDependencies, UndecidableInstances, BlockArguments,
  DefaultSignatures, TemplateHaskell
  #-}

{-# options_ghc -Wincomplete-patterns #-}

module SOP where

import Data.Kind
import Up

--------------------------------------------------------------------------------

data family Sing (a :: k)

data instance Sing (a :: Type) where
  SType :: Sing a

data instance Sing (a :: [k]) where
  SNil  :: Sing '[]
  SCons :: forall x xs. Sing x -> Sing xs -> Sing (x ': xs)

class                           SingI a           where sing :: Sing a
instance                        SingI (a :: Type) where sing = SType
instance                        SingI '[]         where sing = SNil
instance (SingI a, SingI as) => SingI (a ': as)   where sing = SCons sing sing

type family (++) (xs :: [k]) (ys :: [k]) :: [k] where
  (++) '[]       ys = ys
  (++) (x ': xs) ys = x ': (++) xs ys

infixr 5 %++
(%++) :: Sing xs -> Sing ys -> Sing (xs ++ ys)
(%++) SNil         ys = ys
(%++) (SCons x xs) ys = SCons x (xs %++ ys)

data Id :: forall k. k -> k -> Type where
  Refl :: forall k (x :: k). Id x x

--------------------------------------------------------------------------------

data P :: [Type] -> Type where
  Nil  :: P '[]
  Cons :: forall a as. Up a -> P as -> P (a ': as)

data S :: [[Type]] -> Type where
  Here  :: forall as ass. P as -> S (as ': ass)
  There :: forall as ass. S ass -> S (as ': ass)

-- ProdP
--------------------------------------------------------------------------------

type ProdP :: [Type] -> [Type] -> [Type]
type ProdP as bs = as ++ bs

sProdP :: Sing as -> Sing bs -> Sing (ProdP as bs)
sProdP = (%++)

pairP :: P as -> P bs -> P (ProdP as bs)
pairP Nil         ys = ys
pairP (Cons x xs) ys = Cons x (pairP xs ys)

fstP :: forall as bs. Sing as -> P (ProdP as bs) -> P as
fstP SNil         xs          = Nil
fstP (SCons a as) (Cons x xs) = Cons x (fstP @_ @bs as xs)

sndP :: forall as bs. Sing as -> P (ProdP as bs) -> P bs
sndP SNil         xs          = xs
sndP (SCons a as) (Cons x xs) = sndP @_ @bs as xs

-- SumS
--------------------------------------------------------------------------------

type SumS :: [[Type]] -> [[Type]] -> [[Type]]
type SumS ass bss = ass ++ bss

sSumS :: Sing ass -> Sing bss -> Sing (SumS ass bss)
sSumS = (%++)

leftS :: forall ass bss. S ass -> S (SumS ass bss)
leftS (Here xs)  = Here xs
leftS (There xs) = There (leftS @_ @bss xs)

rightS :: forall ass bss. Sing ass -> S bss -> S (SumS ass bss)
rightS SNil         y = y
rightS (SCons x xs) y = There (rightS xs y)

eitherS :: forall ass bss. Sing ass -> S (SumS ass bss) -> Either (S ass) (S bss)
eitherS SNil         y         = Right y
eitherS (SCons x xs) (Here y)  = Left (Here y)
eitherS (SCons x xs) (There y) = case eitherS @_ @bss xs y of
  Left y  -> Left (There y)
  Right y -> Right y

-- Product of a P and an S
--------------------------------------------------------------------------------

type family ProdPS (as :: [Type])(ass :: [[Type]]) where
  ProdPS as '[]         = '[]
  ProdPS as (bs ': bss) = ProdP as bs ': ProdPS as bss

sProdPS :: Sing as -> Sing ass -> Sing (ProdPS as ass)
sProdPS as SNil           = SNil
sProdPS as (SCons bs bss) = SCons (sProdP as bs) (sProdPS as bss)

pairPS :: P as -> S bss -> S (ProdPS as bss)
pairPS xs (Here ys)  = Here (pairP xs ys)
pairPS xs (There ys) = There (pairPS xs ys)

unpairPS :: Sing as -> Sing bss -> S (ProdPS as bss) -> (P as, S bss)
unpairPS as SNil              ys = case ys of
unpairPS as (SCons @bs _ bss) ys = case ys of
  Here ys  -> (fstP @_ @bs as ys, Here (sndP @_ @bs as ys))
  There ys -> case unpairPS as bss ys of (x, y) -> (x, There y)

-- ProdS
--------------------------------------------------------------------------------

type family ProdS (ass :: [[Type]]) (bss :: [[Type]]) :: [[Type]] where
  ProdS '[]         bss = '[]
  ProdS (as ': ass) bss = ProdPS as bss ++ ProdS ass bss

sProdS :: Sing ass -> Sing bss -> Sing (ProdS ass bss)
sProdS SNil           bss = SNil
sProdS (SCons as ass) bss = sProdPS as bss %++ sProdS ass bss

pairS :: Sing ass -> Sing bss -> S ass -> S bss -> S (ProdS ass bss)
pairS ass (bss :: Sing bss) xs ys = case ass of
  SNil                  -> case xs of
  SCons @as @ass as ass -> case xs of
    Here xs  -> leftS  @(ProdPS as bss) @(ProdS ass bss) (pairPS xs ys)
    There xs -> rightS @_ @(ProdS ass bss) (sProdPS as bss) (pairS ass bss xs ys)

unpairS :: Sing ass -> Sing bss -> S (ProdS ass bss) -> (S ass, S bss)
unpairS ass (bss :: Sing bss) xs = case ass of
  SNil                  -> case xs of
  SCons @as @ass as ass -> case eitherS @_ @(ProdS ass bss) (sProdPS as bss) xs of
    Left x  -> case unpairPS as bss x of (x, y) -> (Here x, y)
    Right x -> case unpairS ass bss x of (x, y) -> (There x, y)

-- Functions from P to Up
--------------------------------------------------------------------------------

type family FunP (as :: [Type]) (b :: Type) :: Type where
  FunP '[]       b = () -> b
  FunP (a ': as) b = a -> FunP as b

lamP :: Sing as -> (P as -> Up b) -> Up (FunP as b)
lamP SNil         f = [|| \_ -> $$(f Nil) ||]
lamP (SCons a as) f = [|| \x -> $$(lamP as (\xs -> f (Cons [||x||] xs))) ||]

appP :: Up (FunP as b) -> P as -> Up b
appP f Nil         = [|| $$f () ||]
appP f (Cons x xs) = [|| $$(appP [|| $$f $$x ||] xs) ||]

-- Functions from S to Up
--------------------------------------------------------------------------------

type family FunS (ass :: [[Type]]) (b :: Type) :: [Type] where
  FunS '[]         b = '[]
  FunS (as ': ass) b = FunP as b ': FunS ass b

tabulate :: forall ass b. Sing ass -> (S ass -> Up b) -> P (FunS ass b)
tabulate SNil           f = Nil
tabulate (SCons as ass) f = Cons (lamP as (f . Here)) (tabulate ass (f . There))

index :: P (FunS ass b) -> S ass -> Up b
index Nil         x         = case x of
index (Cons f fs) (Here x)  = appP f x
index (Cons f fs) (There x) = index fs x

--------------------------------------------------------------------------------

class IsSOP (a :: Type) where
  type Rep a :: [[Type]]
  singRep :: Sing (Rep a)
  encode  :: a -> S (Rep a)
  decode  :: S (Rep a) -> a

instance IsSOP (Up a) where
  type Rep (Up a) = '[ '[ a ]]
  singRep = sing
  encode x = Here (Cons x Nil)
  decode (Here (Cons x Nil)) = x
  decode (There x) = case x of

instance SingI as => IsSOP (P as) where
  type Rep (P as) = '[ as ]
  singRep = sing
  encode x = Here x
  decode (Here x) = x
  decode (There x) = case x of

instance SingI ass => IsSOP (S ass) where
  type Rep (S ass) = ass
  singRep = sing
  encode = id
  decode = id

instance IsSOP () where
  type Rep () = '[ '[] ]
  singRep = sing
  encode _ = Here Nil
  decode _ = ()

instance IsSOP a => IsSOP (Maybe a) where
  type Rep (Maybe a) = SumS (Rep ()) (Rep a)
  singRep = SCons SNil (singRep @a)
  encode Nothing    = Here Nil
  encode (Just a)   = There (encode a)
  decode (Here Nil) = Nothing
  decode (There a)  = Just (decode a)

instance (IsSOP a, IsSOP b) => IsSOP (Either a b) where
  type Rep (Either a b) = SumS (Rep a) (Rep b)
  singRep = sSumS (singRep @a) (singRep @b)
  encode (Left  a) = leftS  @(Rep a) @(Rep b) (encode a)
  encode (Right b) = rightS @(Rep a) @(Rep b) (singRep @a) (encode b)
  decode x = case eitherS @(Rep a) @(Rep b) (singRep @a) x of
    Left a  -> Left (decode a)
    Right b -> Right (decode b)

instance (IsSOP a, IsSOP b) => IsSOP (a, b) where
  type Rep (a, b) = ProdS (Rep a) (Rep b)
  singRep = sProdS (singRep @a) (singRep @b)
  encode (a, b) = pairS (singRep @a) (singRep @b) (encode a) (encode b)
  decode x = case unpairS (singRep @a) (singRep @b) x of
    (a, b) -> (decode a, decode b)

instance (IsSOP a, IsSOP b, IsSOP c) => IsSOP (a, b, c) where
  type Rep (a, b, c) = ProdS (Rep a) (Rep (b, c))
  singRep = sProdS (singRep @a) (singRep @(b, c))
  encode (a, b, c) = pairS (singRep @a) (singRep @(b, c)) (encode a) (encode (b, c))
  decode x = case unpairS (singRep @a) (singRep @(b, c)) x of
    (a, bc) -> case decode bc of (b, c) -> (decode a, b, c)

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
  decode x = case decode x of (a, b) -> Pair a b

--------------------------------------------------------------------------------
