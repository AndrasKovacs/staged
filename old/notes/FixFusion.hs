
{-# language
  StandaloneDeriving, DeriveFunctor, GADTs, TypeFamilies, AllowAmbiguousTypes, TypeApplications,
  Strict, LambdaCase, DataKinds, PolyKinds, RankNTypes, ScopedTypeVariables, BlockArguments,
  PartialTypeSignatures, UnicodeSyntax
  #-}

{-# options_ghc -Wno-unused-imports #-}

newtype Fix f = Fix {fold :: ∀ x. (f x → x) → x}

newtype Comp f g a = Comp {unComp :: f (g a)}

data ListF a b = NilF | ConsF a b deriving (Show, Functor)

fuse1 :: (∀ x. (g x → x) → (f x → x)) → Fix f → Fix g
fuse1 f g = Fix \con → fold g (f con)

fuse2 :: (∀ x. (g x → x) → (f (a → x) → a → x)) → Fix f → a → Fix g
fuse2 f g a = Fix \con → fold g (f con) a

type List a = Fix (ListF a)

fuse3 :: (∀ x. (g x → x) → (f (h x) → h x)) → Fix f → h (Fix g)
fuse3 f g = _

rev :: List a -> List a
rev as = Fix \con →
  fold as
    (\case NilF      → \acc → acc
           ConsF a k → \acc → k (con (ConsF a acc)))
    (con NilF)



toList :: List a -> [a]
toList as = fold as \case
  NilF       → []
  ConsF a as → a : as

fromList :: [a] -> List a
fromList as = Fix \con -> foldr (\a x -> con (ConsF a x)) (con NilF) as

instance Show a => Show (List a) where
  show = show . toList




-- data ListF a k = NilF | ConsF a k deriving (Show, Functor)

-- fold :: Functor f ⇒ (f a → a) -> Fix f → a
-- fold f (Fix ff) = let go = fold f in f (go <$> ff)

-- fuse1 :: (Functor f, Functor g) ⇒ (∀ x. (g x → x) → (f x → x)) → Fix f → Fix g
-- fuse1 f (Fix ff) = _
