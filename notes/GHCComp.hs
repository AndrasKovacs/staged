{-# language MagicHash #-}

module GHCComp where

import GHC.Exts

-- data List = Nil | Cons Int# !List

-- foo :: List -> List
-- foo Nil = Nil
-- foo (Cons x xs) = Cons (x *# x) (foo xs)

-- foo :: Int# -> Int# -> Int# -> Int# -> Int#
-- foo a b c d = a *# b *# c *# d

bar :: Int# -> Int# -> Int#
bar a b = a *# b
{-# noinline bar #-}

foo :: Int# -> Int# -> Int#
foo a b = a *# (bar a b)
