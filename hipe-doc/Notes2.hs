{-# language MagicHash, UnliftedDatatypes, UnboxedTuples #-}

module Notes2 (foo) where

import GHC.Exts

-- data Box = Box (Int# -> Int#)

-- foo :: Box -> Int#
-- foo (Box f) = f 100#

-- foo :: Bool -> Bool
-- foo True = False
-- foo x    = x

-- foo :: (Bool -> a) -> Bool -> a
-- foo f b = f (case b of True -> False; _ -> True)

-- data UBool :: UnliftedType where
--   UTrue :: UBool
--   UFalse :: UBool

-- foo :: (UBool -> a) -> UBool -> a
-- foo f b = f (case b of UTrue -> UFalse; _ -> UTrue)

-- -- pushes call continuation on stack
-- foo f x = f x x x x x x x x x x

-- foo (x :: Int#)(y :: Int#)(z :: Int#) = (# x, y, z #)

-- bar :: [Int] -> [Int]
-- bar x = 0 : x
-- {-# noinline bar #-}

-- foo (x :: [Int])(y :: Int#) =
--   let !kek = bar x
--   in I# y : kek

foo (x :: Int#) = x


{-
Passing cont in register in LLVM?




f x =
  let y = g x;
  let y' = g y;
  let y'' = h y y'
  ret y''

f0[](k, x) =
  push k;
  tcall g(&f1, x)

f1[k](y) =
  push y;
  tcall g(&f2, y)

f2[k, y](y') =
  y = pop
  k = pop
  tcall h(k, y, y')


-- f0[sp](k, x) =
--   tcall g[sp,k](f1, x)


f x = g x x

f[k](x) =
  tailcall g[k](x, x)    -- hmmm

- if we have tailcall, we just keep the stack or reset it, no pushin
- if we have non-tail call, we have to push ret addresses anyway

- pushing ret address on non-tail call is just fine
- we can try to share stax




    ...
    let y = g x        pass k to g, push
  k:
    ...

f [sp] k x =
  g x \[sp, k] y ->
  g y \[sp, k, y] y' ->
  h [sp] y y' k















-}





  -- f x y =
  --   ..
  --   ..
  --   ..
  --   g ...

  -- g x y =
  --   ..
  --   ..
  --   ..
  --   h ...
