{-# OPTIONS_GHC -Wno-name-shadowing -ddump-simpl -ddump-to-file -dsuppress-all -dno-suppress-type-signatures -dno-typeable-binds #-}
module Quicksort (quicksort) where

import DiffArray
import Data.Tuple (Solo (..))

-- swap :: Int -> Int -> DiffArray a -> DiffArray a
-- swap !i !j !xs = set i (xs ! j) (set j (xs ! i) xs)

swap :: Int -> Int -> DiffArray a -> DiffArray a
swap !i !j !xs =
  let
    -- using this strict matching on MkSolo we
    -- can ensure that the indexing happens
    -- before the mutation (which would slow
    -- down the indexing)
    !(MkSolo x) = index i xs
    !(MkSolo y) = index j xs
  in set i y (set j x xs)

{-# NOINLINE quicksort #-}
quicksort :: Ord a => Int -> Int -> DiffArray a -> DiffArray a
quicksort !l !r !xs
  | r - l <= 1 = xs
  | otherwise =
    let !(MkSolo x) = index (r - 1) xs in
    case partition l (r - 1) xs x of
      (xs, m) -> quicksort l m (quicksort (m + 1) r (swap (r - 1) m xs))

partition :: Ord a => Int -> Int -> DiffArray a -> a -> (DiffArray a, Int)
partition l r xs x = go xs l l where
  go !xs !m !i
    | i == r = (xs, m)
    | xs ! i <= x = go (swap i m xs) (m + 1) (i + 1)
    | otherwise = go xs m (i + 1)

-- >>> quicksort 0 5 (fromList [5,4,3,2,1])
-- fromList [1,2,3,4,5]

-- >>> quicksort 0 5 (fromList [3,1,5,2,4])
-- fromList [1,2,3,4,5]
