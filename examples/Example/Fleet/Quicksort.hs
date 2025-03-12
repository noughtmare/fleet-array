{-# OPTIONS_GHC -Wno-name-shadowing -ddump-simpl -ddump-to-file -dsuppress-all -dno-suppress-type-signatures -dno-typeable-binds #-}
module Example.Fleet.Quicksort (quicksort) where

import Fleet.Array

{-# INLINEABLE quicksort #-}
quicksort :: Ord a => Int -> Int -> Array a -> Array a
quicksort !l !r !xs
  | r - l <= 1 = xs
  | otherwise =
    let (x', t) = index (r - 1) xs in
    case partition l (r - 1) (tag t xs) x' of
      (xs, m) -> quicksort l m (quicksort (m + 1) r (swap (r - 1) m xs))

{-# INLINEABLE partition #-}
partition :: Ord a => Int -> Int -> Array a -> a -> (Array a, Int)
partition l r xs x = go xs l l where
  go !xs !m !i
    | i == r = (xs, m)
    | xs ! i <= x = go (swap i m xs) (m + 1) (i + 1)
    | otherwise = go xs m (i + 1)

-- >>> quicksort 0 5 (fromList [5,4,3,2,1])
-- fromList [1,2,3,4,5]

-- >>> quicksort 0 5 (fromList [3,1,5,2,4])
-- fromList [1,2,3,4,5]
