{-# OPTIONS_GHC -Wno-name-shadowing -ddump-simpl -ddump-to-file -dsuppress-all -dno-suppress-type-signatures -dno-typeable-binds #-}
module QuicksortIM (fromList, quicksort) where

import qualified Data.IntMap as IntMap
import Data.IntMap (IntMap)

newtype MutArr a = MA (IntMap a)

readMA :: MutArr a -> Int -> a
readMA (MA m) i = m IntMap.! i

writeMA :: Int -> a -> MutArr a -> MutArr a
writeMA i x (MA m) = MA (IntMap.insert i x m)

fromList :: [a] -> MutArr a
fromList xs = MA (IntMap.fromDistinctAscList (zip [0..] xs))

toList :: MutArr a -> [a]
toList (MA m) = map snd (IntMap.toList m)

swap :: Int -> Int -> MutArr a -> MutArr a
swap !i !j !arr =
  let
    x = readMA arr i
    y = readMA arr j
  in writeMA j x (writeMA i y arr)

{-# INLINEABLE quicksort #-}
quicksort :: Ord a => Int -> Int -> MutArr a -> MutArr a
quicksort !l !r !arr 
  | r - l <= 1 = arr
  | otherwise =
    let
      x = readMA arr (r - 1)
      (arr1, m) = partition l (r - 1) x arr
      arr2 = swap (r - 1) m arr1
      arr3 = quicksort (m + 1) r arr2
    in
      quicksort l m arr3

{-# INLINEABLE partition #-}
partition :: Ord a => Int -> Int -> a -> MutArr a -> (MutArr a, Int)
partition l r x = go l l where
  go !m !i !arr
    | i == r = (arr, m)
    | otherwise =
      if readMA arr i <= x then
        go (m + 1) (i + 1) (swap i m arr)
      else go m (i + 1) arr