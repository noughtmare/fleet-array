{-# LANGUAGE MagicHash, UnboxedTuples #-}
{-# OPTIONS_GHC -Wno-name-shadowing -ddump-simpl -ddump-to-file -dsuppress-all -dno-suppress-type-signatures -dno-typeable-binds #-}
module QuicksortMA (MutArr, fromList, toList, clone, quicksort) where

import MutArr

{-# INLINEABLE quicksort #-}
quicksort :: Ord a => MutArr a -> Int -> Int -> IO ()
quicksort !arr !l !r
  | r - l <= 1 = pure ()
  | otherwise = do
    x <- readMA arr (r - 1)
    m <- partition arr l (r - 1) x
    swap arr (r - 1) m
    quicksort arr (m + 1) r
    quicksort arr l m

{-# INLINEABLE partition #-}
partition :: Ord a => MutArr a -> Int -> Int -> a -> IO Int
partition arr l r x = go arr l l where
  go !arr !m !i
    | i == r = pure m
    | otherwise = do
      y <- readMA arr i
      if y <= x then do
        swap arr i m
        go arr (m + 1) (i + 1)
      else go arr m (i + 1)