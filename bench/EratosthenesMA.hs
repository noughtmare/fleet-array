{-# OPTIONS_GHC -ddump-simpl -ddump-to-file -dsuppress-all -dno-suppress-type-signatures -dno-typeable-binds -ddump-stg-final -ddump-stg-from-core -ddump-prep #-}
{-# OPTIONS_GHC -O2 #-}
module EratosthenesMA where

import MutArr as MA

sieve :: Int -> IO [Int]
sieve n = do
  arr <- MA.replicate (n + 1) True
  go arr 2
  where
    go :: MutArr Bool -> Int -> IO [Int]
    go !ma !p
      | p > n = pure []
      | otherwise = do
        isPrime <- readMA ma p
        if isPrime then do
          go' ma p (p + p)
          xs <- go ma (p + 1)
          pure (p : xs)
        else
          go ma (p + 1)
    go' ma d i
      | i > n = pure ()
      | otherwise = do
        writeMA ma i False
        go' ma d (i + d)