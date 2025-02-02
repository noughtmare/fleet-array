{-# OPTIONS_GHC -O2 #-}
module Eratosthenes where

import Fleet.Array as Fleet

sieve :: Int -> [Int]
sieve n = go 2 (Fleet.replicate (n + 1) True) where
  go !p !xs
    | p > n = []
    | xs ! p = p : go (p + 1) (go' p (p + p) xs)
    | otherwise = go (p + 1) xs
  go' !d !i !xs
    | i > n = xs
    | otherwise = go' d (i + d) (if xs ! i then set i False xs else xs)