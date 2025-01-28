{-# LANGUAGE MagicHash, UnboxedTuples #-}
{-# OPTIONS_GHC -Wno-name-shadowing -ddump-simpl -ddump-to-file -dsuppress-all -dno-suppress-type-signatures -dno-typeable-binds #-}
module QuicksortA (MutArr, fromList, toList, clone, quicksort) where

import GHC.Exts hiding (fromList, toList)
import GHC.IO (IO (IO))

data MutArr a = MA (MutableArray# RealWorld a)

readMA :: MutArr a -> Int -> IO a
readMA (MA arr) (I# i) = IO $ \s -> readArray# arr i s

writeMA :: MutArr a -> Int -> a -> IO ()
writeMA (MA arr) (I# i) x = IO $ \s -> (# writeArray# arr i x s , () #)

fromList :: [a] -> IO (MutArr a)
fromList xs = IO $ \s ->
  case newArray# (case length xs of I# x -> x) undefined s of
  { (# s , arr #) -> go arr 0# xs s
  }
  where
    go arr _ [] s = (# s, MA arr #)
    go arr i (x:xs) s = go arr (i +# 1#) xs (writeArray# arr i x s)

toList :: MutArr a -> IO [a]
toList (MA arr) = IO $ \s ->
  let
    n = sizeofMutableArray# arr
    go i s
      | isTrue# (i >=# n) = (# s, [] #)
      | otherwise =
        case readArray# arr i s of
        { (# s, x #) ->
        case go (i +# 1#) s of
        { (# s, xs #) -> (# s, x : xs #)
        }}
  in go 0# s

clone :: MutArr a -> IO (MutArr a)
clone (MA arr) = IO $ \s ->
  case cloneMutableArray# arr 0# (sizeofMutableArray# arr) s of
  { (# s, arr' #) -> (# s , MA arr' #)
  }

swap :: MutArr a -> Int -> Int -> IO ()
swap !arr !i !j = do
  x <- readMA arr i
  y <- readMA arr j
  writeMA arr i y
  writeMA arr j x

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