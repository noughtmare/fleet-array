{-# LANGUAGE MagicHash, UnboxedTuples #-}
module Example.MutArr.MutArr where

import GHC.Exts
import GHC.Base

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

replicate :: Int -> a -> IO (MutArr a)
replicate (I# n) x = IO $ \s ->
  case newArray# n x s of
    (# s', arr #) -> (# s', MA arr #)

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
