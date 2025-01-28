{-# LANGUAGE MagicHash, UnboxedTuples #-}
module Array (Array, fromList, (!)) where

import GHC.Exts hiding (fromList)

data Array a = A (MutableArray# RealWorld a)

fromList :: [a] -> Array a
fromList xs = A (runRW# $ \s ->
  case newArray# (case length xs of I# x -> x) undefined s of
  { (# s , arr #) -> go arr 0# xs s
  })
  where
    go arr _ [] _ = arr
    go arr i (x:xs) s = go arr (i +# 1#) xs (writeArray# arr i x s)

(!) :: Array a -> Int -> a
(!) (A arr) (I# i) = runRW# $ \s ->
  case readArray# arr i s of
    (# _ , x #) -> x