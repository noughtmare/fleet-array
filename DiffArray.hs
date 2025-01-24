{-# LANGUAGE MagicHash, UnboxedTuples, UnliftedDatatypes #-}
{-# OPTIONS_GHC -Wall -Wno-name-shadowing #-}

module DiffArray (DiffArray, fromList, (!)) where

import GHC.Exts hiding (fromList)

import Data.Kind (Type)

data DiffArray a = DA (MutVar# RealWorld (DiffArrayData a))
type DiffArrayData :: Type -> UnliftedType
data DiffArrayData a
  = Here {-# UNPACK #-} !(MutableArray# RealWorld a)
  | There Int# a !(DiffArrayData a)

{-# NOINLINE fromList #-}
fromList :: [a] -> DiffArray a
fromList xs = DA (runRW# $ \s ->
  case newArray# (case length xs of I# x -> x) undefined s of
  { (# s , arr #) ->
  case newMutVar# (Here arr) (go arr 0# xs s) of
  { (# _ , x #) -> x
  }})
  where
    go _ _ [] s = s
    go arr i (x:xs) s = go arr (i +# 1#) xs (writeArray# arr i x s)

(!) :: DiffArray a -> Int -> a
(!) (DA v) (I# i) = runRW# $ \s ->
  case readMutVar# v s of
    (# s , Here arr #) ->
      case readArray# arr i s of
        (# _ , x #) -> x
    (# _ , There j x xs #)
      | isTrue# (i ==# j) -> x
      | otherwise ->
        let
          go (Here arr) = case readArray# arr i s of (# _ , x #) -> x
          go (There j x xs)
            | isTrue# (i ==# j) = x
            | otherwise = go xs
        in go xs

set :: Int -> a -> DiffArray a -> DiffArray a
set (I# i) x (DA v) = runRW# $ \s ->
  case readMutVar# v s of
    (# s , xs@(Here arr) #) ->
      case newMutVar# xs (writeMutVar# v (There i x xs) (writeArray# arr i x s)) of
        (# _ , v' #) -> DA v'
    (# s, xs #) -> case newMutVar# (There i x xs) s of (# _ , v' #) -> DA v'
