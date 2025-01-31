{-# LANGUAGE MagicHash, UnboxedTuples #-}
module Fleet.Array.MutArray where

import GHC.Exts
import GHC.Base

data MutArray a = MA (MutableArray# RealWorld a)

newMutArray :: Int -> a -> IO (MutArray a)
newMutArray (I# n) x = IO $ \s ->
  case newArray# n x s of
    (# s', arr #) -> (# s', MA arr #)

readMutArray :: MutArray a -> Int -> IO a
readMutArray (MA arr) (I# i) = IO (readArray# arr i)

writeMutArray :: MutArray a -> Int -> a -> IO ()
writeMutArray (MA arr) (I# i) x = IO (\s -> (# writeArray# arr i x s, () #))

cloneMutArray :: MutArray a -> Int -> Int -> IO (MutArray a)
cloneMutArray (MA arr) (I# off) (I# len) = IO $ \s ->
  case cloneMutableArray# arr off len s of
    (# s', arr' #) -> (# s', MA arr' #)

sizeofMutArray :: MutArray a -> Int
sizeofMutArray (MA x) = I# (sizeofMutableArray# x)
