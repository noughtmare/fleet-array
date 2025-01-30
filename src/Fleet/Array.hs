{-# LANGUAGE MagicHash, UnboxedTuples, UnliftedDatatypes #-}
{-# OPTIONS_GHC -ddump-simpl -ddump-to-file -dsuppress-all -dno-suppress-type-signatures -dno-typeable-binds #-}
-- O2 is necessary to get the right call pattern specializations and remove all the lifted abstractions
{-# OPTIONS_GHC -O2 #-}
{-# LANGUAGE LambdaCase #-}

{-|
Module      : Fleet.Array
Description : Fleet arrays
Copyright   : (c) Jaro Reinders, 2025
License     : BSD-3-Clause
Maintainer  : jaro.reinders@gmail.com
Stability   : experimental
Portability : Portable

This module defines fleet arrays and their basic interface.

All the asymptotic complexities listed in this module assume you are modifying
the latest version of the array. Otherwise the performance regresses to O(k),
where k is the number of changes between the version you are accessing and the
latest version.
-}
module Fleet.Array (Array, fromList, toList, (!), index, set, copy, swap, aseq) where

import Data.Tuple (Solo (MkSolo))
import GHC.Exts hiding (fromList, toList, Lifted)

import Data.Kind (Type)
import GHC.IO.Unsafe (unsafeDupablePerformIO)
import GHC.Base (IO(IO))

data Op a = Set {-# UNPACK #-} !Int a | Swap {-# UNPACK #-} !Int {-# UNPACK #-} !Int

-- | Fleet arrays.
data Array a = A {-# UNPACK #-} !(ArrayVar a)
type ArrayData# :: Type -> UnliftedType
data ArrayData# a
  = Current# {-# UNPACK #-} !(MutArray a)
  | Diff# {-# UNPACK #-} !(Op a) {-# UNPACK #-} !(ArrayVar a)

data ArrayData a = Current (MutArray a) | Diff !(Op a) !(ArrayVar a)

to# :: ArrayData a -> ArrayData# a
to# (Current x) = Current# x
to# (Diff op v) = Diff# op v

from# :: ArrayData# a -> ArrayData a
from# (Current# x) = Current x
from# (Diff# op v) = Diff op v

instance Show a => Show (Array a) where
  show xs = "fromList " ++ show (toList xs)

-- | Sequencing array operations.
aseq :: a -> b -> b
aseq x y = x `seq` lazy y

-- ArrayVar
data ArrayVar a = AV (MutVar# RealWorld (ArrayData# a))
newArrayVar :: ArrayData a -> IO (ArrayVar a)
newArrayVar x = IO $ \s ->
  case newMutVar# (to# x) s of
    (# s', v #) -> (# s', AV v #)

readArrayVar :: ArrayVar a -> IO (ArrayData a)
readArrayVar (AV v) = IO $ \s -> case readMutVar# v s of (# s', x #) -> (# s', from# x #)

writeArrayVar :: ArrayVar a -> ArrayData a -> IO ()
writeArrayVar (AV v) x = IO $ \s -> (# writeMutVar# v (to# x) s, () #)

-- MutArray

data MutArray a = MA (MutableArray# RealWorld a)

newMutArray :: Int -> a -> IO (MutArray a)
newMutArray (I# n) x = IO $ \s ->
  case newArray# n x s of
    (# s', arr #) -> (# s', MA arr #)

readMutArray :: MutArray a -> Int -> IO a
readMutArray (MA arr) (I# i) = IO (readArray# arr i)

writeMutArray :: MutArray a -> Int -> a -> IO ()
writeMutArray (MA arr) (I# i) x = IO (\s -> (# writeArray# arr i x s, () #))

-- | Convert a list into an array. O(n)
fromList :: [a] -> Array a
fromList xs = unsafeDupablePerformIO $ do
  arr0 <- newMutArray (length xs) undefined
  let go _ _ [] = pure ()
      go arr i (x:xs') = writeMutArray arr i x *> go arr (i + 1) xs'
  go arr0 0 xs
  v <- newArrayVar (Current arr0)
  pure (A v)

cloneMutArray :: MutArray a -> Int -> Int -> IO (MutArray a)
cloneMutArray (MA arr) (I# off) (I# len) = IO $ \s ->
  case cloneMutableArray# arr off len s of
    (# s', arr' #) -> (# s', MA arr' #)

sizeofMutArray :: MutArray a -> Int
sizeofMutArray (MA x) = I# (sizeofMutableArray# x)

copyInternal :: ArrayVar a -> IO (MutArray a)
copyInternal v = do
  av <- readArrayVar v
  case av of
    Current arr -> cloneMutArray arr 0 (sizeofMutArray arr)
    Diff op v' -> do
      clone <- copyInternal v'
      appOp clone op
      pure clone

-- | Converting an array into a list. O(n)
toList :: Array a -> [a]
toList (A v) = unsafeDupablePerformIO $ do
  arr <- copyInternal v
  let n = sizeofMutArray arr
      go i
        | i >= n = pure []
        | otherwise = do
            x <- readMutArray arr i
            xs <- go (i + 1)
            pure (x : xs)
  go 0

-- | Indexing an array. O(1)
{-# INLINE (!) #-}
(!) :: Array a -> Int -> a
A v0 ! i0 = unsafeDupablePerformIO (go v0 i0) where
  go v i = do
    dat <- readArrayVar v
    case dat of
      Current arr -> readMutArray arr i
      Diff (Set j x) v'
        | i == j -> pure x
        | otherwise -> go v' i
      Diff (Swap j1 j2) v'
        | i == j1 -> go v' j2
        | i == j2 -> go v' j1
        | otherwise -> go v' i

-- | Indexing an array. O(1)
-- Using the 'Solo' constructor, you can sequence indexing to happen before
-- future updates without having to evaluate the element itself.
{-# INLINE index #-}
index :: Int -> Array a -> Solo a
index i0 (A v0) = unsafeDupablePerformIO (go v0 i0) where
  go v i = do
    dat <- readArrayVar v
    case dat of
      Current arr -> MkSolo <$> readMutArray arr i
      Diff (Set j x) xs
        | i == j -> pure (MkSolo x)
        | otherwise -> go xs i
      Diff (Swap j1 j2) xs
        | i == j1 -> go xs j2
        | i == j2 -> go xs j1
        | otherwise -> go xs i

{-# INLINE invert #-}
invert :: MutArray a -> Op a -> IO (Op a)
invert _ (Swap i j) = pure (Swap i j)
invert arr (Set i _) = do
  y <- readMutArray arr i
  pure (Set i y)

{-# INLINE appOp #-}
appOp :: MutArray a -> Op a -> IO ()
appOp arr (Set i x) = writeMutArray arr i x
appOp arr (Swap i j) = do
  x <- readMutArray arr i
  y <- readMutArray arr j
  writeMutArray arr i y
  writeMutArray arr j x

{-# INLINE appDiffOp #-}
appDiffOp :: Op a -> Array a -> Array a
appDiffOp op (A v) = unsafeDupablePerformIO $ do
  dat <- readArrayVar v
  case dat of
    xs@(Current arr) -> do
      op' <- invert arr op
      appOp arr op
      v' <- newArrayVar xs
      writeArrayVar v (Diff op' v')
      pure (A v')
    Diff op' v' -> do
      -- TODO: pointer inversion instead of copy
      -- first invert all pointers until Current
      -- then apply all updates until back at v
      -- then do the same as above
      arr <- copyInternal v'
      appOp arr op'
      appOp arr op
      v'' <- newArrayVar (Current arr)
      pure (A v'')

-- | Update the array element at a given position to a new value. O(1)
{-# INLINE set #-}
set :: Int -> a -> Array a -> Array a
set i x = appDiffOp (Set i x)

-- | Swap two elements in an array. O(1)
{-# INLINE swap #-}
swap :: Int -> Int -> Array a -> Array a
swap i j = appDiffOp (Swap i j)

-- | Copy an array. O(n)
-- This detaches any future updates from old versions of the array.
-- Use this when you know you will be updating a large part of an array.
copy :: Array a -> Array a
copy (A v) = unsafeDupablePerformIO $ do
  arr <- copyInternal v
  var <- newArrayVar (Current arr)
  pure (A var)