{-# LANGUAGE MagicHash, UnboxedTuples, UnliftedDatatypes #-}
{-# OPTIONS_GHC -Wno-name-shadowing -ddump-simpl -ddump-to-file -dsuppress-all -dno-suppress-type-signatures -dno-typeable-binds #-}
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
import GHC.Base (IO(IO), unIO)

data Op a = Set Int# a | Swap Int# Int#

-- | Fleet arrays.
data Array a = DA (MutVar# RealWorld (ArrayData a))
type ArrayData :: Type -> UnliftedType
data ArrayData a
  = Current (MutableArray# RealWorld a)
  | Diff {-# UNPACK #-} !(Op a) (MutVar# RealWorld (ArrayData a))

instance Show a => Show (Array a) where
  show xs = "fromList " ++ show (toList xs)

-- | Sequencing array operations.
aseq :: a -> b -> b
aseq x y = x `seq` lazy y

type Lifted :: UnliftedType -> Type
data Lifted a = Lifted a

{-# INLINE newMutVarIO #-}
newMutVarIO :: forall (a :: UnliftedType). a -> IO (Lifted (MutVar# RealWorld a))
newMutVarIO x = IO $ \s ->
  case newMutVar# x s of
    (# s', v #) -> (# s', Lifted v #)

{-# INLINE readMutVarIO #-}
readMutVarIO :: forall (a :: UnliftedType) b. MutVar# RealWorld a -> (a -> IO b) -> IO b
readMutVarIO v f = IO (\s -> case readMutVar# v s of (# s', x #) -> unIO (f x) s')

{-# INLINE writeMutVarIO #-}
writeMutVarIO :: forall (a :: UnliftedType). MutVar# RealWorld a -> a -> IO ()
writeMutVarIO v x = IO (\s -> (# writeMutVar# v x s, () #))

readArrayIO :: MutableArray# RealWorld a -> Int# -> IO a
readArrayIO arr i = IO (readArray# arr i)

writeArrayIO :: MutableArray# RealWorld a -> Int# -> a -> IO ()
writeArrayIO arr i x = IO (\s -> (# writeArray# arr i x s, () #))

newArrayIO :: Int# -> a -> IO (Lifted (MutableArray# RealWorld a))
newArrayIO n x = IO $ \s ->
  case newArray# n x s of
    (# s', arr #) -> (# s', Lifted arr #)

-- | Convert a list into an array. O(n)
fromList :: [a] -> Array a
fromList xs = unsafeDupablePerformIO $ do
  let !(I# n) = length xs
  Lifted arr <- newArrayIO n undefined
  let go _ _ [] = pure ()
      go arr i (x:xs') = writeArrayIO arr i x >> go arr (i +# 1#) xs'
  go arr 0# xs
  Lifted var <- newMutVarIO (Current arr)
  pure (DA var)

cloneMutableArrayIO :: MutableArray# RealWorld a -> Int# -> Int# -> IO (Lifted (MutableArray# RealWorld a))
cloneMutableArrayIO arr off len = IO $ \s ->
  case cloneMutableArray# arr off len s of
    (# s', arr' #) -> (# s', Lifted arr' #)

copyInternalIO :: MutVar# RealWorld (ArrayData a) -> IO (Lifted (MutableArray# RealWorld a))
copyInternalIO v =
  readMutVarIO v $ \case
    Current arr -> cloneMutableArrayIO arr 0# (sizeofMutableArray# arr)
    Diff op v' -> do
      Lifted clone <- copyInternalIO v'
      appOpIO clone op
      pure (Lifted clone)

-- | Converting an array into a list. O(n)
toList :: Array a -> [a]
toList (DA v) = unsafeDupablePerformIO $ do
  Lifted arr <- copyInternalIO v
  let n = sizeofMutableArray# arr
      go i
        | isTrue# (i >=# n) = pure []
        | otherwise = do
            x <- readArrayIO arr i
            xs <- go (i +# 1#)
            pure (x : xs)
  go 0#

-- | Indexing an array. O(1)
{-# INLINE (!) #-}
(!) :: Array a -> Int -> a
DA v ! I# i = unsafeDupablePerformIO (helper v i) where
    helper v i = readMutVarIO v $ \case
      Current arr -> readArrayIO arr i
      Diff (Set j x) xs
        | isTrue# (i ==# j) -> pure x
        | otherwise -> helper xs i
      Diff (Swap j1 j2) xs
        | isTrue# (i ==# j1) -> helper xs j2
        | isTrue# (i ==# j2) -> helper xs j1
        | otherwise -> helper xs i

-- | Indexing an array. O(1)
-- Using the 'Solo' constructor, you can sequence indexing to happen before
-- future updates without having to evaluate the element itself.
{-# INLINE index #-}
index :: Int -> Array a -> Solo a
index (I# i) (DA v) = unsafeDupablePerformIO (helper v i) where
  helper v i = readMutVarIO v $ \case
      Current arr -> MkSolo <$> readArrayIO arr i
      Diff (Set j x) xs
        | isTrue# (i ==# j) -> pure (MkSolo x)
        | otherwise -> helper xs i
      Diff (Swap j1 j2) xs
        | isTrue# (i ==# j1) -> helper xs j2
        | isTrue# (i ==# j2) -> helper xs j1
        | otherwise -> helper xs i

{-# INLINE invertIO #-}
invertIO :: MutableArray# RealWorld a -> Op a -> IO (Op a)
invertIO _ (Swap i j) = pure (Swap i j)
invertIO arr (Set i _) = do
  y <- readArrayIO arr i
  pure (Set i y)

{-# INLINE appOpIO #-}
appOpIO :: MutableArray# RealWorld a -> Op a -> IO ()
appOpIO arr (Set i x) = writeArrayIO arr i x
appOpIO arr (Swap i j) = do
  x <- readArrayIO arr i
  y <- readArrayIO arr j
  writeArrayIO arr i y
  writeArrayIO arr j x

{-# INLINE appDiffOp #-}
appDiffOp :: Op a -> Array a -> Array a
appDiffOp op (DA v) = unsafeDupablePerformIO $
  readMutVarIO v $ \case
    xs@(Current arr) -> do
      op' <- invertIO arr op
      appOpIO arr op
      Lifted v' <- newMutVarIO xs
      writeMutVarIO v (Diff op' v')
      pure (DA v')
    Diff op' v' -> do
      Lifted arr <- copyInternalIO v'
      appOpIO arr op'
      appOpIO arr op
      Lifted v'' <- newMutVarIO (Current arr)
      pure (DA v'')

-- | Update the array element at a given position to a new value. O(1)
{-# INLINE set #-}
set :: Int -> a -> Array a -> Array a
set (I# i) x = appDiffOp (Set i x)

-- | Swap two elements in an array. O(1)
{-# INLINE swap #-}
swap :: Int -> Int -> Array a -> Array a
swap (I# i) (I# j) = appDiffOp (Swap i j)

-- | Copy an array. O(n)
-- This detaches any future updates from old versions of the array.
-- Use this when you know you will be updating a large part of an array.
copy :: Array a -> Array a
copy (DA v) = unsafeDupablePerformIO $ do
  Lifted arr <- copyInternalIO v
  Lifted var <- newMutVarIO (Current arr)
  pure (DA var)