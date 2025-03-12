{-# LANGUAGE MagicHash, UnboxedTuples, UnliftedDatatypes #-}
{-# OPTIONS_GHC -ddump-simpl -ddump-to-file -dsuppress-all -dno-suppress-type-signatures -dno-typeable-binds #-}
-- O2 is necessary to get the right call pattern specializations and remove all the lifted abstractions
{-# OPTIONS_GHC -O2 #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}

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
module Fleet.Array
  ( Array
  , fromList
  , replicate
  , toList
  , (!)
  , index
  , tag
  , set
  , copy
  , swap
  , pseq
  ) where

import Prelude hiding (replicate)

import GHC.Exts hiding (fromList, toList, Lifted)

import Data.Kind (Type)
import GHC.Conc (pseq)
import GHC.Base (IO (IO))
-- import GHC.IO.Unsafe (unsafeDupablePerformIO)

import Fleet.Array.MutVar
import Fleet.Array.Lift
import Fleet.Array.MutArray

unsafeDupablePerformIO :: IO a -> a
unsafeDupablePerformIO (IO f) = runRW# (\s -> case f s of (# _ , x #) -> x)

data Op a = Set {-# UNPACK #-} !Int a | Swap {-# UNPACK #-} !Int {-# UNPACK #-} !Int

-- | Fleet arrays.
data Array a = A {-# UNPACK #-} !(ArrayVar a)
type ArrayData# :: Type -> UnliftedType
data ArrayData# a
  = Current# {-# UNPACK #-} !(MutArray a)
  | Diff# {-# UNPACK #-} !(Op a) {-# UNPACK #-} !(ArrayVar a)

type ArrayVar a = MutVar (ArrayData# a)

type ArrayData a = Lift (ArrayData# a)
pattern Current :: MutArray a -> ArrayData a
pattern Current x = Lift (Current# x)
pattern Diff :: Op a -> ArrayVar a -> ArrayData a
pattern Diff op v = Lift (Diff# op v)
{-# COMPLETE Current, Diff #-}

instance Show a => Show (Array a) where
  show xs = "fromList " ++ show (toList xs)

-- | Convert a list into an array. O(n)
fromList :: [a] -> Array a
fromList xs = unsafeDupablePerformIO $ do
  arr0 <- newMutArray (length xs) undefined
  let go _ _ [] = pure ()
      go arr i (x:xs') = writeMutArray arr i x *> go arr (i + 1) xs'
  go arr0 0 xs
  v <- newMutVar (Current arr0)
  pure (A v)

replicate :: Int -> a -> Array a
replicate n x = unsafeDupablePerformIO $ do
  arr <- newMutArray n x
  v <- newMutVar (Current arr)
  pure (A v)

copyInternal :: ArrayVar a -> IO (MutArray a)
copyInternal v = do
  av <- readMutVar v
  case av of
    Current arr -> cloneMutArray arr 0 (sizeofMutArray arr)
    -- _ -> error "Accessing old version"
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
  --
  -- __WARNING:__ If you were to write your own 'swap' function. You might be
  -- tempted to write it like this:
  --
  -- > swap :: Int -> Int -> Array a -> Array a
  -- > swap !i !j !xs = set i (xs ! j) (set j (xs ! i) xs)
  --
  -- Unfortunately, this leaves the order between the reads and writes undefined.
  -- And in practice, GHC picks the wrong order. To enforce that reads happen
  -- before writes, you can use 'pseq' like this:
  --
  -- > swap !i !j !xs =
  -- >   let
  -- >     x = xs ! i
  -- >     y = xs ! j
  -- >   in x `pseq` y `pseq` set i y (set j x xs)
  --
  -- If you want to avoid forcing the elements in the array, then you can use
  -- 'index' like this:
  --
  -- > swap !i !j !xs =
  -- >   let
  -- >     x@(MkSolo x') = index i xs
  -- >     y@(MkSolo y') = index j xs
  -- >   in x `pseq` y `pseq` set i y' (set j x' xs)
  --
  -- In the future, we hope to write a GHC plugin that can automatically detect
  -- when pseq is necessary in common cases.
{-# INLINE (!) #-}
(!) :: Array a -> Int -> a
A v0 ! i0 = unsafeDupablePerformIO (go v0 i0) where
  go v i = do
    dat <- readMutVar v
    case dat of
      Current arr -> readMutArray arr i
      -- _ -> error "Accessing old version"
      Diff (Set j x) v'
        | i == j -> pure x
        | otherwise -> go v' i
      Diff (Swap j1 j2) v'
        | i == j1 -> go v' j2
        | i == j2 -> go v' j1
        | otherwise -> go v' i

data Token = Token (State# RealWorld)

returnToken :: a -> IO (a, Token)
returnToken x = IO (\s -> (# s , (x, Token s) #))

-- | Indexing an array. O(1)
--
-- The tuple and 'Token' serve two purposes:
--
-- - You can now separately force the evaluation of the tuple and the actual
--   array element
-- - You can use the 'Token' to with the 'tag' function on an array to force
--   the indexing to happen before the array can be written to.
{-# INLINE index #-}
index :: Int -> Array a -> (a, Token)
index i0 (A v0) = unsafeDupablePerformIO (go v0 i0) where
  go v i = do
    dat <- readMutVar v
    case dat of
      Current arr -> readMutArray arr i >>= returnToken
      -- _ -> error "Accessing old version"
      Diff (Set j x) xs
        | i == j -> returnToken x
        | otherwise -> go xs i
      Diff (Swap j1 j2) xs
        | i == j1 -> go xs j2
        | i == j2 -> go xs j1
        | otherwise -> go xs i

-- | This is a no-op, but can be used to enforce an ordering between indexing
-- and other array operations, to avoid the overhead of indexing from older
-- versions of the array.
--
-- For example, swapping two elements in an array by using 'index'
-- and 'set' can be done like this:
--
-- @
-- swap :: Int -> Int -> Array a -> Array a
-- swap i j xs =
--   let (x, t1) = index i xs
--       (y, t2) = index j xs
--   in set i y (set j x (tag t1 (tag t2 xs)))
-- @
--
-- This ensures the indexing happens before the setting.
{-# NOINLINE tag #-}
tag :: Token -> Array a -> Array a
tag (Token _) xs = xs

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

{-# INLINE reversePointers #-}
reversePointers :: ArrayVar a -> IO (MutArray a)
reversePointers v = do
  dat <- readMutVar v
  case dat of
    Current arr -> pure arr
    Diff op v' -> reversePointersDiff v op v'

-- this needs to be a separate function, because we want the good weather path
-- (where dat = Current ...) to inline and optimize. The recursion in this
-- function, which prevents inlining, thus needs to be extracted from
-- reversePointers.
reversePointersDiff :: ArrayVar a -> Op a -> ArrayVar a -> IO (MutArray a)
reversePointersDiff v op v' = do
  dat <- readMutVar v'
  arr <- case dat of
    Current arr -> pure arr
    Diff op' v'' -> reversePointersDiff v' op' v''
  op' <- invert arr op
  appOp arr op
  writeMutVar v' (Diff op' v)
  pure arr

{-# INLINE appDiffOp #-}
appDiffOp :: Op a -> Array a -> Array a
appDiffOp op (A v) = unsafeDupablePerformIO $ do
  arr <- reversePointers v
  op' <- invert arr op
  appOp arr op
  v' <- newMutVar (Current arr)
  writeMutVar v (Diff op' v')
  pure (A v')

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
  var <- newMutVar (Current arr)
  pure (A var)

unsafeAppDiffOp :: Op a -> Array a -> Array a
unsafeAppDiffOp op (A v) = unsafeDupablePerformIO $ do
  readMutVar v >>= \case
    Current arr -> do
      appOp arr op
      pure (A v) -- reusing 'v' like this avoids allocating, but it will cause
                 -- old versions to silently give wrong results.
                 -- I don't see an easy option that avoids silent breakage
    _ -> error "Unsafe operation encountered old array version."