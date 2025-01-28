{-# LANGUAGE MagicHash, UnboxedTuples, UnliftedDatatypes #-}
{-# OPTIONS_GHC -Wno-name-shadowing -ddump-simpl -ddump-to-file -dsuppress-all -dno-suppress-type-signatures -dno-typeable-binds #-}

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

import GHC.Exts hiding (fromList, toList)
import Data.Tuple (Solo (MkSolo))

import Data.Kind (Type)

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

-- | Convert a list into an array. O(n)
fromList :: [a] -> Array a
fromList xs = DA (runRW# $ \s ->
  case newArray# (case length xs of (I# n) -> n) undefined s of { (# s , arr #) ->
  case newMutVar# (Current arr) (go arr 0# xs s) of { (# _ , x #) -> x
  }}) where
    go _ _ [] s = s
    go arr i (x:xs) s = go arr (i +# 1#) xs (writeArray# arr i x s)

-- | Converting an array into a list. O(n)
toList :: Array a -> [a]
toList (DA v) = runRW# $ \s ->
  case copyInternal v s of { (# s, arr #) ->
  let
    n = sizeofMutableArray# arr
    go i s
      | isTrue# (i >=# n) = []
      | otherwise =
        case readArray# arr i s of
        { (# s, x #) -> x : go (i +# 1#) s
        }
  in go 0# s
  }

-- | Indexing an array. O(1)
{-# INLINE (!) #-}
(!) :: Array a -> Int -> a
DA v ! I# i = helper v i where
  helper v i = runRW# $ \s ->
    case readMutVar# v s of
      (# s , Current arr #) ->
        case readArray# arr i s of (# _ , x #) -> x
      (# _ , Diff (Set j x) xs #)
        | isTrue# (i ==# j) -> x
        | otherwise -> helper xs i
      (# _ , Diff (Swap j1 j2) xs #)
        | isTrue# (i ==# j1) -> helper xs j2
        | isTrue# (i ==# j2) -> helper xs j1
        | otherwise -> helper xs i

-- | Indexing an array. O(1)
-- Using the 'Solo' constructor, you can sequence indexing to happen before
-- future updates without having to evaluate the element itself.
{-# INLINE index #-}
index :: Int -> Array a -> Solo a
index (I# i) (DA v) = helper v i where
  helper v i = runRW# $ \s ->
    case readMutVar# v s of
      (# s , Current arr #) ->
        case readArray# arr i s of (# _ , x #) -> MkSolo x
      (# _ , Diff (Set j x) xs #)
        | isTrue# (i ==# j) -> MkSolo x
        | otherwise -> helper xs i
      (# _ , Diff (Swap j1 j2) xs #)
        | isTrue# (i ==# j1) -> helper xs j2
        | isTrue# (i ==# j2) -> helper xs j1
        | otherwise -> helper xs i

{-# INLINE appOp #-}
appOp :: MutableArray# RealWorld a -> Op a -> State# RealWorld -> State# RealWorld
appOp arr (Set i x) s = writeArray# arr i x s
appOp arr (Swap i j) s =
  case readArray# arr i s of { (# s, x #) ->
  case readArray# arr j s of { (# s, y #) ->
  case writeArray# arr i y s of { s ->
  writeArray# arr j x s
  }}}

{-# INLINE invert #-}
invert :: MutableArray# RealWorld a -> Op a -> State# RealWorld -> (# State# RealWorld, Op a #)
invert _ (Swap i j) s = (# s , Swap i j #)
invert arr (Set i _) s =
  case readArray# arr i s of { (# s, y #) ->
  (# s, Set i y #) }

{-# INLINE appDiffOp #-}
appDiffOp :: Op a -> Array a -> Array a
appDiffOp op (DA v) = runRW# $ \s ->
  case noDuplicate# s of { s ->
  case readMutVar# v s of
    (# s , xs@(Current arr) #) ->
      case invert arr op s of { (# s, op' #) ->
      case appOp arr op s of { s ->
      case newMutVar# xs s of { (# s , v' #) ->
      case writeMutVar# v (Diff op' v') s of { !_ ->
        DA v'
      }}}}
    -- making a change to an old version of the array
    -- we copy to anticipate more usage
    (# s, Diff op' v' #) ->
      case copyInternal v' s of { (# s , arr #) ->
      case appOp arr op' s of { s ->
      case appOp arr op s of { s ->
      case newMutVar# (Current arr) s of { (# _ , v'' #) ->
        DA v''
      }}}}}

-- {-# INLINE appUndoOp #-}
-- appUndoOp :: Op a -> Array a -> Array a
-- appUndoOp op (DA v) = runRW# $ \s ->
--   case readMutVar# v s of
--     (# s , xs@(Current arr) #) ->
--       case invert arr op s of { (# s, op' #) ->
--       case appOp arr op s of { s ->
--       case newMutVar# xs s of { (# s , v' #) ->
--       case writeMutVar# v (Diff op' v') s of { !_ ->
--         DA v'
--       }}}}
--     -- making a change to an old version of the array
--     -- we roll back all changes
--     (# s, Diff op' v' #) ->
--       case appUndoOp ov' s of { (# s , arr #) ->
--       case appOp arr op s of { s ->
--       case appOp arr op s of { s ->
--       case newMutVar# (Current arr) s of { (# _ , v'' #) ->
--         DA v''
--       }}}}

-- | Update the array element at a given position to a new value. O(1)
{-# INLINE set #-}
set :: Int -> a -> Array a -> Array a
set (I# i) x = appDiffOp (Set i x)

-- | Swap two elements in an array. O(1)
{-# INLINE swap #-}
swap :: Int -> Int -> Array a -> Array a
swap (I# i) (I# j) = appDiffOp (Swap i j)

copyInternal :: MutVar# RealWorld (ArrayData a) -> State# RealWorld -> (# State# RealWorld, MutableArray# RealWorld a #)
copyInternal v s =
  case readMutVar# v s of
    (# s , Current arr #) ->
      cloneMutableArray# arr 0# (sizeofMutableArray# arr) s
    (# s , Diff op v #) ->
      case copyInternal v s of { (# s , arr #) ->
      case appOp arr op s of { s -> (# s , arr #)
      }}

-- | Copy an array. O(n)
-- This detaches any future updates from old versions of the array.
-- Use this when you know you will be updating a large part of an array.
copy :: Array a -> Array a
copy (DA v) = runRW# $ \s ->
  case copyInternal v s of { (# s , arr #) ->
  case newMutVar# (Current arr) s of { (# _, v #) -> DA v
  }}