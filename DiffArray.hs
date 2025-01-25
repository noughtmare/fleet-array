{-# LANGUAGE MagicHash, UnboxedTuples, UnliftedDatatypes #-}
{-# OPTIONS_GHC -Wno-name-shadowing -ddump-simpl -ddump-to-file -dsuppress-all -dno-suppress-type-signatures -dno-typeable-binds #-}

module DiffArray (DiffArray, fromList, toList, (!), index, set, copy, swap) where

import GHC.Exts hiding (fromList, toList)
import Data.Tuple (Solo (MkSolo))

import Data.Kind (Type)

data Op a = Set Int# a | Swap Int# Int#

data DiffArray a = DA (MutVar# RealWorld (DiffArrayData a))
type DiffArrayData :: Type -> UnliftedType
data DiffArrayData a
  = Current {-# UNPACK #-} !(MutableArray# RealWorld a)
  | Diff {-# UNPACK #-} !(Op a) (MutVar# RealWorld (DiffArrayData a))

instance Show a => Show (DiffArray a) where
  show xs = "fromList " ++ show (toList xs)

fromList :: [a] -> DiffArray a
fromList xs = DA (runRW# $ \s ->
  case newArray# (case length xs of (I# n) -> n) undefined s of
  { (# s , arr #) ->
  case newMutVar# (Current arr) (go arr 0# xs s) of
  { (# _ , x #) -> x
  }})
  where
    go _ _ [] s = s
    go arr i (x:xs) s = go arr (i +# 1#) xs (writeArray# arr i x s)

toList :: DiffArray a -> [a]
toList (DA v) = runRW# $ \s ->
  case copyInternal v s of
  { (# s, arr #) ->
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

{-# INLINE (!) #-}
(!) :: DiffArray a -> Int -> a
DA v ! I# i = helper v i where
  helper v i = runRW# $ \s ->
    case readMutVar# v s of
      (# s , Current arr #) ->
        case readArray# arr i s of
          (# _ , x #) -> x
      (# _ , Diff (Set j x) xs #)
        | isTrue# (i ==# j) -> x
        | otherwise -> helper xs i
      (# _ , Diff (Swap j1 j2) xs #)
        | isTrue# (i ==# j1) -> helper xs j2
        | isTrue# (i ==# j2) -> helper xs j1
        | otherwise -> helper xs i

{-# INLINE index #-}
index :: Int -> DiffArray a -> Solo a
index (I# i) (DA v) = helper v i where
  helper v i = runRW# $ \s ->
    case readMutVar# v s of
      (# s , Current arr #) ->
        case readArray# arr i s of
          (# _ , x #) -> MkSolo x
      (# _ , Diff (Set j x) xs #)
        | isTrue# (i ==# j) -> MkSolo x
        | otherwise -> helper xs i
      (# _ , Diff (Swap j1 j2) xs #)
        | isTrue# (i ==# j1) -> helper xs j2
        | isTrue# (i ==# j2) -> helper xs j1
        | otherwise -> helper xs i

{-# INLINE set #-}
set :: Int -> a -> DiffArray a -> DiffArray a
set (I# i) x (DA v) = runRW# $ \s ->
  case readMutVar# v s of
    (# s , xs@(Current arr) #) ->
      case readArray# arr i s of
      { (# s , y #) ->
      case writeArray# arr i x s of
      { s ->
      case newMutVar# xs s of
      { (# s , v' #) ->
      case writeMutVar# v (Diff (Set i y) v') s of
      { !_ -> DA v'
      }}}}
    -- making a change to an old version of the array
    -- we copy to anticipate more usage
    (# s, Diff op v' #) ->
      case copyInternal v' s of
      { (# s , arr #) ->
      case appOp arr op s of
      { s ->
      case writeArray# arr i x s of
      { s ->
      case newMutVar# (Current arr) s of
      { (# _ , v'' #) -> DA v''
      }}}}

appOp :: MutableArray# RealWorld a -> Op a -> State# RealWorld -> State# RealWorld
appOp arr (Set i x) s = writeArray# arr i x s
appOp arr (Swap i j) s =
  case readArray# arr i s of { (# s, x #) ->
  case readArray# arr j s of { (# s, y #) ->
  case writeArray# arr i y s of { s ->
  writeArray# arr j x s
  }}}

{-# INLINE swap #-}
swap :: Int -> Int -> DiffArray a -> DiffArray a
swap (I# i) (I# j) (DA v) = runRW# $ \s ->
  case readMutVar# v s of
    (# s , xs@(Current arr) #) ->
      case appOp arr (Swap i j) s of { s ->
      case newMutVar# xs s of
      { (# s , v' #) ->
      case writeMutVar# v (Diff (Swap i j) v') s of
      { !_ -> DA v'
      }}}
    -- making a change to an old version of the array
    -- we copy to anticipate more usage
    (# s, Diff op v' #) ->
      case copyInternal v' s of
      { (# s , arr #) ->
      case appOp arr op s of { s ->
      case appOp arr (Swap i j) s of { s ->
      case newMutVar# (Current arr) s of
      { (# _ , v'' #) -> DA v''
      }}}}

copyInternal :: MutVar# RealWorld (DiffArrayData a) -> State# RealWorld -> (# State# RealWorld, MutableArray# RealWorld a #)
copyInternal v s =
  case readMutVar# v s of
    (# s , Current arr #) ->
      cloneMutableArray# arr 0# (sizeofMutableArray# arr) s
    (# s , Diff op v #) ->
      case copyInternal v s of
      { (# s , arr #) ->
      case appOp arr op s of
      { s -> (# s , arr #)
      }}

copy :: DiffArray a -> DiffArray a
copy (DA v) = runRW# $ \s ->
  case copyInternal v s of
  { (# s , arr #) ->
  case newMutVar# (Current arr) s of
  { (# _, v #) -> DA v
  }}