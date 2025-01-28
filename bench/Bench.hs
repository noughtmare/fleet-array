{-# OPTIONS_GHC -ddump-simpl -ddump-to-file -dsuppress-all -dno-typeable-binds -dno-suppress-type-signatures #-}
import qualified Array
import Test.Tasty.Bench
import qualified Fleet.Array as DiffArray
import Fleet.Array (DiffArray)
import Quicksort (quicksort)
import qualified QuicksortA

class Indexable a where
  (!) :: a -> Int -> Int

instance Indexable (Array.Array Int) where
  (!) = (Array.!)
instance Indexable (DiffArray Int) where
  (!) = (DiffArray.!)

loop :: Indexable a => a -> Int -> Int -> Int
loop _ 1 s = s
loop arr i s = loop arr (arr ! i) (s + 1)

{-# NOINLINE fooA #-}
fooA :: Array.Array Int -> Int
fooA arr = loop arr (arr ! 1) 0

{-# NOINLINE fooDA #-}
fooDA :: DiffArray Int -> Int
fooDA arr = loop arr (arr ! 1) 0

list :: [Int]
list = 0:take 9972 (iterate (\x -> (x * 9962) `rem` 9973) 9962)

main :: IO ()
main = do
  let
    arrA = Array.fromList list
    arrDA = DiffArray.fromList list
  print $ fooA arrA
  print $ fooDA arrDA
  let
    !arr10 = DiffArray.fromList list
    !arr9 = DiffArray.set 0 0 arr10
    !arr8 = DiffArray.set 0 0 arr9
    !arr7 = DiffArray.set 0 0 arr8
    !arr6 = DiffArray.set 0 0 arr7
    !arr5 = DiffArray.set 0 0 arr6
    !arr4 = DiffArray.set 0 0 arr5
    !arr3 = DiffArray.set 0 0 arr4
    !arr2 = DiffArray.set 0 0 arr3
    !arr1 = DiffArray.set 0 0 arr2
    !arr0 = DiffArray.set 0 0 arr1
  !marr <- QuicksortA.fromList list
  defaultMain
    [ bench "array" $ whnf fooA (Array.fromList list)
    , bench "diffarray" $ whnf fooDA arr0
    , bench "diffarray 1" $ whnf fooDA arr1
    , bench "diffarray 2" $ whnf fooDA arr2
    , bench "diffarray 5" $ whnf fooDA arr5
    , bench "diffarray 7" $ whnf fooDA arr7
    , bench "diffarray 10" $ whnf fooDA arr10
    , bench "quicksort array" $ whnfIO (QuicksortA.clone marr >>= \marr' -> QuicksortA.quicksort marr' 0 9973)
    , bench "quicksort diffarray" $ whnf (Quicksort.quicksort 0 9973) (DiffArray.copy arr0)
    , bench "quicksort diffarray copy" $ whnf (Quicksort.quicksort 0 9973 . DiffArray.copy) arr0
    ]