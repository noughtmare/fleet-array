{-# OPTIONS_GHC -ddump-simpl -ddump-to-file -dsuppress-all -dno-typeable-binds -dno-suppress-type-signatures #-}
import qualified Array
import Test.Tasty.Bench
import qualified Fleet.Array as Fleet
import qualified Example.Fleet.Quicksort as Quicksort
import qualified Example.MutArr.Quicksort as QuicksortMA
import qualified QuicksortIM
import qualified Data.List as List
import qualified Example.Fleet.Eratosthenes as Fleet
import qualified Example.MutArr.Eratosthenes as MA

class Indexable a where
  (!) :: a -> Int -> Int

instance Indexable (Fleet.Array Int) where
  (!) = (Fleet.!)
instance Indexable (Array.Array Int) where
  (!) = (Array.!)

loop :: Indexable a => a -> Int -> Int -> Int
loop _ 1 s = s
loop arr i s = loop arr (arr ! i) (s + 1)

{-# NOINLINE fooA #-}
fooA :: Array.Array Int -> Int
fooA arr = loop arr (arr ! 1) 0

{-# NOINLINE fooDA #-}
fooDA :: Fleet.Array Int -> Int
fooDA arr = loop arr (arr ! 1) 0

list :: [Int]
list = 0:take 9972 (iterate (\x -> (x * 9962) `rem` 9973) 9962)

main :: IO ()
main = do
  let
    !arr10 = Fleet.fromList list
    !arr9 = Fleet.set 0 0 arr10
    !arr8 = Fleet.set 0 0 arr9
    !arr7 = Fleet.set 0 0 arr8
    !arr6 = Fleet.set 0 0 arr7
    !arr5 = Fleet.set 0 0 arr6
    !arr4 = Fleet.set 0 0 arr5
    !arr3 = Fleet.set 0 0 arr4
    !arr2 = Fleet.set 0 0 arr3
    !arr1 = Fleet.set 0 0 arr2
    !arr0 = Fleet.set 0 0 arr1
  !marr <- QuicksortMA.fromList list
  defaultMain
    [ bgroup "read"
      [ bench "array" $ whnf fooA (Array.fromList list)
      , bench "fleet" $ whnf fooDA arr0
      -- reading old versions:
      , bench "fleet 1" $ whnf fooDA arr1
      , bench "fleet 2" $ whnf fooDA arr2
      , bench "fleet 5" $ whnf fooDA arr5
      , bench "fleet 7" $ whnf fooDA arr7
      , bench "fleet 10" $ whnf fooDA arr10
      ]
    , bgroup "quicksort"
      [ bench "MutArr" $ whnfIO (QuicksortMA.clone marr >>= \marr' -> QuicksortMA.quicksort marr' 0 9973)
      , bench "Fleet" $ whnf (Quicksort.quicksort 0 9973) (Fleet.copy arr0)
      , bench "Fleet copy" $ whnf (Quicksort.quicksort 0 9973 . Fleet.copy) arr0
      , bench "IntMap" $ whnf (QuicksortIM.quicksort 0 9973) (QuicksortIM.fromList list)
      , bench "List.sort" $ nf (\xs -> List.sort xs) list
      ]
    , bgroup "sieve"
      [ bench "fleet" $ nf Fleet.sieve 100000
      , bench "MutArr" $ nfIO (MA.sieve 100000)
      ]
    ]