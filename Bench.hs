{-# OPTIONS_GHC -ddump-simpl -ddump-to-file -dsuppress-all -dno-typeable-binds -dno-suppress-type-signatures #-}
import qualified Array
import Test.Tasty.Bench
import qualified DiffArray
import DiffArray (DiffArray)

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
  defaultMain
    [ bench "diffarray" $ whnf fooDA (DiffArray.fromList list)
    , bench "array" $ whnf fooA (Array.fromList list)
    ]