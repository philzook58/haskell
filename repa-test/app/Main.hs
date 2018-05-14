
{-# LANGUAGE TypeOperators #-}
module Main where



import Lib
import Data.Array.Repa as Repa
import qualified Data.Vector.Unboxed as V



main :: IO ()
main = someFunc

inputs = [1..10] :: [Double]
x = fromListUnboxed (Z :. (10::Int)) inputs

y :: Array U DIM3 Int
y = fromListUnboxed (Z :. (3::Int) :. (3::Int) :. (3::Int)) [1..27]


-- needs typeoperators
type DIM6 = DIM5 :. Int

--Data.Vector package
z :: Array U DIM1 Int
z = fromUnboxed (Z :. (10::Int)) (V.enumFromN 0 10)

-- indexing
w = z ! (Z :. 0)

-- mapping creates D, delayed array. Fusion of loops good for performance. Interesting.
v = Repa.map (^2) x

-- can't be printed. Indexing it forces the delayed value.
v' = v Repa.! (Z :. 3)

-- This computes answer in parrallel
--  computeP v :: IO (Array U DIM1 Double)

-- shape of array (Z :. 10)
vshape = extent v

y2 :: Array U (Z:.Any Int) Double
y2 = fromListUnboxed (Z:. (1::Int)) [23.0]
--Nope
--y2 = fromListUnboxed (Z:. Any) [23.0]


y3 = y2 +^ x


 
