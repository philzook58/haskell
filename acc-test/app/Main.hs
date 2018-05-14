module Main where

import Data.Array.Accelerate as Acc

import Lib

main :: IO ()
main = someFunc


-- use pops haskell arrays up to accelerate guys

{-
dotp :: Num a => Vector a -> Vector a -> Acc (Scalar a)
dotp xs ys =
  let
      xs' = use xs
      ys' = use ys
  in
  Acc.fold (+) 0 ( Acc.zipWith (*) xs' ys' )

-}

indexHead $ constant (Z:.2)

fromShapetoList (a:.b) x = fromShapetoList a (b:x)
fromShapetoList _ x = x


