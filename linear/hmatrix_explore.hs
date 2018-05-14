import Numeric.LinearAlgebra
import Data.Complex


-- 

v = (3><3) [1,2,3,4,5,6,7,8,9] :: Matrix R

-- tr is trnaspose. That is BAD.
a = eigSH $ trustSym (v + (tr v))
b = eig v


c = v <> v

d = vector [3,0,-2]

e = matrix 3 [1..9] * ident 3

-- Creates f elemnt vector from list
g = 5 |> [1..] :: Vector R


-- horizontal concaenttation
f = ident 3 ||| konst 7 (3,4) :: Matrix R

-- === vertical concanetnation

-- <.> dot produc
-- <> matrix product


-- resphape flatten 

h = linspace 5 (8,2 :+ 1) :: Vector (Complex Double)

-- Slicing using DSL

-- ident diag takeDiag

-- fromBlocks
--diagBlocks


j = scalar 3
