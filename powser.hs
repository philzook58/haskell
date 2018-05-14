--Written by Doug McIlroy 

default (Integer,Rational,Double)

infixr 9 #
series f = f : repeat 0
instance (Num a, Eq a) => Num [a] where
   fromInteger c = series(fromInteger c)
   negate (f:ft) = -f : -ft
   (f:ft) + (g:gt) = f+g : ft+gt
   (f:ft) * gs@(g:gt) = f*g : ft*gs + series(f)*gt
   abs (f:ft) = (abs f) : (abs ft)
   signum (f:ft) = (signum f) : (signum ft)
instance (Fractional a, Eq a) => Fractional [a] where
   (f:ft) / (g:gt) = qs where qs = f/g : series(1/g)*(ft-qs*gt)
   fromRational c = series(fromRational c)
-- avoiding evaluating infinite series just to get first number.
-- if I extend will a finite evaluation function could we be better?
-- Then we get real composition. Butttttt. 

(f:ft) # gs@(0:gt) = f : gt*(ft#gs)
revert (0:ft) = rs where rs = 0 : 1/(ft#rs)
int fs = 0 : zipWith (/) fs [1..]
diff (_:ft) = zipWith (*) ft [1..]
tans = revert(int(1/(1:0:1)))
sins = int coss
coss = 1 - int sins
exps = 1 + int exps

-- fmap fromRational $ take 5 exps

-- laurent series? could be useful for example using z+ 1/z trick for fourier.

-- Legendre transform -- interesting.
-- legendre x =  (int . series 1) - x # (revert . diff x)
-- test legendre of axx => pp/a

