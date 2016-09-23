
-- Define real number as inequality on the fractions
-- I had totally forgotten about dedekind cuts
-- least upper bound is important here
-- So maybe not just the inequality, but the fixified version. Bottom may represent inifnity or negative inifnty


-- what about cauchy sequences? They seem pretty good. Infinite Lists + some kind of algorithm or guarantee system
-- for any \epsilon -> N s.t. all m,n > N the sequence stays closer than epsilion


-- Raitonal data type?

import Data.Ratio

data Real = Real (Rational -> Bool)
sqrt :: Rational -> Real
sqrt x = (\s -> s <= x * x) 

-- Hmmm. How to compound or lift
sqrt2 :: Real -> Real

half = 1 % 2 -- Construciton of rational

-- not a functor.
{-
instance Functor Real where
	fmap :: (Rational -> Rational) -> (Real -> Real)
	fmap f = 
		-}
ratLift :: (Rational -> Rational) -> Real -> Real
ratLift f r = r . f -- ? 
