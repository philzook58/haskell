
{-
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

import Data.Functor.Const
-- c is the product of a and b
-- it is a projection if 
-- We can only have c be a product of two possible types? An unfortunate restriction.
class ProdC a b c | c -> a b where
	proj1 :: c -> a
	proj2 :: c -> b
	profactor :: (d -> a) -> (d -> b) -> (d -> c)

instance ProdC a b (a,b) where
	proj1 = fst
	proj2 = snd
	profactor f g d = (f d, g d) 

data TwistProd a b = TwistProd {getpair :: (b,a)}

instance ProdC a b (TwistProd a b) where
	proj1 = snd . getpair
	proj2 = fst . getpair
	profactor f g d = TwistProd (g d, f d) 

-}

data CommuteSquare a b c d | 

class Pullback a b c d | d -> a b c where
	f :: a -> d
	g :: b -> d
	p :: c -> a
	q :: c -> b
	univ :: (v -> a) -> (v -> b) -> (v -> c) 
