{-# LANGUAGE Arrows #-}
import Control.Arrow
import Control.Category
import Prelude hiding (id,(.))


newtype Simple a b = Simple (a -> b)
-- Simple is just a wrapper for ordinary function with splitting via pairs.

instance Arrow Simple where
	arr f = Simple f -- wrap up function
	first (Simple f) = Simple (\(a,b) -> (f a, b)) 
	second (Simple f) = Simple (\(a,b) -> (a, f b)) 


-- can't directly define >>>
-- it's implied from composiiton defintion though
instance Category Simple where
	(Simple f) . (Simple g) = Simple (f . g)
	id = arr id



applySimple (Simple f) x = f x

f = arr (\x -> 2 + x)
g = Simple (\x -> 3 * x)

h = (f >>> g)
--applySimple h 3


h' = proc x -> do
	fx <- f -< x
	gfx <- g -< fx
	returnA -< gfx

