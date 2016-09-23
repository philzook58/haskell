import Control.Applicative
--inspired by that really cool infnite list as derviatives concept

-- Okay. So what if I defined integration using lazy lists

--data Tree a = Leaf a | Branch (Tree a) (Tree a)

-- should not just have arrays. More general than that
data MultiScale a b = Nil | Scale a b (MultiScale a b) deriving (Show)


--interpolate :: 
-- interpolate (Scale coarsescale b (Scale finescale d e)) = something d a c (interpolate (Scale c d e))   

--scaledown
--scaleup
-- need a away to 

-- maybe create a to interpolate that pushes up a scale
-- and a typeclass decimate that
{-
multdx Scale dx b c = Scale (fmap (\a -> dx * a) b) (multdx c)
integrate f interval = (foldMap (+) (fmap f interval))

-- these two are very similar. Something is funky here.

instance Foldable (MultiScale a) where
	foldMap f Nil = Nil
	foldMap f (Scale a b c) = Scale a (foldMap f b) (foldMap f c) 

instance Functor (MultiScale a) where
	fmap f (Scale x y z) = Scale x (fmap f y) (fmap f z) 
	fmap f Nil = Nil 
-- fmap (+1.0) (generateScaler 1.0)

--generateScaler :: Real a =>  ->  
generateScaler a
	| a > 0.1 = Scale a [1, 2, 3] (generateScaler (a/2.0))
	| otherwise = Nil
--getScale (Scale a b _) = a
--getList (Scale a b _) = b
--DOABLE TASKS
 -- generate infintie array of spacing a

linspace :: Num a => a -> a -> [a]
linspace spacing current = (current):(linspace spacing (current+spacing))
--take 5 (linspace 1 0)

linspace' spacing current = iterate (+spacing) current

linspace'' = [0 ..]

--scaleMap

-}




-- or really I define the reals via lazy lists or trees

-- the reals are closures of fractions

-- if i made a strcture that as you keep pulling (maybe a tree? Each node is a box with a midpoint and subnodes are the smaller subdivisions)
-- or a list of lists. each list is a finer subdivision of [0,1] or even [0,\infty]

-- a scaling list. Also intriguing for renormalization applications?

-- renormazlie by upshifting list and then somehow rescaling? sampling from higher subdiviosn to lower subdivision?

-- Then that is sort of the reals

--Good for romberg integration.

--Then the integral is a function that converts the scale tree into a value at each scale. A lazy list of summation values

--you could define derivatives this way too. As finite differences at changing scales. So clearly this is not the same as the other 
-- exact method

-- monte carlo is another one. COuld take infnite sequence of pseudo random numebers

-- could also do lebesque integration? Could that be useful? seems like we avoid dangerous places like 1/x^2 accidentally evaluating
-- 0 is bad.

-- a function on the real takes this structure to the values at those positions	
-- or really mybe functions ARE the data structure. and you can lift usual functions to these functions by application
-- a map type operation

-- lift f = \tree -> map f tree

-- smort

-- romberg n integral = do extrapolation on first n scale levels of integral

-- generally maybe a reduce or fold type operation could occur at scales to reduce to a scale list. 


-- could take same idea for quantum field hilbert space. a lazy list of differently cutoff hilbert spaces.
-- or for multiscale PDE analysis.


-- might want to make scale space a doubly linked list, since we may want to go up in space too
-- also [-\infty,\infty] integration limits becomes possible

-- similar to idea that real numbers are defined as 0.289538985083... this is a lazy list that gives you the next digit
-- we're not really enumerating the reals. we're enumeration something like all the fractions.

--  to do numerics that ignore analytic results is insane. Like kalman filter


-------------------------

-- Another thought. We can use inifinite differentiability to perform an inifnite sequence of a steepest descent calucaiton


zeros = repeat 0
thunkzeros = repeat (const 0)
constD n = const n : thunkzeros 
x = id : constD 1
--sumD :: (Num b) => [a -> b] -> [a -> b] -> [a -> b]
sumD (a:as) (b:bs) = ((+) <$> a <*> b) : sumD as bs
--multD :: [a -> b] -> [a -> b] -> [a -> b]
multD f@(a:as) g@(b:bs) = ((*) <$> a <*> b) : sumD (multD f bs) (multD g as) -- The product rule
compD f@(a:as) g@(b:bs) = (a . b) : multD bs (compD as g) -- chain rule
powx 0 = constD 1
--powx n = multD x (powx (n-1))
powx n = (** n) : (multD (constD n) (powx (n-1))) 
d = tail
sinD = cycle (sin : cos : ((*(-1)) <$> sin) : ((*(-1)) <$> cos) : [])
cosD = tail sinD
expD = repeat exp
lnD = log : powx (-1)


-- solve minimization problem
minimize f@(_:a:a':as) x0 = x0 : minimize f x where
	                             val = a x0
	                             x = x0 - val/(a' x0)


-- it is possible that I want minimize to return some sequence of functions rather than x values	                             



-- evaluating D functions at point determines taylor series at point (missing n! values)

evalD (a:as) x = a x : evalD as x

fact 0 = 1
fact n = n * fact (n-1)
--expterm :: (Int a) => a 
--expterm n x =

--expseries = map (\n x -> x **n / (fact n)) [0..]

--taylorseries f x =  (*) <$> (evalD f x) <*> expseries 









