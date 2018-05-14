-- Guassian functional integral
-- takes 
-- Summation using fmm
-- infinite series in J
-- 

-- I think that dieriatives hsould be one forms
-- every higher derivative gets an extra
data Diff a b = Cons b (Diff a (a -> b))


-- power series are quire similar to differentiable functions
-- except differentiable function have (x -> c) at the end rather than just 
-- c

-- since we expect all parameters to be the same anyway
-- we could compress this into just a striaghtup list.
sameargs :: (a -> a -> c) -> (a -> c)
sameargs f = \a -> f a a

{-
arityone :: Diff a b -> [a -> b]
arityone (Cons x y) = x : (fmap sameargs (arityone y))
-}

-- a higher arity 0 function
-- curried
higherzero = const (const (const 0))

-- an sampling integration procedure
-- integ fmaps in ?
-- m :: f Float
-- fmaps (* dx) m
-- also need to add them together
-- composable integration routines
-- f Float = (anything -> Float) usually.
-- also need applicative instance for adding together results? 
-- or foldable?
-- integ :: (Float -> Float -> f Float) -> f Float
-- use endpoints a and b
-- integ a b = 
integ :: (Applicative f) => Float -> Float -> Float -> (Float -> f Float) -> f Float
integ a b dx f = fold (\x y -> (+) <$> x <*> y) (fmap f [a, (a + dx) .. b])

type Diff' x dx y = [(x -> dx -> y)] 

type Functional = Diff' (Float -> Float) (Float -> Float) Float

-- polynomial in J functionals are power series. I can probably somehow convert all power series to
-- derivatives
-- two nested lists. one for power of J one for power of dJ. I Don't like it.

JGJ :: Functional
JGJ =  \j dj ->  integ (\x y -> (g x y) * (j x) * (j y) : 
	\j dj -> 2 * integ (\x y -> (g x y) * (j x) * (dj y) :
\j dj ->  integ (\x y -> (g x y) * (dj x) * (dj y) :
myzero


expJ :: Functional -> Functional
expJ = 1 : (\j -> 1/ n * expJ)   


-- also would be nice or necessary to form the expoential of d/dJ

myzero = repeat (const (const 0))

-- There is nothing enforcing me to use J in the polynomial. Linear Types might

quadG = \j1 j2 -> integ (integ (\x y -> (g x y) * (j1 x) * (j2 y) ))

-- The power series at j = 0 is the power series I want probably.



