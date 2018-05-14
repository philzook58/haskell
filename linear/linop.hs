-- I think I'm at a watershed where I can attempt my library.

{-

Notes:

This approach hsould extend to symbolic vectors

the dual vectors of continuous functions contain integration schemes.

A somewhat nice way of doing this is to let the field they are vectors in be an infinite sequence
of greater and greater precision. Take the number of terms you want and series accelerate.

This also shows an example where you may want the dual to not be over the same field as the original vector.
Original vectors would be float but dual would inject into [Float].
type Dual a b f = a -> f b
in this case f is []. often would be identity functor. 
i.e.  

it would feel more natural for somehow the basis to be sequence like... but it doesn't yet obviously fit to me eye.

Th function bottleneck concerns me. Functions are relatively inncuous general things though in Haskell context

Generalizble to contrvaraint functors instead of vecs? But c tagging along is important

if you have the dual function, you can convert to any other vector type with same basis.

Profunctorial isomorphisms as basis change?


partial_x is unit vector

It's actually a linear operator

partial_x :: (X -> Phi) -> (X -> GradPhi)
partial_y
partial_z

hmm.

-}




--{-# LANGUAGE RankNTypes #-}


import Data.Profunctor
import Data.Complex

--newtype Vec b c = Vec (b -> c)
type Vec b c = b -> c


newtype WrappedVec b c = WrappedVec {unwrapvec :: Vec b c}

instance LinearVec WrappedVec where
	vec = unwrapvec




-- for example Dual (Vec b c) c 
-- type Dual b c = Vec (Vec b c) c
type Dual b c = (Vec b c) -> c
--newtype Dual b c = Dual (Vec b c -> c)

-- what I want is Dual (Vec b c) to somehow be the right thing.


-- This shows that the Vec object should be applicative in c.
(.+) :: Num c => Vec b c -> Vec b c -> Vec b c
(.+) v w = (+) <$>  v <*> w

(.*) :: Num c => c -> Vec b c -> Vec b c
(.*) s v = (* s) <$> v


--newtype LinOp a b c = LinOp (Vec a c -> Vec b c)

type LinOp a b c = Vec a c -> Vec b c

-- monadic variant
-- type Linop a b c = a -> Vec b c

class LinearVec v where
	vec :: (Num c) => v b c -> Vec b c
	unitvec :: (Num c, Eq b) => b -> v b c


{-

unitvec a b = if (b == a) then (fromInteger 1) else (fromInteger 0)
-}

	-- unitvec?
	-- maybe have an overwriteable implementation for .+ and .* that defaults to ..+

(..+) :: (Num c, LinearVec v, LinearVec u) => u b c -> v b c -> Vec b c
(..+) u v = (vec u) .+ (vec v)

(..*) :: (Num c, LinearVec v) => c -> v b c -> Vec b c
(..*) s v = s .* (vec v)  

class LinearOp f where
	linop :: f a b c -> LinOp a b c

(**) :: (LinearOp f, LinearOp g) => f b d c -> g a b c -> LinOp a d c
a ** b = (linop a) . (linop b)



class Dualizable v where
	dual :: (Num c) => v b c -> (v b c -> c)
	dual = inner

	inner :: (Num c) => v b c -> (v b c -> c)
	inner = dual


-- conversion between deifferent encodings in same basis
{-


unitvec :: b -> f b c
 




--wait. this doesn't work at all. 
convertvec :: (Dualizable f, Dualizable g) => f b c -> g b c
convertvec v = \a -> ((dual (unitvec a) v)  .* (unitvec a))


-}

-- I don't think this works. 
{-
class Dualable v where
	dual :: v b c -> (v b c -> d)
	dual = inner

	inner :: v b c -> (v b c -> d)
	inner = dual

	-}



-- data Block a b c d e = Block (LinOp a c e) (LinOp b c e) (LinOp a d e) (LinOp b d e)

-- But what I really want is to be able to make a block of any four objects that implement LinearOp
type Block a b c d e = LinOp (Either a b) (Either c d) e

blockify :: (Num e) => LinOp a c e -> LinOp b c e -> LinOp a d e -> LinOp b d e -> Block a b c d e
--blockify a b c d = either (\v1 -> directsum (a v1) (c v1) ) (\v2 -> directsum (b v2) (d v2))
-- Holy cripes. That is a weird one. Sort of built up backwards.
-- the result is the direct sum of the upper and lower parts. 
blockify a b c d v = directsum ((a u1) .+ (b u2)) ((c u1) .+ (d u2)) where 
																		 u1 = v . Left
 																		 u2 = v . Right

-- Hmm. Should there be a dual of blockify in some sense?
-- I wonder if it is AxI + IxB
type FactorOp a b c d e = LinOp (a, b) (c, d) e
kronify a b v = directproduct (a  (v . fst)) (b (v . snd))

-- do these also work on the dual?
directsum :: Vec a c -> Vec b c -> Vec (Either a b) c
directsum = either


-- It's strange that this is so relatively inelegant
-- while direct sum is much more elegant
-- I wonder if this has relation to the coproduct nature of Either.
-- maybe direct product is easier for DualVecs
directproduct :: (Num c) => Vec a c -> Vec b c -> Vec (a, b) c
directproduct u v = \(x, y) -> (u x) * (v y) 

{-

instance Linear LinOp where
	linop = id

	-}

{-
instance Linear Vec where
	linop v = LinOp (\_ -> v) 
-}




data Basis = X | Y | Z

x :: Vec Basis Integer
x X = 1
x _ = 0

dx :: Dual Basis a
dx v = (v X)


-- ************

-- This is an experiment with continuum vectors and encoding an integration scheme as part of dualizing.
-- It does not appear to fit in with the previous mechanisms without modification

type PositionBasis = Float
type Amp = Complex Float
--type PositionVec = (Floating a) => Vec a (Complex a)

e0 :: (Floating a) => Vec a (Complex a)
e0 x = (:+) (sin x) (fromInteger 0)

e1 :: (Floating a) => Vec a (Complex a)
e1 x = (:+) (sin (fromInteger 2) * x) (fromInteger 0)

en :: (Floating a) => Integer -> Vec a (Complex a)
en n x = (:+) (sin (fromInteger n) * x) (fromInteger 0)

-- The integration procedure requires the output and domain to have same type.
e1' :: (Floating a) => Vec a (a)
e1' x = sin ((fromInteger 2) * x)

newtype BoxVec b c = BoxVec {unwrapbox :: Vec b c}

newtype ContVec b c = ContVec {unwrapcontvec :: Vec b c}

-- why functional programming matters - John Hughes
-- I feel I should also be maintaining an accumultor though.
--integrate f a b = sum . fmap f [0 ] where mid = (b+a) / 2
integrate f a b = integ f a b (f a) (f b)
integ f a b fa fb = ((fa + fb) * (b-a)/2) : (zipWith (+) (integ f a m fa fm) (integ f m b fm fb)) where
	                                      m = (a+b)/2
	                                      fm = f m



duale :: (Enum a) => 

-- Yeah, it's going to go from a 




dualc a b v1 v2 = integrate (\y -> (v2 y) * (v1 y)) a b

dualbox = dualc 0 pi 

en' :: (Floating a) => Integer -> Vec a a
en' n x = (sqrt 2) * sin ((fromInteger n) * x) / (sqrt pi) 


psiexample = 3 .* (en' 1) .+ (en' 3)

{-
instance Dualable ContVec where
	dual f = \x -> ((integrate (\y -> ((unwrapcontvec x) y) * ((unwrapcontvec f) y)) 0 1) !! 10)

-}

{-
en' :: (Floating a) => Integer -> ContVec a (Complex a)
en' n x =  ContVec ((:+) (sin (fromInteger n) * x) (fromInteger 0))

-}




