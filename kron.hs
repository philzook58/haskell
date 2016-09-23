
class Kron kron where
	-- traversal? Transpose
	associatel :: kron a (kron b c) -> kron (kron a b) c 
	associater :: kron (kron a b) c -> kron a (kron b c)
	commute :: kron a b -> kron b a
	-- more general? Using Unit here feels weird. Could ask it to be passed in

	curryl :: kron a b -> a -> kron () b
	curryr :: kron a b -> b -> kron a ()
	-- curryl :: kron a b -> c -> a -> kron c b

	-- linear bimap
	bimapL f g (kron a b) = kron (linearApply f a) (linearApply g b)  
	-- monadic apply

	-- burying tuples explicitly in here feels wrong.
	-- yeah. Isn't nice for scalar and vect
	linearApply :: ((a,b) -> kron a b) -> kron a b -> kron a b
	--linearApply :: (c -> kron a b) -> kron a b -> kron a b

	joinl :: kron (kron a b) c -> kron (a,b) c
	joinr :: kron a (kron b c) -> kron a (b,c)
	-- equivalent curried version
	-- return :: a -> b -> kron a b
	return :: (a,b) -> kron a b
	-- return in and of itself is the identity linearmap
	lmap f = bimapL f return
	rmap g = bimapL return g

--BiMonad ? As an extension of BiFunctor?

-- just tosses out a and b
data Scalar a b = Val Float

-- should always get () unit values in stuff?
-- scalar should implment a bunch of dumping on the floor.
-- kind of the Kron implementation of the Const Functor or something
-- or is it the Idnentity functor?
-- depends if I want to dump on the floor functions or if I want 

-- could I have nested scalars?
instance Kron Scalar where
	return (_,_) = Val 1
	-- inmplicit is that f has type signature that take ((),())
	linearApply :: (((),())  -> Scalar a b) -> Scalar a b -> Scalar a b
	linearApply f (Val x) = Val (innerval * x) where innerval = pullout ( f ((),())  )
	                                                 pullout (Val x) = x

    commute x = x
    associate 

-- should scalar just be 1-d Vecs?
-- Do I need a 0-D object that truly dumps?
-- sort of a Leaf tag. A Nil tag at bottom of tree.
data Dump a b = Dump
instance Kron Dump where
	return _ = Dump
	linearApply _ _ = Dump
	-- etc


instance Kron LVec where

instance Kron RVec where

instance Kron BiVec where
	return x = [(1, x)]


-- Maybe Vector should also be a typeclass
-- and bivec itself  is a vector of that type class
-- class (Vect kron) => Kron kron
-- since linear apply makes it a vector
-- and then scalar and Vec are vectors
-- but they are not bivectors

-- the pattern here is that a bifunctor is a wrapper around two distinct functors?

-- vectors [(a, e)] have a curried form also e -> [(a,())] which I've mentioned before

-- curryl kron (Vec a) (Vec b) -> a -> Vec b   


-- Try to make a kron that only requires it components to be vectors.
[(a, (v,w))]
-- to make a linear operator at the kron level
e1 -> [(a , e2 -> [(b , (e1,e2))])]
--or
e1 -> [(a , (e2 -> [(b , e2)],  e1))] --- an operatr that applied to 1 gives back a linear sum of tuples of 1 and operators to apply to 2

-- bimap does not generate complete set of operators
-- unless you can make linear sums of bimaps
[ (a, bimap f g) ]




