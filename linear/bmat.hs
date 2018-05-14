
{-
-- or are these typeclasses?

data BVec a b = 

-- BVec is a functor at least. BiFunctor


-- Type variables are describing the vecotr space
type BMat a b c d = (Monoid c, Monoid d) => (a -> c) (a -> d) (b -> c) (b -> d)
type SquareBMat a b = BMat a b a b
-- Do they need to be fields with * or is monoid enough

apply (M a b c d) (Vec v w) = (Vec ((a v) ++ (b w)) ((c v) ++ (d w))

-}

-- Double Profunctor. A BiProfunctor? With monoid constraint.

{-
data BlockOp a b c d = Blocks (a -> c) (b -> c) (a -> d) (b -> d)

-- Wait. I'm getting confused about Blocks vs BlockOp. Type parameters for input and outut sapces versus the blocks of the matrix.
type BlockRow a b = BlockOp a b () ()

type BlockCol a b = BlockOp a () b ()

type Block a = BlockOp a () () ()
--type Block a = BlockOp a Void c ()

bcompose :: (BlockOp a b c d) -> (BlockOp c d e f) -> (BlockOp a b e f)
bcompose (Blocks a b c d) (Blocks e f g h) = (Blocks ((a . e) ++ (b . g)) ((a . f) ++ (b . h)) ((c . e) ++ (d . g)) ((c . f) ++ (d . h)))


-- kind of feels like an fmap situation. if a and b and c and d implement BlockOp.
transpose (Blocks a b c d) = (Blocks (transpose a) (transpose c) (transpose b) (transpose d))



--associate (Blocks (Blocks a' b' c' d') (Blocks a'' b'' c'' d'') (Blocks a''' b''' c''' d''') d) = 

class BlockOp a b c d = 
-}

-- The most cleanest least fancy formulation
-- HOWEVER. I would like to do more things than just encode an orindary matrix funny
-- I want to have special methods
--data Matrix a = Block (Matrix a) (Matrix a) (Matrix a) (Matrix a) | Row (Matrix a) (Matrix a) | Col (Matrix a) (Matrix a)  | Scalar a 

-- Make Rows and columns with judicious none placement
-- Nothing enforces that I place None correctly. 
--data Matrix' a = Block (Matrix a) (Matrix a) (Matrix a) (Matrix a) | Scalar a | None

-- Simplest. Only powers of two
data Matrix a = Block (Matrix a) (Matrix a) (Matrix a) (Matrix a) | Scalar a deriving Show

--data BlockMatrix a = Num a => Block a a a a
-- Then we don't need the explicit base scalar case
-- BlockFunction a b c d is the space form
-- Then BlockMatrix impelements block function with further restriction of Num. and using (* a) as a->b

-- The functor and applicative instances are elementwise
instance Functor Matrix where
	fmap f (Scalar a) = (Scalar (f a))
	fmap f (Block a b c d) = Block (fmap f a) (fmap f b) (fmap f c) (fmap f d)

instance Applicative Matrix where
	pure x = Scalar x
	(<*>) (Scalar f) (Scalar x) = (Scalar (f x)) 
	(<*>) (Block f g h l) (Block x y z w) = Block (f <*> x) (g <*> y) (h <*> z) (l <*> w)

-- Matrix algebra
instance Num a => Num (Matrix a) where
	(+) a b = (+) <$> a <*> b 
	(*) (Scalar a) (Scalar b) = Scalar (a * b)
	(*) (Block a b c d) (Block e f g h) = Block ((a * e) + (b * g)) ((a * f) + (b * h)) ((c * e) + (d * g)) ((c * g) + (d * h))
	abs = fmap abs
	signum = fmap signum
	fromInteger = pure . fromInteger
	negate = fmap negate


-- identity matrix of size 2 ^ N
zeromat 0 = pure 0
zeromat n = Block x x x x where x = zeromat (n-1) 
identitymat 0 = pure 1
identitymat n = Block i z z i where i = identitymat (n-1) 
                                    z = zeromat (n-1)

scalarmat n x = fmap (* x) (identitymat n)


sigmaz = Block 1 0 0 (-1)
sigmax = Block 0 1 1 0
sigmap = Block 0 1 0 0
sigmam = Block 0 0 1 0

zerolike a = fmap (* 0) a

-- wait. The sharing here is quite interesting.
-- I bet haskell does not double allocate z
diagcopy a = Block a z z a where z = zerolike a


--size (identitymat 4) = 4
size = size' 0
size' n (Block a _ _ _) = size' (n+1) a
size' n (Scalar _) = n


transpose (Block a b c d) = Block (transpose a) (transpose c) (transpose b) (transpose d)
transpose x = x

invert (Block a b c d) = undefined -- schur complement is D invertible? -- Not all matrices are invertible, so should return a maybe anyhwo
invert (Scalar a) = Just (Scalar (recip a))

-- okay it seems crazy that I keep getting these repetitive patterns
--explciit kron is bad.
kron (Block a b c d) z = Block (kron a z) (kron b z) (kron c z) (kron d z)
kron (Scalar a) z = fmap (* a) z



fft x = undefined -- fft is probably not that hard.
-- Wait. Actually I'm not resricted to powers of 2^N am I? Because the blocking does not have to be balanced
-- But I do need a minimum of 2 coluomn. And there is a restriction on how much the # of rows and columns can differ.
-- One is minimally the log of the other. Without None.
-- If I include None, the strucures of the two matrices might not match.


-- decompositions
-- QR algorithm as a lazy sequence of values. That'd be fun.

-- could easily do things in an error checking monad that carries around the sizes of matrices and makes sure that 
-- everything matches.


--addMat

--multMat (Block a b c d) (Block e f g h) = (Block (multMat a f)   )
--multMat (Scalar x) (Scalar y) = (Scalar x * y)

-- Yeah. It would be cool if we could use laziness to create an infinite sequence of matrices
-- I think we need to push from the outside in. So maybe a scaling parameter needs to be multiplied.


class CrossBiFunctor f where
    blockMap :: (a -> c) -> (b -> c) -> (a -> d) -> (b -> d) -> f a b -> f c d

-- I need to extend notion of functions to block functions?

-- Any BiFUnctor has a trivial nstance of CrossBiFuntor where it ignores two functions


-- non recursive. Kind of a problem
data BlockFunctions a b c d = BlockFunctions (a -> c) (b -> c) (a -> d) (b -> d) 

-- I need a structure that blockMaps down the structure

-- The two types are  f (a -> (c,d)) (b -> (c,d)) ~ f a b

instance CrossBiFunctor (,) where
    blockMap a b c d (x,y) = ( a x  ++ b y  , c x  ++  d y )

--Either actually does not even have good CrossBiFunctor structure
--instance CrossBiFunctor Either where
--    blockMap a b c d (Left x) = 

data Basis = X | Y | Z

type DVec a b  = (Num b) => a -> b


instance Monoid (DVec a) where
    mappend x a =  \y ->  (x y) ++ (a y) 

-- Contravariant functor in basis.

type DVec3 = DVec Basis Float

-- List all the vector types

--Function Vec
-- Also DVec is kind of a vec, it is contravraint in the basis however
type Vec a b = (DVec a b) -> b 

-- ordinary array vec
type LVec a = Num a => [a]

-- a keyed list vec
type KVec a b = (Num b) => [(a,b)]

-- Hash Vect


-- restrictiosn on b
-- Num
-- Monoid? Allows addition. not scalar multiplication


-- Vec as a special case of matrix?



