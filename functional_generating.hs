-- ok
-- i have some good stuff hanging around
-- this power series guy is good.
-- and integration

-- the differentce between differentation and power series
-- power series is numbers

-- infinitesimal.hs is my differntiation guy
-- update to use Num typeclass

-- numberfunction.hs is the integration library
-- it's fairly insane how i've named these files.

-- differentation is functions of the value

-- Kind of the same though
-- especially with this function as Num idea
-- The automatic differentation and the functional power series are similar.

-- automatic dif can be converted to power series by just evaluating everything at 0.
-- the backwards direction is no go.
-- you can't build the true function that will evaluate in constant time
-- you can do it with a truncation
-- you could also do it in a oduble type that has a built in epsilon.
-- onyl need to evaluate finitely many terms.


-- how chain rule works is different for AD list vs power.
-- how composition works is different for AD list vs power. (I guess these are the same point)
-- The power series library gives a hint how to invert
-- there is an interesting issue though.
-- the ad series will never be able to give an exact inverse for first term
-- again you will have to truncate the inversion process
-- newton iteration


-- might be important for a new datatype that has explicit zero type so that we can 
-- efficiently avoid integrating goddamn zero functions.
-- Maybe const instead of just zero?
-- le'ts avoid efficiency to start
data Functional a b = FZero | FFunc (a -> b)
data Functional' a b = FConst b | FFunc' (a -> b)


instance (Num b) => Num (Functional a b) where
	FZero + f = f
	f + FZero = f
	(FFunc f) + (FFunc g) = FFunc $ (+) <$> f <*> g

-- and so on
-- conal elliot has a great idea of using a Maybe for zero.

-- nilsson has dirac delta functions. I'm really going to want those.
-- I probably can get them. integration can be replaced with weighted integration
-- and a dirac functional becomes just sampling.

-- I think i may have just confused myself by reading conal elliott again. He has a lot going on

-- Conal Elliott uses a new type of LinearMaps to define derivatives

-- He has a lot of the ideas I've been playing around with

-- InnerSpace as a newtype

-- I have decided that the monadic enforcement of linearity overcomplicates things. I think.
-- also from the persective that we're computing the power series in the displacement
-- the function AREN'T linear in the displacement.

-- Conal has implicit (explicit?) an enumeration of his basis. Something that I really want to avoid.

-- Would starting from a sampled function be that bad? I'm implicitly sampling at the integration step anyhow?

-- There's two programs. 
--1. using haskell functions as vector spaces. Actual functional differentation would be pretty cool
--2. QFT generating functions (which even on a discrete grid would be pretty cool.)

-- the sampling procedure is really a modification of jGj. It has kind of a smoothing step
-- or sampling with dirac + interpolation step.
-- in some sense, it is perfect with resepct to j.
-- it's just that we need to use a restricted class of jGj
-- that convolve in the fourier domain and the porject. shannon whittaker style.

-- simplifications : I want to inject into R. so linear maps are always one dimensional on a side

-- conal has then recursive j structure in his linmaps. 
-- putting one more argument to each successive derivative

-- This is the same type-ish of D below
-- it IS nice that the zeroth order has no dependence on deltaJ
-- and actually, we want deltaJ to be defifferent so we can pull out different x positions
-- AND we want deltaJ to be integration schemes themelves (distributions).
-- well, wait JGJ also needs to decide how it procedes. i guess J could be distributions too
-- HMMM, Then we can decide 
-- this structure is not just a list

-- go FunctionalMap is such a bad name. What does it even mean?
-- Brainstorm: FuncList 
-- 
data FunctionalMap a b = Val b (FunctionalMap a (a->b))
-- This may be more appropriate. The intention is for dx to be the type of the input to the function
--data FunctionalMap a b = Val (a->b) (FunctionalMap a (a->b))

-- I suppose dx and x may be considered not be the same type
-- one being in the tangent space and the other the actual basespace? (Lie groups and shit) uh....
-- no.....


-- repeats x forever
constFM :: b -> FunctionalMap a b
constFM x = Val x (constFM (const x))

-- this pattern of constFM seems liek it should be extendible
--biFM :: (b -> a -> b) -> b -> FunctionalMap a b
--biFM f x = Val x (biFM (f x))

-- has just a once then is zero forever. This means a is a Num to match type of 0
scalarFM :: Num b => b -> FunctionalMap a b
scalarFM a = Val a (constFM (const 0)) 

expFM :: Floating b => FunctionalMap a b -> FunctionalMap a b
expFM fm@(Val x oneform) = Val (exp x) (oneform * (pushFM (expFM fm)))

-- note I've already made functions Nums

-- There are a LOT of const combinators.

-- maybe not THAT bad. fmap . fmap is a pattern to push inside multiple layers of functor
-- the first fmap fmaps inside functionalmap, then next layer fmaps inside the extra (->)
instance Functor (FunctionalMap a) where
	fmap f (Val x deriv) = Val (f x) ((fmap . fmap) f deriv) -- YIKES. This is incomprehensible

instance Applicative (FunctionalMap a) where
	pure x = constFM x
	-- maybe this is impossible? It's hard at least
	-- wait <*> implements the chain rule!
	-- Maybe this is a different typeclass Differentiable which implements the chain rule
	-- it needs a num typeclass constraint on b. Applicative can't do that
	-- also this isn't right yet. fderiv needs an extra a. 
	-- hmm. An argyment could be made that fmap should be the chain rule too.
	--(Val f fderiv) <*> xs@(Val x xderiv) = Val (f x) ( xderiv * (fderiv <*> xs))

pushFM :: FunctionalMap a b -> FunctionalMap a (a->b) -- certainly pushFM has a more general type
pushFM = fmap const


headFM (Val x _) = x
tailFM (Val _ xs) = xs
--applytailFM :: a -> FunctionalMap a b -> FunctionalMap a b
--applytailFM dx = applydxFM dx $ tailFM


-- This is like weird currying. Currying does not atuomatically push inside the type.
pushinFM ::  (a -> FunctionalMap a b) -> FunctionalMap a (a->b)
pushinFM f = Val (\dx -> headFM (f dx))  (pushinFM (\dx -> tailFM (f dx)))

-- is this a monad transformerstack?
-- our are we sequencing? This pattern feels familiar.

pushoutFM ::  FunctionalMap a (a->b) -> (a -> FunctionalMap a b) 
pushoutFM = applydxFM

-- This is cmap.
applydxFM :: FunctionalMap a (a->b) -> a -> FunctionalMap a b
applydxFM (Val f fderiv) dx = Val (f dx) (applydxFM fderiv dx)

-- this is a fold?
--applyalldxFM :: FunctionalMap a (a->b) -> [a] -> FunctionalMap a b
--applyalldxFM fs@(Val f f') (dx:dxs) = Val (f dx) : (applyalldxFM g dxs)  where g = applydxFM f' dx

-- maybe lenses would help
-- okay.This function applies b, the point, to the function to leave pure differential behind
pointevalFM :: FunctionalMap a (b->c) -> b -> FunctionalMap a c
pointevalFM fs@(Val f f') x = Val (f x) (pushinFM (\a -> pointevalFM ((pushoutFM f') a) x))





-- f of g.
-- output of g has to equal input of f 
-- total dx is ds og original g
-- total output is a value in fout
-- think of f'(delg dx) (g)

-- wait 
-- shouldn't f take g functions to a new range?

chainFM :: FunctionalMap gout (gout -> fout) -> FunctionalMap dx (dx -> gout) -> FunctionalMap dx (dx -> fout)

--chainFM :: FunctionalMap gout ((dx -> gout) -> (dx -> fout)) -> FunctionalMap dx (dx -> gout) -> FunctionalMap dx fout

-- I need to trasnform ALL of the gout of f' into dx
--chainFM fs@(Val f f') gs@(Val g g') = Val (f . g) (applydxFM f' (g dx) * g') 

--chainFM fs@(Val f f') gs@(Val g g') = Val (f . g) (pushinFM (\dx -> (chainFM (chainFM f' (applydxFM g' dx)) gs))) 
chainFM fs@(Val f f') gs@(Val g g') = Val (f . g) (chainFM f' ())


-- hmm this does not require Num (->)? It already pushes it's way through all the arrows.
-- uh. No. No it doesn't. nevermind
instance (Num b) => Num (FunctionalMap a b) where
	fromInteger x = scalarFM (fromInteger x)
	(Val x xderiv) + (Val y yderiv) = Val (x + y) (xderiv + yderiv)
	xs@(Val x xderiv) * ys@(Val y yderiv) = Val (x * y) ((pushFM ys) * xderiv + (pushFM xs) * yderiv) -- product rule
	negate = fmap negate 
	abs = fmap abs
	signum = fmap signum

-- okay then essentially do conal elliots stuff except with a->b rather than a -o b


-- conal elliots conversion to functions and lin maps
-- is interesting in that it feels like it might apply the interpolative decomposition




-- again can add a Maybe or a | Zero for 
--expFM (Val b deriv) = Val (exp b) (\x -> (deriv x) * (exp b))

--integ'''' :: (Fractional b) => (Double -> b) -> b
-- might be sensible to let a be the tuple (x,y,z,t) since these integrals always come together
type Distribution a b = (a -> b) -> b


distributify integmethod j = \f -> integmethod (\x -> (j x) * (f x)) 
distributify' integmethod j f = integmethod (f * j)
integinner integmethod j f = integmethod (f * j)
distributify'' = integinner
dualify = integinner 

-- a dirac distribution will just sample at that point.
dirac a f = f a
dirac' = flip ($) 

instance (Num b) => Num (a -> b) where
	a + b = (+) <$> a <*> b
	a * b = (*) <$> a <*> b
	negate = fmap negate 
	abs = fmap abs
	signum = fmap signum
	fromInteger x = const (fromInteger x)

--jgj :: (Num a, Num b) => (Distribution Double a) -> (Distribution Double b) -> (Double -> Double -> Double) -> Double
-- no something isn't right here
jgj :: (Distribution b c) -> (Distribution a (b -> c)) -> (a -> b -> c) -> c
jgj j1 j2 g= j1 (j2 g)


--jjg j g = jgj j j g

-- as I've noted before, this is kind of funky. since the intention is that 
-- Then implement 

-- stream datattype for automatic differentiation
data D a = D a (D a)
-- need to import Data.Stream, also install it. Not installed by default
--type D'' a = Stream a

-- maybe version optimized for zeros. Basically a list. Let's not kid ourselves
data D' a = D' a (Maybe (D' a))
type D''' a = [a]


-- functional power series may be more appropriate since we almost never want to evaluate J not equal to zero?
-- is this true? effective lagranigan manipulations and mean field theory may indeed make 
-- J not zero.

-- god. i should be doing literate haskell.

-- this is acceptable one we give the Num typeclass to functions
instance (Num a) => Num (D a) where
	fromInteger = undefined

-- way down the line, but for the time direction, should I use time ordered
-- or keldysh ordering  -- periodic with half of domain time reversed. makes sense to me
-- periodic time, 
-- conjugate periodic time psi(0)=psi*(T)?
-- go to zero time?

--integtrap :: (Fractional b, Fractional c) => c -> c -> Integer -> (c -> b) -> b
--integ'''' f = getSum $ fold (fmap (Sum . (\x -> x * (fromRational 0.1)) . f) [0, 0.1 .. 1])
-- fromRational is probably not necessary anyhow
-- huh. Yes. the two dx and dx' work in different types and it is not obvious how to cast them
-- This is nuts.
-- could pass in a measure function (c -> b) that will mostly do nothing.
-- but then integtrap has to know the type?
-- insist on applicative to get pure? dx' = pure dx

-- b and c have to be comparable in some way. However not identical
-- maybe c has to be rational?

-- maybe always fo from -1 to 1. and redfine your f if you want something different
-- hasResolution is a curious option

integtrap a b n f = dx' * ((sum (fmap f [a, a+dx .. b])) - ((f a)/2) - ((f b)/2)) where 
	l = b - a
	dx = l / (fromInteger n)
	dx' = realToFrac dx



