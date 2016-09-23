

type OneForm a b = (Num a, Basis b) => b -> [(a,())]

-- If the type of vector was. Actually, this is just a thunked vector. I wonder if haskell even respects thunks like this
	-- i think it doesn't. i think it emmiediatly eavlues them? Nope. It doesn't. you can thunk
type Vector a b = (Num a, Basis b) => () -> [(a,b)]

type Operator a b c = (Num a, Basis b, Basis c) => b -> [(a,c)] -- Maybe....



vec >>= (dagger vec) -- this evaluates to a list of values. If we have combinations, it'll make a list of its scalar product/magniude

--trace of op
-- cup and cap are epr pairs. = \sum | a> | a >
-- this is the specific defintion of cup and cap. Cup and cap should be required to be defined by the type class
-- this will curry to form () -> [(1,(e,e))]
cup basisenumeration () = map (\e -> (1., (e,e))) basisenumeration 
cap = dagger cup

loop op =  cup >>= (first op) >>= cap

--braiding operators -- requires you to be in the context of a pair
-- I'm clearly thinking half generally half specifically.
over (a,b) = [(1,(b,a))]
under = over
-- this is one of the redemiester moves
-- over >>= under = under >>= over = ident  
-- because of extra symmtrry here over >>= over = ident also.  This is symmettric monoidal
-- For anyon is not the case. That's a full 360 winding


-- might be cool to replace the floats that I have with text or symbolic expressions or a function that calcuates numbers
-- also might be cool for the class to automagically generate diagrams. Some sort of writer monad behavior in the backgroudn
-- building up the connectivty graph.
-- also might want to build up in full form and then make more efficient through trnasformations
--


-- inherits idntity propert from 1 multplicative dientity property
ident a = [(1.,a)]  
 
-- up indices go in the front, down indeces for in the back
-- why not [(a,b)] -> c  ?
-- Basis indices have a crtesin porduct which is also a basis. (n,m). I could use a list for bases except tuples is better 
-- if assoiciativity matters, and it will for anyons

-- this is clearly not haskell. But gets the point across. All objects are made of these
type Linear = (b,c,...) -> [(a,(c,d,e,f,..)]


--arrowizing
-- These things are arrows in general shape. Arrow OneForm Float Space for example = Arrow a b c
kron = 
*** = kron
first = -- convert function so it leaves alone guys
loop = -- involves dualizing somehow
>>> = >>=

-- the thing that is unacceptable from arrow is the duplicate operation &&&. That should not be implemented


--or 
type Operator = (Vector a b) -> (Vector a c) 
-- or 
type Operator = Operator -> Operator
--or
type Operator = OneForm -> OneForm
-- then vector >>= oneform applies a vector to a oneform
-- this is gorgeous duality.
-- Now kron can act well.

-- the dual of the vector. i guess seek down the list until you find the given basis element then return [(conj a,())]
-- and vice versa. Will this ever stop?

-- omega e = if (e == X) then 1 else 0

-- dagger should be applicable to oneform and vector?

-- basis should probably have an ordering
-- basislist may be infinite
--
class Basis a where
	enumerate :: [a]  --creates basisList. Must have a canonical enumartion? seems arbitrary. But necessary to dagger oneform
-- actually, enumerable is already a typeclass. Makes sense. Basis just needs to impement that

basisList = [X, Y, Z]

-- unless we want raise and lower functions that only go one way

-- could want to use a metric here
dagger :: OneForm a b -> Vector a b
dagger oneform =  map (\e -> ((oneform e),e)) basisList



-- not quite right. >>= multiplies operators from same space into same space... Does it? It should be agnostic to bases as long as they match

-- this is the identity we're trying to achieve

-- dual should be a very lightweight wrapper
data Dual a = Dagger a

-- joining two duals
instance Monad (Dual a)
	join Dual (Dual a) = a


-- the typeclass daggerable or adjointable
class Daggerable
	dagger a -> adag

dagger (adag >>= oneform) = (dagger oneform) >>= (dagger adag)

