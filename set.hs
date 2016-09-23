import Control.Arrow

type Set a = a -> Bool
-- Note that according the this is the power set of type a.  is of size 2^a since bool is of size 2

-- you can easily do union, intersection of sets via boolean operators on the result


--union (\x-> True) (\x -> True) $ ()

union a b = or . (a &&& b) 
intersect a b = and . (a &&& b) 
complement a = not . a 
empty _ = False
total = complement empty

-- possible difficulty in that you can't introspect

-- easily compute preimage of B under f:a->b
preimage f b = b . f

-- define via preimage 
--
-- use unit interval as canconical.
-- then use preimage to define other sets.
-- defines easy element operation
-- elem x = and (x <= 1, x>=0) 

-- Then use union and such also


-- http://cs.ioc.ee/ewscs/2012/escardo/slides.pdf

-- (a-> Bool) -> Bool
-- takes sets as defined before. now a is in covariant position. Useful?
-- "charecterstic funciont of existential qualifier". Given statement f::(a-> Bool), does it hold in set?
-- Thisis the type E_A.
-- quesiton is x in A. EA (\y -> y == x )


--equal f g = forevery(\a -> f a == g a)


-- elem EA x = EA (\y -> y == x )

-- there is also forall. Forall can be defined in terms of there exists. Does there exist one where p not true.
-- De Morgan's law


-- it's implied that the definition of EA has a search built in it
-- EA f = try f on every object in A until find true. If none return false
-- a being in voraint poistions means deifnition of EA produces a.

-- if EA was a union
type A = Integer
--type SetA = A -> Bool -- Noooo... maybe not
type Prop = A -> Bool
type EA =  Prop -> Bool -- EA is SetA
type SetA = EA

union' :: EA -> EA -> EA
union' ea eb = \p -> or (ea p, eb p)

empty' :: EA
empty' _ = False  

total' :: EA
total' _ = True
toall' ea = \p -> (not . ea) (not . p)
equal' ea eb = (toallp totalp) (\p -> ea p == eb p)
subset' ea eb = (toallp totalp) (\p -> ea p == eb p)

iselem' :: EA -> A -> Bool 
iselem' ea a = ea (\b -> a == b) -- (== a) is shorter

thereexistsin' ea = ea
forallin' ea = toall' ea

-- propositions about integers are also countable? Nooo... they are uncountable. They = Cantor sequence things.
-- And I guess the propositions  
-- so using cantor sequences as propositions and Escardo/Berger we get equality 
totalp :: (Prop -> Bool) -> Bool
totalp _ = True
toallp ep = undefined
-- we also need the set of all propoisitions Prop = A->Bool
-- the existential set of propoisitions (Prop -> Bool) -> Bool
-- gets us a total function 

-- hmmmmm. So. suppose EA represents the interval from [0,1]. and you hand me a property p. How do I answer it? 
-- p has to be continous? which is tough for functions from reals to bools.
-- 
-- continous function is the pair of the function itself and the epsilon delta.
-- second function given an epsilon, gives a delta.
data Continuous = ContFunc (Float -> Float) (Float -> Float)

-- by the types this is how it works. if f = a->b then fdelta = b -> a
-- forard function and kind of weak funny inverse function
ccompose (g,gdelta) (f,fdelta) = (g . f, fdelta . gdelta)
csum (g,gdelta) (f,fdelta) = (\x -> f x + g x, \e -> min (fdelta (e/2)) (gdelta (e/2)))
cmult (g,gdelta) (f,fdelta) = (\x -> (f x) * (g x) , \e ->  min (fdelta (sqrt e)) (gdelta (sqrt e))) -- Maybe. haven't proven

-- Can build up complicated functions from a basic set.
x = (\y -> y, \e -> e) -- the identity function
const' c = (\_ -> c, \_ -> 1000000000) -- not particular elegant. not a tight bound. delta can vary arbitrary far
x2 = cmult x x
x3 = cmult x2 x
-- already got polynomials

-- I think it is clearly possible to define an integration that has bounds given the continuity condition.
-- currying on (f,fdelta) returns a function that can deliver you an accuracy acc if you want it
-- if f and fdelta are mutilvariate in an uintegrated parameter then that should be carried through
integ (f,fdelta) acc = undefined
-- could we return a seqeunce of strictly upper and lower bounds on the integral?



-- I think returning a function that can be computed to arbitrary accuracy is a reasonable approach to the reals.

type CauchyReal = (Integer -> Rational, Rational -> Integer) -- Integer -> Rational
-- a combo of the sequence that gives the rational for the integer value and the N value to go greater than for the
-- epsilon gurantee of closeness.

-- These things all have the types of isomorphisms. Of Profunctors.



-- hmmm. Is this not the vector space over bools kind of? It parallels my vector ideas quite closely.
-- the set function is a 1 form
-- the exists function is a vector of form (e -> Float) -> Float
-- The trouble with vectors is that Float (my stand in for real) is not finite.
-- 
-- we wanted e to be a wavefunction x -> float? or e = x itself?. And then integration had to happen.


-- what about 
-- (a-> Bool) -> a
-- decidable equality apparently. both a are covraint
-- a type of a map from sets of objects to the objects themselves. f(evens) = 2, f(odds)=3, etc
-- return example or counterexample


-- Bool -> a 
-- easy to decide equality. Only two options 


subsets :: (a-> Bool) -> (a-> Bool) -> Bool
subsets b = 

-- subset determination is not obviously possible. Is A < B? Can't figure it out without enumeration.
-- (if A x = true then B x = true) impliles A < B
-- (P => Q) => R
-- this seems like that dependant type stuff.

-- just thinking. the set of all subsets of b. if A is elem of that it is a subset.


-- how to do sup?
-- also typeclass lubp. says that set implemenets sup and inf.





-- should Set be a type class where you fullfill a memeber function? 

-- could make set a list of intervals for R. 
-- or a finite set for other things.


-- This is building sort of a DSL for sets
-- only allowing countable intersections.
-- wasn't that in the definition of point set topology
data RSet = Union [RSet] | Intersect [RSet] | Empty | All | Point Float | OInterval Float Float | Condition Float -> Bool
-- a closed interval can be made with the union of an open interval and the endpoints
close i@(OInterval a b) = Union [i, Point a, Point b]
close (Union as) = Union $ map close as
--close (Intersect as) = Intersect $ map close as -- ? Is that how you close an intersection?


-- typeclass Metric
--      d:: a -> a -> Float


-- More thoughts on subsetting. Because of the curry howard stlye analogy between categories (preorders) and types and logic.
-- perhaps we should encode subset propositions as types

type SubSetProp a = Set a -> Set a
type Evens = Set Int
type AllInts = Set Int

evens :: Set Int
evens 0 = True
evens 1 = False
evens x = evens (x-2)  

allints :: Set Int
allints _ = True

evenDints :: Evens -> AllInts
evenDints a = a 
-- I'm clearly thrusting at something and missing


