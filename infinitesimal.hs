
data DualNum a = Horner a (DualNum a) | R a | Dx a 
-- probably not necessary to have both Dx and R as end cases.
-- Let's us take the horner form of a + dx( b+ dx (c + ))
-- maybe an infix construct +Dx would look nice


-- Instead we could take polynomials
data DualNumAlg a = Plus (DualNumAlg a) (DualNumAlg a) | Times (DualNumAlg a) (DualNumAlg a) | R a | Dx a

-- I suppose it never makes sense to talk of anything except polynomials of infinitesimals

-- is this the same as the infniite list form? I think not. The difference is that yes the output is the same
-- but thereis no way to take the infinite dx input? Well.. there was function composition.

-- I think the biggest spiritual difference is that it maintain the direvative in the f'dx form
-- rather than the f' form. The first is usually better

dsin :: DualNum Float -> DualNum Float
dsin (R x) = R (sin x)
dsin (Dx x) = Dx x
dsin (Horner x y) = Horner (sin x) (dcos y)

-- alternative use angle sum rule here
dsin3 (Horner x y) = (dsin (R x)) .*. (dcos y) .+. (dsin y) .*. (dcos (R x))

(R x) .*. (R y) = R ((*) <$> x <*> y)
(Horner x y) .*. (R z) = Horner ((R x).*. z) (y .*. z)
(Horner x y) .*. (Horner z w) = (Horner x y) .*. (R z) .+. (R x) .*. (Horner z w) .+. (Horner 0  (Horner 0 (w .*. y)))--foil 
-- so we need a 0 element to pull this off. additive identity.
-- the objects underneth need to be a field. ie have addition and multiplication
-- 


--The horner form does seem tighter and ultmately equivalent

-- include the derivation of the derivatvie of sin, which uses the angle sum rule
-- its something like this. Didnt actually look it up, but you get the idea
algsin (Plus x y) = (algsin x) .*. (algcos y) .+. (algsin y) .*. (algcos x)
algsin () = 
algcos ()

-- if we know how to compute the value on inifnitesmal polynomials
-- we can derive derivatives.Is this how derivatives are dervied in general?
-- The possibility of having multiple R that need or could be collected up is annoying
algderivdx f x = f (Plus (R x) (Dx 1)) .-. (f (R x))

removefirstr (R _) = (R 0)
removefirstr (Horner x y) = y

-- Since this mostly a funky labelled list we can take
taker n (Horner)

-- maybe we really should just implement it as a list.
-- gets us a ton of useful functions
-- head tail etc.


-- Is this the easiest way to go functional?
functionalsig :: DualNum (Float -> Float) -> DualNum (Float -> Float)



fsin (R f) = R (sin . f)
fsin (Dx f) = Dx f
fsin (Horner x y) Horner (sin . x) (fcos y) 

-- if i use the identity functor... hmm. I can combine the two cases. sin on fnctions should post compose
-- but on 

-- wait wait wait. This isn't functional differentiation. I'm not collecting function into values.
-- functionals are scalar valued functions of functions

-- a wild though: dependant types. My horner constructor DJ[x] could depend on the position x.


dsin2 (R x) = R (fmap sin x)
dsin2 (Dx x) = R x
dsin2 (Horner x y) = Horner (fmap sin x) (dcos2 y)
-- should work in both cases
example = dsin2 (R (Identity 3))
example2 = dsin2 (R (\x-> 3 * x))

-- also, is this really any different than a symbolic approach?


-- should dualnum itself be a functor?

-- if i combine this approach with a perturbation series in g... i might get something path integrally
-- of course, there is the big problem of non perturbative effects 
-- if one could push all this through, totally automized renormlaization is possible.
-- that'd be mega neato.

-- I was thinking of finite difference renormalization but if we're working on automatic differentiation anyhow...

data Functional =  Integral Intscheme function | Delta x func  -- Delta or Val x func 
-- how does one consturct functionals? Either you integrate or take the value
-- there might be a network of similar values Integral y g(x,y)f(y,z) 
-- Integrals are like lambda functions. They bind variables.
-- any unbound variables are like external paramters of the entire function
-- variables that are integrate over are dummy vars.
-- so a language for integrals ought to look like a syntz for lambda expressions
-- integral like a combinator that replaces that variable with a new variable that takes integration scheme

runintegral (Integral scheme f) params = (scheme f params)

--intscheme takes nothing and returns 
type Intscheme = (a -> b -> ... ) -> param -> (b -> ...)
type Intscheme2 = (a -> b) -> param -> b
f :: a -> b
integral :: (a->b) -> Intscheme2 -> param -> b

-- if secretly param ~ a since somestimesy ou want to undo integrla
(a->b) -> a -> b 
intscheme f x = f x -- really is id in disguise 


--nah get rid of param. Important but put in anothoer way
-- maybe not

type Intscheme2 = param -> (a -> b) -> b
f :: a -> b
integral :: (a->b) -> param -> Intscheme2 -> b


-- b may be a further func
-- I want to thunk intscheme? It is thunked if param = () unit
-- 
-- should intscheme return a list of refining results.
integral :: (a -> (b -> ...)) -> (Intscheme -> (b -> ...))
integral f = \intscheme -> (intscheme f)

-- what the hell am I doing with this intscheme business

-- we don't lose anything either. If intscheme is delta function like (the parameter is a position), 
deltascheme f = (\param ->  f param)

-- ((integral f) deltascheme) = f
--it could return the original function when curried on parameter
-- this could be wuite useful for functional differentiation.
-- sort of apply deltascheme to evey possible integral that contains J.

Ginv = \x y -> 1/(x-y)

-- okay but implied in integral is  you're ooperating on DJ expression 
-- essentially the DJ is marking how much J dependant there is
-- although not in what variable positions
dintegral (R f) = R (integral f)
dintegral (Horner f y) = Horner (integral f)  

actionish f = integral swap integral (\x y -> (f x) * (Ginv x y) * (f y))

generatingfunctional f = dexp (actionish f)   

-- comonad structure. 
-- as it is now i have functions that take and return infinitesmail polynomials.
-- but if i have a function that ... no... takes initiesmial polynomial returns value and cobind rebuilds the thing?


data FunctionalD = Horner a  a->(Intscheme FunctionalD) | R a
-- the inifnitesmial expression for functional differentaion
-- looks like f0 + int f1 deltJ dx
-- f0 is a function that may take many intschemes to produce a value.
-- Hmm, I guess going up in deltJ does not change the number of int schemes.
-- so FunctionalD is not distinct from DualNum?
-- They are all the same. We can also consider like the square of a functional. How does one differentiate that.
-- should somehow 

-- or we might consider finding the effective action via the legendre transformation. Then we have to take the
-- log of a functional.
type FuncionalD2 = DualNum (IntScheme->... -> Float)

data ListFunc a b = Cons (a -> ListFunc a b) | Nil b  -- This is a way of making functions that 
-- have arbitrary variables.

--DualNum (DualNum IntScheme -> ... Float) is the type of 
-- scalar rearrangements after the 
-- which means dualnum needs to implment functor and field.
-- which isn't hard. I guess fmap just passes the fmap on down thorguh the wrapper.
-- And I was already implementing field with .+.
-- it will get confusing though. 

-- sort of recurse down the thing
data DualNum2 a b = Horner a b | R a
-- 

fromfunctoD :: (Float -> Float) -> DualNum (Intscheme -> Float)
JplusdJ J =  Horner (integrate J) (R (integrate (\_ -> 1)))
dJ = Horner (\_ -> 0) (R (integrate (\_ -> 1)))
integralJ = R (integrate J)
JplusdJ2 = integralJ .+. dJ

-- might be cmvenenitn for dV currying to occur for dxdydz. 
-- also, any way to get fundmanetla thoerem of calclus to occur? 
-- The d of intscheme direciton seems possible. If param is upper limit of integral.

-- functional integral is a higher order intscheme. Interesting.
-- even higher order would be a functionalfunctional integral which is a sum over all possible functionals or functions.
-- and so on. 

realvaluedfunctional = Intscheme -> Intscheme -> ... ->  param ->  Float



