import Data.Functor.Identity

type IntScheme a b = (a -> b) -> b

-- each higher order in list gives one higher power of epsilon
type Eps a b = [a b]  -- a should be a functor? Applicaitve? b should be a num
type EpsId b = Eps Identity b


-- hsould probably sepearte out the notion of functor in the list from the list

{-
(.+.) :: (Applicative a, Num b) => Eps a b -> Eps a b -> Eps a b
a .+. b =  (\x y -> (+) <$> x <*> y)  <$> a <*> b -- for each guy in epsilon list, add them

a .*. b = fmult (\x y -> (*) <$> x <*> y) a b


fmult f x@(a:as) y@(b:bs) = (f a b) : ((fmult f as y) .+. (fmult f x bs))
-- use a carry term?

-- allows higher order collection of terms. esplion squared and such
epssin a@(x:xs) = (fmap sin x) : (xs .*. (fmap epscos a))
epscos a@(x:xs) = (fmap sin x) : (xs .*. (fmap epssin a))

-}

--deriv f x = head $ f (x:[1]) .-. f (x:[])


--integrate :: (a->b) -> IntScheme a b -> b
--integrate f scheme = scheme f -- integrate is basically just apply? 

integrate :: (a->b) -> IntScheme a b -> b
integrate = flip ($)

intsin = integrate sin
-- Identity . sin is function double -> Indetity doubel
intsin' :: IntScheme Double (Identity Double) -> Identity Double
intsin' = integrate (Identity . sin)

dx a b n = (b-a)/ (realToFrac n)
linspace a b n = take n $ iterate (\x -> x+(dx a b n)) a
-- a very basic integration scheme. I shouldn't be using endpoints? Or both endpoints?
riemann a b n f = (foldl (+) 0 $ map f (linspace a b n)) * (dx a b n)





zerotopi = riemann 0.0 3.1459  

--quadfunctional :: (a -> a -> b) -> (a -> b) -> IntScheme a b -> IntScheme a b -> b 
-- Intschemes actual stack in typing. That stinks. Since the first scheme has to return a function that takes intschemes
-- this makes clear that I need to wrap in the identity functor. 

riemann' a b n f = fmap  (\x -> x * (dx a b n)) (foldl1 (\x y -> (+) <$> x <*> y) $ map f (linspace a b n))

--zerotopi' = riemann' (0.0::Double) (3.14::Double)
quadfunctional g j = (flip . integrate . flip . integrate) (\x y -> (g x y) * (j y) * (j x))

sin2 x y = Identity (sin x * sin y)
intsin2 = (flip . integrate . flip . integrate) sin2 

-- runIntegral 




