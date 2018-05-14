{-
instance (Monoid b) => Monoid (a -> b) where
	mappend = fmap mappend

	-}

-- Sum monoid from Data.Monoid might be what I want
-- but also need to mutiply by dx
-- or possible more compicated weighting
import Data.Functor.Identity
import Data.Monoid
import Data.Foldable
import Data.Functor.Compose
integ :: (Functor f) => (Double -> f Double) -> f Double
integ f = fmap (*4) (f 3) 

-- a single fmap won't cut it
integ' :: (Monoid g) => (Double -> g) -> g
integ' f = mconcat $ fmap f [0, 0.1 .. 1]

{-
integ'' :: (Functor f) => (Double -> f Double) -> f Double
integ'' f = fold(fmap (*0.1) (map f [0, 0.1 .. 1]))
-}

instance (Monoid (f (g a))) => Monoid (Compose f g a) where
    mappend a b = Compose $ (getCompose a) <> (getCompose b)
    mempty = Compose mempty

-- Haskell gave a slightly more egneral typelcass. With Fractional. and differing input and output
integ''' :: (Functor f, Monoid (f (Sum Double))) => (Double -> f Double) -> f Double
integ''' f = (fmap getSum) $ fold g where g = (fmap . fmap) (\x -> Sum (0.1 * x)) (fmap f [0, 0.1 .. 1]) 

f x y z = x + y + z
f' x y = x + y
f'' x = Identity x
f''' x = Compose (\y -> Identity (x + y))


-- (integ''' (getCompose (integ''' f''')))

-- this does not work at all. f Double does not match like i had hoped.




-- f Double is possibly

-- If Num overloading is unsafe, then perhaps make my own typeclass Integrable

class Integrable f where
	weight :: (Fractional dx) => f -> dx -> f
	add :: f -> f -> f

-- Yes. This is good. See the typeclass level functions convert each layer of type into a layer of fmap

{-
instance (Fractional b) => Integrable (a -> b) where
	add f1 f2 = (+) <$> f1 <*> f2
	weight f dx = fmap (* dx) f 
	-}

--base case
instance (Num b) => Integrable b where
	add = (+) 
	weight = (*)

-- recursion
instance (Integrable b) => Integrable (a -> b) where
	add f1 f2 = add <$> f1 <*> f2
	weight f dx = fmap (weight dx) f 


instance (Num b) => Num (a -> b) where
	a + b = (+) <$> a <*> b
	a * b = (*) <$> a <*> b
	negate = fmap negate 
	abs = fmap abs
	signum = fmap signum
	fromInteger x = const (fromInteger x)



instance (Fractional b) => Fractional (a -> b) where
	a / b  = (/) <$> a <*> b
	recip = fmap recip
	fromRational x = const (fromRational x)




integ'''' :: (Fractional b) => (Double -> b) -> b
--integ'''' f = getSum $ fold (fmap (Sum . (\x -> x * (fromRational 0.1)) . f) [0, 0.1 .. 1])
-- fromRational is probably not necessary anyhow
integ'''' f = sum (fmap (\x -> (fromRational 0.1) * (f x)) [0, 0.1 .. 1]) 

-- can replace the fromrational factor with weights and the built in array with 
chebyshevweights a b n = undefined
chebyshevpoints a b n = undefined 


integ2 = integ'''' . integ''''
{-

instance Num b => Num (a -> b) where
      negate      = fmap negate
      (+)         = liftA2 (+)
      (*)         = liftA2 (*)
      fromInteger = pure . fromInteger
      abs         = fmap abs
      signum      = fmap signum
      -}


{-
integ'''' :: (Functor f, Monoid (f (Sum b)), Num b) => (Double -> f b) -> f b
integ'''' f = (fmap getSum) $ fold g where g = (fmap . fmap) (\x -> Sum (0.1 * x)) (fmap f [0, 0.1 .. 1]) 
-}

