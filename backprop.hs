{-# LANGUAGE RankNTypes, LambdaCase #-}

-- import Data.Functor.Identity
-- import Data.Functor.Const.Compat
import Control.Lens

data V2 a = V2 a a

type RD a b = a -> (b, b -> a)
{-
type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t
type Lens' s a = Lens s s a a
-}
-- id is a Lens
id' :: Lens' a a
id' = id
{-
fst' :: Lens' (a,b) a
fst' = _1

snd' :: Lens' (a,b) b
snd' = _2

_2 and _1 are linear functions

-}
{-
set :: Lens' s a -> a -> s -> s
set l b = runIdentity . (l (\_ -> Identity b))
 

view :: Lens' s a -> s -> a
view l = getConst (l Const)
-}  

--mul :: Number a => Lens' (a,a) a
--mul = 

-- add :: Number a => Lens' (a,a) a
-- add f (x,y) = fmap  

add' :: Num a => Lens' (a,a) a 
add' = lens (\(x,y) -> x + y) (\case (x,y) -> \d -> (d,d))

sub' :: Num a => Lens' (a,a) a 
sub' = lens (\(x,y) -> x - y) (\case (x,y) -> \d -> (d,-d))


mul :: Num a => Lens' (a,a) a 
mul = lens (\(x,y) -> x * y) (\case (x,y) -> \d -> (d*y,d*x))

recip' :: Fractional a => Lens' a a 
recip' = lens recip (\x -> \dx -> -dx / (x*x) )

div' :: Fractional a => Lens' (a,a) a 
div' = lens (\(x,y) -> x / y) (\(x,y) -> \d -> (d/y,-x*d/(y * y)))


{-
mul' :: Num a => Lens' (V2 a) a 
mul' = lens (\(V2 x y) -> x * y) (\case (V2 x y) -> \d -> (V2 y x))
-}

dup :: Num a => Lens' a (a,a)
dup = lens (\x -> (x,x)) (\_ -> (\case (y,z) -> y + z))
{-
dup' :: Num a => Lens' a (V2 a)
dup' = lens (\x -> V2 x x) (\_ -> (\case (V2 y z) -> y + z))

add'' :: Num a => Lens' (V2 a) a 
add'' = lens (\ (V2 x y) -> x + y) (\case (V2 x y) -> \_ -> V2 1 1)
-}
square :: Num a => Lens' a a
square = dup . mul

--square' :: Num a => Lens' a a
--square' = dup' . mul'

par :: Lens' a b -> Lens' c d -> Lens' (a,c) (b,d)
par l1 l2 =  lens (\(x,y) -> (g1 x, g2 y)) (\(x,y)-> \(a,b) -> (s1 a x, s2 b y)) where
                g1 = view l1
                g2 = view l2
                s1 = set l1
                s2 = set l2 

fourve :: Num a => Lens' a a
fourve = square . square -- Isn't working... 
-- evaluation is working
-- Is the whole idea wrong?
-- Yes. Setter composition does not work like derivatives
-- you do sequential getting from the intermiedate types... that is right
-- Then you 
-- Wait isn't this right?
--f s b =  


threeve :: Num a => Lens' a a
threeve = dup . ((par square id) :: Num a => Lens' (a,a) (a,a)) . mul

-- Sort of the nondestructive version of mult. Like a quantum circuit or something
onemore :: Num a => Lens' (a,a) (a,a)
onemore = lens (\(x,xn) -> (x, x*xn)) (\(x,xn)  -> \(d1,d2) -> (d1 + d2*xn, d2*x) )

-- These are probably already in lens (more specifically Isos)
-- The rule of thumb may be actual lens cannot be used since we need to zero pad
-- projections rather that maintain data
-- But Isos are fine
assoc :: Lens' ((a,b),c) (a,(b,c))
assoc = lens (\((a,b),c) -> (a,(b,c))) (\((a,b),c) -> \(da,(db,dc)) -> ((da,db),dc))

-- swapped from Lens should work
swap ::  Lens' (a,b) (b,a)
swap = lens (\(a,b) -> (b,a)) (\(a,b) -> \(db,da) -> (da, db))
-- can also be done using assoc
-- onemore' = (fst' . dup) . assoc . mul
snd' :: Num a => Lens' (a,a) a
snd' = lens (\(x,y) -> y) (\(x,y) -> \dy -> (0,dy) )

fst' :: Num a => Lens' (a,a) a
fst' = lens (\(x,y) -> x) (\(x,y) -> \dx -> (dx,0) )


pow :: Num a => Int -> Lens' a a
pow n = dup . (foldr (.) id (replicate (n-1) onemore)) . snd'

sin' :: Floating a => Lens' a a
sin' = lens sin (\x -> \dx -> dx * (cos x))

cos' :: Floating a => Lens' a a
cos' = lens cos (\x -> \dx -> -dx * (sin x))

cmul :: Num a => a -> Lens' a a
cmul c = lens (* c) (\x -> \dx -> c * dx)

exp' :: Floating a => Lens' a a
exp' = lens exp (\x -> \dx -> dx * (exp x))



-- Can do less well typed version using lists
{-
headdup :: Num a => Lens' [a] [a]
headdup = lens (\x -> (head x) : x) (\_ ->(a:b:xs) -> (a + b) : xs)
-}
{-
listsum :: Num a => Lens' [a] [a]
listsum = lens (\x -> sum x) (\x -> \_ -> fmap (const 1) x)
-}
grad :: Num a => Lens' b a -> b -> b 
grad f = set f 1  

{-
-- Vectorized Operations
sum :: Number a => Lens' (Vector a) a
fill :: Number a => Int -> Lens' a (Vector a)
add :: Lens (Vector a, Vector a) (Vector a)
kron :: Lens (Vector a, Vector a) (Vector a)
elemmul :: Lens (Vector a, Vector a) (Vector a)
det :: Lens (Matrix a) (Matrix a)
dot :: Lens (Matrix a, Matrix a) (Matrix a)
-- with det and dot, we could make a differentiable Hartree Fock


-- It might make sense to use a traversal?
-- Or is traversal for use in a monadic/applicative context?
-- like reinfrocement learning

-- functional differentiation.

-}


{-
-- according to the usual scehem
We'll need a  (grad -> Lens a b)

RecursiveLens a b = Lens a (b, RecursiveLens a b) 

-}

--val = view
--deriv = set



