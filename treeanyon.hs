import Prelude hiding (repeat)
import Data.Map (toList,fromListWith)
import Data.Complex
import Control.Monad       (liftM, ap)
infixl 7 .*
infixl 6 .+
infixl 6 .-

-- Jesus. b is the ampltide. a is the algerbiac thing
data W amp base = W { runW :: [(base,amp)] } deriving (Eq,Show,Ord) 

mapW f (W l) = W $ map (\(a,b) -> (a,f b)) l

instance Functor (W b) where
      fmap f (W x) = W $ map (\(a,p) -> (f a,p)) x

-- collect = accumulate on same index

-- typeclass equivalent basis. must implement converto to standard basis func 

instance Num b => Applicative (W b) where
    pure = return
    (<*>) = ap

--instance Num b => Monad (W b) where
--	return a = W [(a,1)]
--	x >>= f = W $ concatMap (\(W d, b) -> map (\(p,q) ->(p, q*b )) d) (runW $ fmap f x)


instance Num b => Monad (W b) where
    return x = W [(x,1)]
    l >>= f = W $ concatMap (\(W d,p) -> map (\(x,q)->(x,p*q)) d) (runW $ fmap f l)

W a .+ W b = W $ (a ++ b)

a .* b = mapW (a*) b

W a .- W b = W a .+ ((-1) .* W b)

type Q a = W (Complex Float) a

type V a b = W a b 

data Space = X | Y | Z deriving (Show, Eq)

type Dir3 = V Float Space

data Anyontype = Tau | Id deriving (Show, Eq)


-- Good first guess but wrong.
-- The tau have to fuse to identity but they won't
split :: Anyontype -> Q (Anyontype, Anyontype)
split Tau = return (Id,Tau) .+ return (Tau,Id) .+ return (Tau,Tau)
split Id = return (Id,Id) .+ return (Tau,Tau)

-- this rule must be good. Unless the whole framework is garbage
cup = split Id

fuse :: Anyontype -> Anyontype -> Q Anyontype
fuse Tau Tau = return Tau .+ return Id
fuse Tau Id = return Tau 
fuse Id Id = return Id
fuse a b = fuse b a -- I'm not sure I want this

-- No
--cap = fuse Id

-- this is bad 
-- The R matrix is only usable immediately after a splitting. And requires all three
topspin Tau Tau Tau = 4/5
topspin Tau Tau Id = -3/5
topspin _ _ _ = 0
 
{-
rmatrix a b c = makepolar 1 (pi * (topspin a b c)) 

over a b = topphase a b .* return (a,b)

under a b = (conjugate topphase a b) .* return (a,b)

-- the inability to easily make inverses really sucks.
--fmove Tau Tau Tau Tau Tau =
-- fmove  

-- needs 3 particles because it transforms the one underneath the other two
-- nope. needs 5 particles
braidover Tau Tau Tau = -- something something return (Tau,Tau,Tau) reversing the first two argument
-- if the two braided particles aren't Tau (one is Id), it doesn do anything
braidover a b c d e = return (b,a) 
braidunder Tau Id Tau = 
	-}

-- not a functor in Hask, but a functor in all sized hilbert spaces
-- instance Functor Kron V
-- bifunctor?
-- profunctor? contravaraint and covariant functors. suspcious
-- Natural transfromaiton (AB)C = A(BC)
-- suppoesedly some aspect of monads is a monoid on the level of natural transfromations

diagram = do (i,j) <- cup
             (fuse i j)


