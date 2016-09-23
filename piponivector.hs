import Prelude hiding (repeat)
import Data.Map (toList,fromListWith)
import Data.Complex
import Control.Monad       (liftM, ap)
infixl 7 .*
infixl 6 .+
infixl 6 .-

-- Jesus. b is the ampltide. a is the algerbiac thing
data W b a = W { runW :: [(a,b)] } deriving (Eq,Show,Ord) 

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



x :: Dir3
x = return X

u,v,w :: V Float Space
u = return X .- return Y
v = return X .+ 2.* return Y
w = return Y .- return Z

 -- outer pdoruct basis
cup :: (Space,Space) -> V Float ()
cup (i,j) = case (i,j) of
   (X,X) -> return ()
   (Y,Y) -> return ()
   (Z,Z) -> return ()
   otherwise -> 0 .* return ()

-- v >>= dual v 
dual :: V Float Space -> Space -> V Float ()
dual v i = do
    j <- v
    cup (i,j)


-- returns tensor product of 
cap :: () -> V Float (Space,Space)
cap () = return (X,X) .+ return (Y,Y) .+ return (Z,Z)


