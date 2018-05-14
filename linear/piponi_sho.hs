import Prelude hiding (repeat)
import Data.Map (toList,fromListWith)
import Data.Complex
import Control.Monad       (liftM, ap, replicateM, (>=>))
infixl 7 .*
infixl 6 .+
infixl 6 .-

-- Jesus. b is the ampltide. a is the algerbiac thing
data W b a = W { runW :: [(a,b)] } deriving (Eq,Show,Ord) 

mapW f (W l) = W $ map (\(a,b) -> (a,f b)) l

instance Functor (W b) where
	fmap f (W x) = W $ map (\(a,p) -> (f a,p)) x


instance Num b => Applicative (W b) where
    pure = return
    (<*>) = ap

instance Num b => Monad (W b) where
    return x = W [(x,1)]
    l >>= f = W $ concatMap (\(W d,p) -> map (\(x,q)->(x,p*q)) d) (runW $ fmap f l)

W a .+ W b = W $ (a ++ b)

a .* b = mapW (a*) b

W a .- W b = W a .+ ((-1) .* W b)

type Q a = W (Complex Float) a

type SHO = Q Int

sqrtInt :: Int -> Complex Float 
sqrtInt =  sqrt . fromIntegral

vac :: SHO
vac = return 0

--myiterate n op acc = if (n>0) then myiterate (n-1) op (op >=> acc) else acc
--excited n = vac >>= (myiterate n adag )
excited n = if (n>0) then (excited $ n-1) >>= adag else vac

-- excited n = vac >>= iterate (>=> adag) adag !! (n-1) -- doesn't work for n =0
-- These defintions should be in terms of one another. By attaching a cup or cap?
-- Is it the infinite basis that is troubling?
{-
a :: Int -> SHO 
a n = if (n > 1) then [(n-1,sqrtInt n)] else []
-}
adag :: Int -> SHO 
adag n = W [(n+1,(sqrtInt $ n + 1))]


