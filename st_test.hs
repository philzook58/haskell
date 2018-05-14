import Control.Monad.ST
import Data.STRef
import Control.Monad
import Data.Vector.Unboxed.Mutable as M
import Data.Vector.Unboxed as V
 
 
sumST :: Num a => [a] -> a
sumST xs = runST $ do           -- runST takes out stateful code and makes it pure again.
 
    n <- newSTRef 0             -- Create an STRef (place in memory to store values)
 
    Control.Monad.forM_ xs $ \x -> do         -- For each element of xs ..
        modifySTRef n (+x)      -- add it to what we have in n.
 
    readSTRef n                 -- read the value of n, and return it.




makeArray = runST $ do
    n <- newSTRef [1,2,3]
    readSTRef n

makeArray' = newSTRef 10 -- >>= readSTRef

makeArray'' = do 
	a <- newSTRef 10 
	b <- newSTRef 11
	return (a,b) 



makeVec = runST $ do
	v <- M.replicate 3 (1.2::Double)
	write v 1 3.1
	V.freeze v 
