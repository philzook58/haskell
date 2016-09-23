import Prelude hiding (repeat)
import Data.Map (toList,fromListWith)
import Complex
infixl 7 .*

data W b a = W { runW :: [(a,b)] } deriving (Eq,Show,Ord)

mapW f (W l) = W $ map (\(a,b) -> (a,f b)) l


instance Functor (W b) where
	fmap f (W a) = W $ map (\(a,p) -> (f a,p)) a