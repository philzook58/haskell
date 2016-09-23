

\begin{code} 

import Control.Monad

data Reader b a = Reader (b->a)
data Writer b a = Writer b a

instance Monad (Writer e) where
	return a = Writer mzero x
	(Writer e a) >>= f = f a
instance Monad (Reader e) where
	return a = (\_ -> a)
	a >>= f = \e -> (f (a e)) e



\end{code}



