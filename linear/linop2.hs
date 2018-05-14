--{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

import Data.Profunctor

class Linear f

instance (Applicative (f b), Num c, Profunctor f) => Linear (f b c)


class LinOp f

instance (Linear (f b c), Linear (f d c)) => LinOp ((f b c) -> (f d c)) 



-- matrix element ab with number c
dimap (a -> b) (\x -> x * c) v



