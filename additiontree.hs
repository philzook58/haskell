data Expr = Plus  Expr Expr | Times Expr Expr | Atomic Int deriving Show


eval (Atomic x) = x
eval (Plus x y) = (eval x) + (eval y)
eval (Times x y) = (eval x) * (eval y)


-- simple additions to this strcture include if else statements. Inequuality evaluationg to bools

-- The same thing but now with a type variable a

data Expr' a = Plus'  (Expr' a) (Expr' a) | Times' (Expr' a) (Expr' a) | Atomic' a deriving Show

instance Functor Expr' where
	fmap f (Plus' n m) = Plus' (fmap f n) (fmap f m)
	fmap f (Times' n m) = Times' (fmap f n) (fmap f m)
	fmap f (Atomic' x) = Atomic' $ f x 

eval':: (Num a) => Expr' a -> a
eval' (Atomic' x) = x
eval' (Plus' x y) = (eval' x) + (eval' y)
eval' (Times' x y) = (eval' x) * (eval' y)


--fold version?


-- fix version

data ExprF a b = PlusF a b | TimeF a b | AtomicF a deriving Show
data Fix f = In f (Fix f)
