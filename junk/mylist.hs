
data List a = Nil | Cons a (List a) deriving (Show)

mymap :: (a -> b) -> List a -> List b
mymap f Nil = Nil
mymap f (Cons x b) = Cons (f x) (mymap f b)

--mymap (+1) (Cons 1 (Cons 2 Nil)) 

examp = (Cons 3 (Cons 1 (Cons 2 Nil))) 

mytake _ Nil = Nil
mytake 0 _ = Nil
mytake n (Cons x b) = Cons x (mytake (n - 1) b) 

--mytake 2 examp

myprepend :: a -> List a -> List a 
myprepend a b = Cons a b

mycombine Nil c = c
--mycombine (Cons a Nil) c = Cons a c
mycombine (Cons a b) c =Cons a (mycombine b c)