
--ycomb :: (a -> b) -> a -> b

--ycomb f x = f (ycomb f x)
-- ycomb and fix are the same. This def is porbably wrong
-- should not have x.
--fix'' f = (ycomb f)



-- ycomb (const 3) 4 

-- = (const 3) (ycomb (const 3) 4) = 3

-- these don't type check
--ycomb' f = (\x -> f x x) (\x -> f x x)
--ycomb'' f = f (\x -> f x x) (\x -> f x x) -- one reection 


newtype Rec a = In { out :: Rec a -> a }

y :: (a -> a) -> a -- Takes a function and returns a value. 
y = \f -> (\x -> f (out x x)) (In (\x -> f (out x x)))



-- fix and ycomb are the same thing. Hard to see
fix :: (a -> a) -> a
fix f = let x = f x in x
-- the second x refers to the first x. Hence the recursion. It's kind of twisted that haskell allows this

-- or not. Scheme has let and letrec. I recall letrec getting mentoined in littler schemer

-- similar use of let.
-- resursive lets are primitive 
-- https://mail.haskell.org/pipermail/beginners/2009-February/000901.html
--
ones' = let x = 1:x in x
-- this is apparently more efficicent
repeat' x = let xs = x : xs in xs
-- than this more obvious one. "Tying the knot" Function calling versus mere referral
repeat'' x = x : repeat'' x

cyclic = let x = 0 : y
             y = 1 : x
         in  x

-- simple no nrecrusive let
-- x = y in z
-- (\x -> z) y 







hello = fix (const "hello")

ones = fix (1:)

fix' f = f (fix' f)  

-- fix' makes sense. fix makes no sense.

--fix'' f = let x = f y 
--			  y = f x in x

newtype Fix f = Fix (f (Fix f))
{-
newtype Mu f = Mu (forall a.(f a->a)->a)
data Nu f = forall a.Nu a (a->f a)
-}


--from blog http://blog.sumtypeofway.com/an-introduction-to-recursion-schemes/
--data Term f = In (f (Term f))

--out :: Term f -> f (Term f)  
--out (In t) = t
