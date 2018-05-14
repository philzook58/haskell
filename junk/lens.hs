{-# LANGUAGE TemplateHaskell #-}
data Foo a = Foo { _bar :: Int, _baz :: Int, _quux :: a }
makeLenses ''Foo

{- generates
bar, baz :: Simple Lens (Foo a) Int
quux :: Lens (Foo a) (Foo b) a b

-}

fuzzy = set mapped 43 [1,2,34,22]
-- [43,43,43,43]
-- mapped gets setter on all elements of list

-- _2 gets second element of tuple


--


--(.).(.) :: (b -> c) -> (a -> a1 -> b) -> a -> a1 -> c
-- how. This is the triple boob. Triple nip?
-- 
-- (.) :: (b-> c) -> (a -> b) -> a -> c
-- okay so applying this to itself. what is getting bound as b c a
-- f . g = \x -> f g x
-- (.) . (.) = \x -> (.) (.) x
-- = (.) . x in infix form
-- = apply x then apply (.)
-- so x applied must return a function a->b since that is what (.) takes.
-- then (.) partially applied to a function, is of type (b->c)-> a -> b
--  

-- (.)(.) :: (a -> b -> c) -> a -> (a1 -> b) -> a1 -> c
-- (.)(.)(.)
--(.)(.)(.)(.)(.) :: (b -> c) -> (a1 -> b) -> (a -> a1) -> a -> c

-- (first.second) (+3) ((1,4),2)
-- ((1,7),2)
-- result = (.)
-- function compoisition is a result operator
-- result id doesn't change anything
-- good for setters. Getters?
-- (.).(.) should be written result.result
--  (result.result)  (+3)  (\x -> \y -> 4)
-- returns (\x -> \y -> 7)
-- let result = (.)
-- ((result.result)  (+3)  (\x -> \y -> 4)) () ()
-- does return 7
-- fmap can also be read as elements
-- THis let's you edit all the elements
-- argument = flip (.)

-- traverse . traverse
-- in this language its a bit different.
-- fmap will apply the setter to all arguments.
-- traverse will apply but then pull the applicative structure out 

-- result.result.result
