{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

import Data.Functor.Const
-- c is the product of a and b
-- it is a projection if 
-- We can only have c be a product of two possible types? An unfortunate restriction.
class ProdC a b c | c -> a b where
	proj1 :: c -> a
	proj2 :: c -> b
	profactor :: (d -> a) -> (d -> b) -> (d -> c)

instance ProdC a b (a,b) where
	proj1 = fst
	proj2 = snd
	profactor f g d = (f d, g d) 

data TwistProd a b = TwistProd {getpair :: (b,a)}

instance ProdC a b (TwistProd a b) where
	proj1 = snd . getpair
	proj2 = fst . getpair
	profactor f g d = TwistProd (g d, f d) 

-- Products are also limits of 

-- a cone takes two functors
-- one is constant functor and the other is whatever.
-- Hask is pattern category. This is problematic.
{-
-- it would be nice to declare

-- restricted Hask is category
-- objects are Data Set. The Tags become the objects.
-- morpshisms is also a data Set (a->b) | (c -> d) 
-- morphisms are generated?

-- supply basic categories
-- Cat Int
-- Cat Pair
-- 

-- + ability to work in Cat itself.
-- 

-- model with a monad?
-- And endofunctor that takes Hask to a subset of itself.

-- typeclass is a subset of haskell types

Then 2-Cat with 2 morphisms 


id is always avaiable
can always compose

recursion using fix.

Cat C where
	objects :: Int | | |
	morphisms :: 
	
-- Functor F
	fmap
-- Nat G

-}


{-
-- a cone pointing to c using functor f
class Cone f a where
	coneside :: Functor f => (Const a b) -> (f b)


instance Cone [] a where
	coneside c = [getConst c] 
-- naturality condition 
-- (fmap h) . proj . Const =   proj . Const . h 

{-
class Cone => Limit 
-}

class Cat2 where
	type Obj1 :: *
	type Obj2 :: *
	morph1 :: Obj1 -> Obj2
	morph2 :: Obj2 -> Obj1

instance Cat2 where
	type Obj1 = ()
	type Obj2 = ()
	morph1 = id
	morph2 = id

-}



-- required laws
-- Commuting
--  p . (h! f g) = f
--  q . (h! f g) = g

--  And (h! f g) unique?
{-
class CoProdC a b c | a b -> c where
	inc1 :: a -> c
	inc2 :: b -> c
	coprofactor :: (a -> d) -> (b -> d) -> (c -> d)

 -}


-- using type families?
{-
class ProdC a b  where
	type c 
	proj1 :: c -> a
	proj2 :: c -> b
	profactor :: (d -> a) -> (d -> b) -> (d -> c)
-}