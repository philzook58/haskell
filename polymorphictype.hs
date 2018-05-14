{-# LANGUAGE RankNTypes, GADTs, FlexibleInstances, 
DataKinds, TypeFamilies,MultiParamTypeClasses,
UndecidableInstances,
FlexibleContexts,
OverlappingInstances,
ScopedTypeVariables,
FunctionalDependencies,
AllowAmbiguousTypes,
GADTs,
TypeOperators
 #-}

import Data.Proxy
-- You can basically take any typeclass into a "Free" Version of itself
-- that just stores usage of typeclass functions in a AST
data FreeCat k a b = Comp (FreeCat k a b) (FreeCat k a b) | Id | Morph (k a b)
data MonoidalCat k a b = Catty (FreeCat k a b) 
					| Par (MonoidalCat k a b) (MonoidalCat k a b) 
 					| Dup | Fst | Snd

data FunWrap = FunWrap (forall a. a -> a)
data Forall f = Forall (forall a. a -> f a)

class UniqueFun a where
	rep :: a -- forall k a b. FreeCat k a b --?

instance UniqueFun FunWrap where
	rep = FunWrap id

instance (UniqueFun a, UniqueFun b) => UniqueFun (a -> b) where
	rep = const rep

instance (UniqueFun a, UniqueFun b) => UniqueFun (a,b) where
	rep = (rep, rep)

-- Given a N to start labelling with returns tuple and new N
class TagTuple a where
	ordered :: Int -> (a, Int) -- forall k a b. FreeCat k a b --?

instance (TagTuple a, TagTuple b) => TagTuple (a,b) where
	ordered n = ((tup1,tup2), n'') where
					(tup1, n') = ordered n
					(tup2, n'') = ordered n'

instance TagTuple Tag where
	ordered n = (Tag n, n+1)



-- Ok. Maybe we can't with type protection do it
-- But it seems like maybe I can destruct the structure of lambdas
-- But how can I know

-- Wait.. Can I?
-- What about generics maybe?
-- or typeable?
-- Those things let me see the type. But of lambdas?


-- Or should I be able to just USE the function and inspect the result
-- This will probably work since it has to be polymorhpic if it isn't speciallfy using my tags

data Tag = Tag Int deriving Show


class TryFunction a where
	rep'' :: TagTuple b => a -> b

-- (intuple, outtuple)

-- Base Case. This one is an id.
--instance TryFunction a => TryFunction (Tag -> Tag) where
--	rep'' _ =  
{-
instance TryFunction a => TryFunction (Tag -> a) where
	rep'' f = rep'' (f (Tag 0))

-- No this is a base case. This function should select one of the given tags
instance TagTuple a => TryFunction (a -> Tag) where
	rep'' f = f tag where
					(tag , _) =ordered 0

-- autocurry
instance (TagTuple a, TryFunction b) => TryFunction (a -> b) where
	rep'' f = rep'' (curry f)

instance (TagTuple a, TagTuple b, TagTuple c) => TryFunction (a -> (b,c)) where
	rep'' f = Tag 0 where
				l = rep'' (fst . f)
				r = rep'' (snd . f)

-}
-- There are a couple different kinds of functions that can be 
-- reoncistiuted, representable functions can by by tabulate.
-- Functions from and bounded enum can be tabulated.
-- Linear functions by sampling on a basis
-- 

--type family  (a,b) = (TupTag a, TupTag b)
{-
class TestIdea a where
	works :: TupleTag b => a -> b

instance TestIdea (Tag -> Tag) where
	works f = f (Tag 0)

thisworks :: String
thisworks = works id
-}
class IsTag a

instance IsTag Tag

class IsTup a

instance IsTup (a,b)
{-
class GetVal a where
	val :: a

instance (GetVal a, GetVal b) => GetVal (a,b) where
	val = (val, val)

instance GetVal Tag where
	val = Tag 0
-}

type family (Tagify a) :: * where
	Tagify (a,b) = (Tagify a, Tagify b)
	Tagify a = Tag 

type family (F a) :: Bool where
  F (a,b)  = 'True
  F a     = 'False

type family (G a) :: Bool where
  G (a -> b)  = 'True
  G a         = 'False




{-
class GetVal a where
  val :: Int -> a -> (Tagify a, Int)

instance (Tagify a ~ Tag) => GetVal a where
	val n _ = (Tag n, n+1)

instance (GetVal a, GetVal b) => GetVal (a,b) where
	val n (x,y) = ((v1, v2), n'') where
						(v1 , n') = val n x 
						(v2 , n'') = val n' y

-}

forceTag :: Tag -> Tag
forceTag = id
-- unsafeCoerce :: a -> Tag
{-
class GetVal a where
  val :: Int -> Proxy a -> (Tagify a, Int)

instance (Tagify a ~ Tag) => GetVal a where
	val n _ = (Tag n, n+1)

instance (GetVal a, GetVal b) => GetVal (a,b) where
	val n _ = ((v1, v2), n'') where
						(v1 , n') = val n (Proxy :: Proxy a) 
						(v2 , n'') = val n' (Proxy :: Proxy b)
-}
-- We know we're supposed to be able to take in tags

-- Do not export the Tag constructor.
-- I guess you could still pattern match into it.
-- But 
type T = Tag
class GetVal a where
  val :: Int -> Proxy a -> (a, Int)

instance GetVal Tag where
	val n _ = (Tag n, n+1)

instance (GetVal a, GetVal b) => GetVal (a,b) where
	val n _ = ((v1, v2), n'') where
						(v1 , n') = val n (Proxy :: Proxy a) 
						(v2 , n'') = val n' (Proxy :: Proxy b)

data TagTree = Node TagTree TagTree | Leaf Tag -- | Apply (k a b) TagTree

class Treeify a where
	treeify :: a -> TagTree

instance Treeify Tag where
	treeify x = Leaf x

instance (Treeify a, Treeify b) => Treeify (a,b) where
	treeify (a,b) = Node (treeify a) (treeify b)


fst1 :: (Tag, b) -> Tag
fst1 = fst

snd1 :: (a, Tag) -> Tag
snd1 = snd
{-
-- Hmm I'm not sure how to monomorhpize this.
fst' :: (TagTup a) => (a, b) -> a
fst' = fst
-}
{-
class AutoCurry a b | a -> b where
	autocurry :: a -> b 

instance AutoCurry (a->b->Tag) ((a,b)->Tag) where
	autocurry f = uncurry f

instance AutoCurry c (a->c') => AutoCurry (b->c) ((b,a) -> c') where
	autocurry f = uncurry (\b -> autocurry (f b))
-}


data FunData = FunData {inval :: TagTree, outval :: TagTree}

class TestIdea a b where
	works :: (a -> b) -> (a, b)

instance (GetVal a) => TestIdea a b where
	works f = (inval,  f inval) where inval = fst $ val 0 (Proxy :: Proxy a) -- fst $ val 0 (Proxy :: Proxy b)

fuckmyshitup :: (GetVal a, Treeify a, Treeify b) => (a -> b) -> FunData
fuckmyshitup f = let (a, b) = works f in FunData (treeify a) (treeify b)

-- Then we can compile to categories. Replacing the entire structure with dup and par and
-- fst, snd, etc.

-- Make an infix operator $'
data Apply k a b c = Apply (FreeCat k a b) c
type ($$) = Apply
-- No, don't need getval.
-- We'll just need it for treeify?
{-instance GetVal c => GetVal (Apply k a b c) where
	val n _ = where x, n' = val n Proxy c
-}
-- Another Option

data A
data B
data C

-- This is basically a lambda calculus
-- I could probably finitely enumerate through all the typeclasses for all the variables
 
example = Proxy :: Proxy ((A,B) -> B)

-- Hmm this would allow you to force duplicate input types though.

{-
class (Tagify a ~ a, Tagify b ~ b) => TestIdea a b where
	works :: (a -> b) -> (a, b)

instance (GetVal a) => TestIdea a b where
	works f = (inval,  f inval) where inval = fst $ val 0 (Proxy :: Proxy a) -- fst $ val 0 (Proxy :: Proxy b)
-}
--thisworks :: String
--thisworks = works id

-- fst . (val 0)

{-
instance (F a ~ flag, GetVal' flag a) => GetVal a where
  val = val' (Proxy :: Proxy flag)

class GetVal' (flag :: Bool) a where
  val' :: Proxy flag -> a -> Tagify a

instance (GetVal a, GetVal b) => GetVal' 'True (a,b) where
  val' _ (x,y) = (val x, val y)

instance GetVal' 'False a where
  val' _ x = Tag 0
-}

