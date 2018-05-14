
--traverse _ [] = [] 
--traverse f x:xs = (:) <$> f x <*> traverse f xs

-- That (:) is a binary function. fmapped over the return of f x, which must be an applicative.
-- the applicative stuff referred to by the map and the <*> is the return of f. Not the actual object we're traversing
 -- leads to expressions like the followinf

yolo = [(:) 0] <*> ([(:) 2 , (:) 3] <*> [[]])

data Tree a  = Empty  | Leaf a | Node (Tree a) a (Tree a) deriving Show

instance Functor Tree where
  fmap _ Empty = Empty
  fmap f (Leaf a) = Leaf (f a)
  fmap f (Node l x r) = Node (fmap f l) (f x) (fmap f r)

instance Foldable Tree where
    foldMap _ Empty = mempty
    foldMap f (Leaf a) = f a
    foldMap f (Node l x r) = (foldMap f l) `mappend` (f x) `mappend` (foldMap f r)

instance Traversable Tree where
   traverse _ Empty = pure Empty
   traverse f (Leaf a) = Leaf <$> f a
   traverse f (Node l x r) = Node <$> traverse f l <*> f x <*> traverse f r

 -- note here that we could have... ? could we have changed the order the trees were made?
 -- yes... We could have flipped Node argument ordering.
 -- flip Node <$> f x <$> traverse f l <*> traverse r l  
-- would change the enumeration ordering. noncommutative applicatives only
-- in Just it wouldn't have mattered

-- should print out a list of all trees with all possible placements of 0 and 1
--traverse (\x -> [0,1]) (Node (Leaf 12) 14 (Leaf 13))
--[Node (Leaf 0) 0 (Leaf 0),Node (Leaf 0) 0 (Leaf 1),Node (Leaf 0) 1 (Leaf 0),Node (Leaf 0) 1 (Leaf 1),Node (Leaf 1) 0 (Leaf 0),Node (Leaf 1) 0 (Leaf 1),Node (Leaf 1) 1 (Leaf 0),Node (Leaf 1) 1 (Leaf 1)]
-- length is 8. Which is 2^3


-- the traversible ordering that you choose...

--let f x = Just x 
--traverse f [1,2,3]
--Just [1,2,3]
-- first mapping makes a Just ((:) x)
--which then gets pushed on along inside <*>  maintaining Just the whole time
-- (Just x:x2:)
-- yada yada

--let a = ZipList [1,2,3]
--let b = ZipList [a,a,a]
--seqeunceA b 
-- does matrix transpose


-- traverse ~ mapM

-- sequenceA ~ sequence ~ flip constructors? 



-- applicative. 
-- partially applied (two argument function mapped)

-- map (+) [1,2,3] -- returns a list of functions

--(+) <$> [1,2,3] <*> [40,50,60]

-- applicative can be used instead of monad in some situations.
-- monad givesyou extra power of being able to use results
 -- if you don't use your results anywhere except insdie of a return, that is applicative and doesn't require monadism
 -- Lets assume the list monad
{-
fred x = do a <- (3*) x 
            b <- (1+) x
            return 

-}
--Can apply these functions using  <*>

-- ZipList apply list of functions to lists of 