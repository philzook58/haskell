-- Martin Escardo, 29 Feb 2012, 01 March 2012.

-- Some more Haskell code for EWSCS'2012, Palmse, Estonia.
-- With lots of comments, particularly towards the end.

{-# LANGUAGE NPlusKPatterns #-}

data Bit = Zero | One

findBit    :: (Bit -> Bool) -> Bit
forsomeBit :: (Bit -> Bool) -> Bool

findBit p = if p Zero then Zero else One

forsomeBit p = p(findBit p)

-- Promise not to use negative integers:

type N = Integer         

-- We represent sequences as functions defined on the natural
-- numbers. 

type Sequence x = N -> x
type Cantor = Sequence Bit

hd :: Sequence x -> x
hd a = a 0

tl :: Sequence x -> Sequence x
tl a = \n -> a(n+1) 

(#) :: x -> Sequence x -> Sequence x
(x # a)    0  = x
(x # a) (n+1) = a n

-- We have 
--                 hd(x # a) = x 
--                 tl(x # a) = a 
--           (hd b) # (tl b) = b

-- Essentially Berger's 1990 algorithm:

findCantor    :: (Cantor -> Bool) -> Cantor
forsomeCantor :: (Cantor -> Bool) -> Bool

forsomeCantor p = p(findCantor p)

findCantor p = if forsomeCantor(\a -> p(Zero # a))
               then Zero # findCantor(\a -> p(Zero # a))
               else  One # findCantor(\a -> p( One # a))                    

-- The run time is exponential in the modulus of continuity of p.                     

-- A faster algorithm, exploiting call by need:

findCantor'    :: (Cantor -> Bool) -> Cantor
forsomeCantor' :: (Cantor -> Bool) -> Bool

forsomeCantor' p = p(findCantor' p)


findCantor' p = x # a 
  where x = findBit(\x -> forsomeCantor'(\a -> p(x # a)))
        a = findCantor'(\a -> p(x # a)) 
        
-- Now this is exponential in the number of elements of the
-- sequence a that p queries, plus a linear overhead depending on
-- the modulus of uniform continuity.

-- We now reduce this overhead from linear to logarithmic.  We view
-- sequences as trees, as in the heap-sort algorithm:

type Tree x = N -> x

root :: Tree x -> x
root a = a 0

left :: Tree x -> Tree x
left a = \i -> a(2 * i + 1)

right :: Tree x -> Tree x
right a = \i -> a(2 * i + 2)

tree :: x -> Tree x -> Tree x -> Tree x
tree x l r i   | i == 0    = x
               | odd i     = l((i-1) `div` 2)
               | otherwise = r((i-2) `div` 2)

findCantor''    :: (Cantor -> Bool) -> Cantor
forsomeCantor'' :: (Cantor -> Bool) -> Bool

forsomeCantor'' p = p(findCantor'' p)

findCantor'' p = tree x l r 
  where x = findBit(\x -> forsomeCantor''(\l -> forsomeCantor''(\r -> p(tree x l r))))
        l = findCantor''(\l -> forsomeCantor''(\r -> p(tree x l r)))
        r = findCantor''(\r -> p(tree x l r))

-- Now we consider a computational counter-part of the Tychonoff
-- Theorem from topology, generalizing the above algorithm with
-- linear overhead.
 
-- Given a sequence of selection functions ("find") for countably
-- many searchable subsets of a type X, we produce a single
-- selection function for the product of the searchable sets.
        
type Selection  x = (x -> Bool) -> x        
type Quantifier x = (x -> Bool) -> Bool       
  
tychFind    :: Sequence(Selection x) -> Selection (Sequence x)
tychForsome :: Sequence(Selection x) -> Quantifier(Sequence x)

tychForsome finders p = p(tychFind finders p)

tychFind finders p = x # a 
  where find     = hd finders 
        finders' = tl finders 
        x        = find(\x -> tychForsome finders'(\a -> p(x # a)))
        a        = tychFind finders'(\a -> p(x # a))
        
-- Now the arboreal version, which again reduces the overhead from
-- linear to logarithmic.

tychFind'    :: Tree(Selection x) -> Selection (Tree x)
tychForsome' :: Tree(Selection x) -> Quantifier(Tree x)

tychForsome' finders p = p(tychFind' finders p)

tychFind' finders p = tree x l r
  where find     = root finders 
        lfinders = left finders 
        rfinders = right finders 
        x = find(\x -> tychForsome' lfinders (\l -> tychForsome' rfinders(\r -> p(tree x l r))))
        l = tychFind' lfinders(\l -> tychForsome' rfinders(\r -> p(tree x l r)))
        r = tychFind' rfinders(\r -> p(tree x l r))

-- Example:
        
findCantor3    :: Selection Cantor
forsomeCantor3 :: Quantifier Cantor

findCantor3 = tychFind (\i -> findBit)
forsomeCantor3 p = p(findCantor3 p)

-- If we use infinite lazy lists, and use booleans for
-- representing bits, then we get the following self-contained
-- search algorithm for the Cantor space that doesn't use
-- if-then-else at all (linear overhead version):

findBool    :: Selection  Bool
forsomeBool :: Quantifier Bool

findBool p = p True
--         = if p True then True else False

forsomeBool p = p(findBool p)

type Cantor' = [Bool]

findCantor4    :: Selection  Cantor'
forsomeCantor4 :: Quantifier Cantor'

forsomeCantor4 p = p(findCantor4 p)

findCantor4 p = x : a 
  where x = findBool(\x -> forsomeCantor4(\a -> p(x : a)))
        a = findCantor4(\a -> p(x : a)) 

-- If we expand some of the definitions, we get the following
-- equivalent, short and self-contained program for search and
-- quantification over the cantor space (without mutual
-- recursion):

epsilon :: Selection  Cantor'
phi     :: Quantifier Cantor'

epsilon p =      x : epsilon(\a -> p(   x : a)) 
  where x = p(True : epsilon(\a -> p(True : a)))

phi p = p(epsilon p)

-- We not only check all cases in finite time, but we also do so
-- without using if-then-else! (More discussion about this below.)

-- Here phi is the existential quantifier. If we replace True by
-- False in the above definition, then the resulting phi is the
-- universal quantifier instead, and the selection function then
-- instead finds a counter-example if it exists, and otherwise
-- gives an example.

-- Now suppose that p : Cantor' -> Bool, has modulus of uniform
-- continuity 2, so that it can be written as 
--
--    p a = q(a !! 0)(a !! 1) 
--
-- where q : Bool -> Bool -> Bool is 
--
--    q x y = p([x , y] ++ anything).
--
-- Then what the above algorithm computes is 
--
-- x0 = q True True
-- x1 = q True x0
-- x2 = q x1 True
--
-- and x1,x2 is a solution to q x1 x2  =  True iff a solution exists.
--
-- I've written a program to compute this set of equations for any
-- modulus of continuity, and draw a dependency graph. What we get
-- for 4 is this (2^4-1 equations, as expected):
--
--  x0  = q(True,True,True,True)
--  x1  = q(True,True,True,x0)
--  x2  = q(True,True,x1,True)
--  x3  = q(True,True,x1,x2)
--  x4  = q(True,x3,True,True)
--  x5  = q(True,x3,True,x4)
--  x6  = q(True,x3,x5,True)
--  x7  = q(True,x3,x5,x6)
--  x8  = q(x7,True,True,True)
--  x9  = q(x7,True,True,x8)
--  x10 = q(x7,True,x9,True)
--  x11 = q(x7,True,x9,x10)
--  x12 = q(x7,x11,True,True)
--  x13 = q(x7,x11,True,x12)
--  x14 = q(x7,x11,x13,True)
--
--  x7,x11,x13,x14 is a solution to q x7 x11 x13 x14 = True iff a
--  solution exists. 
--
-- I've also computed the dependency graphs of such variable
-- sharing (notice that e.g. x7 is used 7 times), which are
-- available at http://www.cs.bham.ac.uk/~mhe/.talks/EWSCS2012/

-- To partly see how I have computed this, consider a type Bool'
-- of boolean expressions that can be formed from a formal truth
-- value True' and formal predicate P, together with a type
-- Cantor'' of sequences of boolean expressions:

type Cantor'' = [Bool']
data Bool' = True' | P Cantor'' deriving (Show)

-- Notice that we have P :: Cantor'' -> Bool'

type Selection'  x = (x -> Bool') -> x        
type Quantifier' x = (x -> Bool') -> Bool'       

epsilon' :: Selection'  Cantor''
phi'     :: Quantifier' Cantor''

epsilon' p =      x : epsilon'(\a -> p(    x : a)) 
  where x = p(True' : epsilon'(\a -> p(True' : a)))

phi' p = p(epsilon' p)

-- So this is the same code with "'" all over the place, including
-- the types.
--
-- The following computes the symbolic solution list for the
-- satisfiability problem of a predicate with modulus of uniform
-- continuity n:

symbolicSolution :: Int -> Bool'
symbolicSolution n = phi' p
 where p a = P(take n a)

-- For example,
--
-- symbolicSolution 3 = 
--
-- P [
--    [P [True',P [True',True',P [True',True',True']],P [True',P [True',True',P [True',True',True']],True']],
--      
--     P [P [True',P [True',True',P [True',True',True']],P [True',P [True',True',P [True',True',True']],True']],True',
--        P [P [True',P [True',True',P [True',True',True']],P [True',P [True',True',P [True',True',True']],True']],True',True']],
--
--     P [P [True',P [True',True',P [True',True',True']],P [True',P [True',True',P [True',True',True']],True']],
--        P [P [True',P [True',True',P [True',True',True']],P [True',P [True',True',P [True',True',True']],True']],True',
--        P [P [True',P [True',True',P [True',True',True']],P [True',P [True',True',
--        P [True',True',True']],True']],True',True']],True']]
--   ]
--       
-- If we remove the repeated sub-expressions (which occur because
-- the variable x is used twice in the definition of epsilon), we get that
--
-- symbolicSolution 3 = P[x3, x5, x6]
--   
-- where, removing "'" for readability,      
--
--   x0 = P(True,True,True)
--   x1 = P(True,True,x0)
--   x2 = P(True,x1,True)
--   x3 = P(True,x1,x2)
--   x4 = P(x3,True,True)
--   x5 = P(x3,True,x4)
--   x6 = P(x3,x5,True)
--
-- This gives a closed form expression for the satisfiability
-- problem, which uses P and True only. In general, this closed
-- form solution has (2^n)-1 equations. If we don't use equations
-- to represent the sharing of the variable x, we get an
-- expression of size 2^(2^n), computed as (symbolicSolution
-- n). This gives an indication of the run time of the algorithm.
--
-- Now consider a p that uses its arguments sparsely, for example
-- p a = a!!17 == a!!1000. This has modulus of uniform continuity
-- 1001, but queries only to positions of a, and hence the run
-- time should be 2^2 only, and not 2^1000-1. Without resolving
-- the sharing as above, we still get a very small expression
-- tree.

sparseExample :: Bool'
sparseExample = phi' p
 where p a = P [a!!17, a!!1000]

-- Then sparseExample = P [P [True',P [True',True']],P [P [True',P [True',True']],True']]
