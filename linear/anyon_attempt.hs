
data ParticleType = Ident | Tau
data Mu = One
data Edge = Edge ParticleType Node
data Node  = Split Mu Edge Edge | Nil | NextLeaf Node
-- linked list at bottom.
-- could also keep previous node. doubly linked list.
data Root = Root Edge Node -- root edge and leftmost head leaf.

-- possibly useful. Make each node contain a linear sum of possible subtrees.

--use zipper?

root = Edge Ident (Nil)

-- makes leafnum have same parent
parentify leafnum tree =  -- find least common ancestor then Fmove up both sides until they have same parent
-- or if right leaf go up until left leaf then follow all left leaves.
--could also do f moves while going up.
-- that is a much better algorithm 
lca n = construct parent lists. find first common element in lists. -- or build one list. iterate up thorugh other parents and check
parentlist node = if not root then parent node : parentlist parent node
inlist x [] = False	
inlist x a:as = if x == a then True else inlist x:as
posinlist n x a:as = if a == x then n else  posinlist (n+1) x as
-- this is bad code. Not haskelly.


lrotate = 
rrotate = 

annhilate = parentify -- and then remove those leaves if equal particles and make entire diagram 0 if unequal particles
split a b = parentify then 
join = split a b then annihilate -- >- 

rbraid leafnum = parentify then apply R matrix
lbraid leafnum = parentify then apply R^-1 matrix

-- store linear values in hash table keyed by diagrams or 
-- keys need to be immutable in python