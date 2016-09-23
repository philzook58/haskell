



Data LongFunc a = Done a | NotDone (LongFunc a) -> (LongFunc a)

-- This seems primed for the discussion of algebras I've been reading recently
-- Fix or In or Mu or whatever

-- to elegantly transform these functions.

-- Should objects like these be the indices to anyon space?


-- Also Kind of Tree Like
-- probably some kind of abstraction there.
Data Tree a = Leaf a | Node a (Tree a) (Tree a)

