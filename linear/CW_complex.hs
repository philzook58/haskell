-- makes a circle

type V1 = ()
type V2 = ()

type E1 = Float
type E2 = Float

v1e1 :: V1 -> E1
v1e1 _ = 0

v1e2 :: V1 -> E2
v1e2 _ = 0

v2e1 :: V1 -> E1
v2e1 _ = 1

v2e2 :: V1 -> E2
v2e2 _ = 1


-- somehow I should encode that E1 and E2 are the same. They both implement the typeclass edge?
{-
class Simplex where
	boundary

class Edge where
	boundary :: (Edge e, Vertex v) => e -> (v,v)

class Vertex where
	boundary :: (Vertex v) => v -> [()]

	-}
-- This is a subset diagram. 
-- reversing the arrows

-- I should use type level numerals.


-- An alternative apporach
-- I feel this is not generic enough
-- maybe have CircleEdge implement boundary
data CircleVertex = V1 | V2 
data CircleEdge = E1 | E2 


boundary :: CircleEdge -> (CircleVertex, CircleVertex)
boundary E1 = (V1,V2)
boundary E2 = (V1,V2)



