

data Node = Node Int [Edge] deriving Show
data Edge = Edge Node Node deriving Show



node1 = Node 1 [edge12]
edge12 = Edge node1 node2
edge23 = Edge node2 node3
node2 = Node 2 [edge12, edge23]
node3 = Node 3 [edge23, edge12]


data Vertex = Vertex Int [Vertex] deriving Show

vert1 = Vertex 1 [vert2, vert3]
vert2 = Vertex 2 [vert1, vert3]
vert3 = Vertex 3 [vert1, vert2]

godeepn 0 (Vertex a xs) =  Vertex a []
godeepn n (Vertex a xs) = Vertex a (map (godeepn (n-1)) xs)

step 0 (Vertex a _) = [a]
step n (Vertex _ vs) = concatMap (step (n-1)) vs

-- vertex could also be a DAG if you only list nodes coming out of it. or you could have two lists. One listing incoming nodes
--and one listing outgoing edges

-- Directed Acyclic Graph

data DAGraph = Split {left :: DAGraph, right :: DAGraph} | Fuse DAGraph | Nil | Straight DAGraph deriving Show


-- a split into b and c which then fuses into d and ends
d = Straight Nil
b = Fuse d
c = Fuse d
a = Split {left = b, right = c}
-- I don't enforce fusion to be 2 -> 1 with types
-- Split is tree like. Fuse needs to be coTree like whatever that means


-- Zipping down the tree sort of makes a co tree going back up the tree.
-- zipping down a list is simpler. YOu meake a list going back the other way while you're doing it

data CoDAGraph = Fuse {left :: DAGraph, right :: DAGraph} | Split CoDAGraph | Nil |Straight CoDAGraph

--or perhaps traversing down the graph needs to  be nondeterministic

data ActivePaths = [DAGraph]

-- This is a fused togehter data type of the upward tree zipper crumbs and the tree itself
data CoCrumbGraph =  Fuse CoCrumbGraph CoCrumbGraph | RightSplit {rightdown:: CrumbGraph, up CoCrumbGraph} | LeftSplit {leftdown:: CrumbGraph, up CoCrumbGraph} | Root -- yada yada yada
-- this allows multiple roots. But that is ok. Just wanted to let you know.
data CrumbGraph =  Split CrumbGraph CrumbGraph | RightFuse {leftup:: CoCrumbGraph, down CrumbGraph} | LeftFuse {rightup:: CoCrumbGraph, down CrumbGraph}  | Bottom
-- They are exactly the same data type. but importantly DISTINCT. Dual.
-- quite beautiful. and I believe enforces via type that fusing and splitting are trivalent vertices

-- Is this almost a full trvialent graph sturcture?
data TrivalentGraph = TriVert TrivalentGraph TrivalentGraph TrivalentGraph | Nil

-- and we can implment the zipper pattern on them?
-- they kind of have a zipper built in
-- but suppose we make a pure tree using CrumbGraph. It won't ever have a coGraph pointing up 

-- dagger Crumbgraph = coCrumbGraph -- would flip all arrows

-- leftdown (CrumbGraph, coCrumbGraph) = Maybe 
-- leftup
-- 

-- if you want to store data on the edges (nodes?) parametize type with data CrumbGraph a
-- something like CurmbGraph node edge = Split node edge CrumbGraph edge CrumGraph |
-- in a tree every node has exatcly one edge coming into it. so edges = nodes + roots
-- I think we do not have such a simple relation here

-- consider perhaps using Split as a root? or Specify  | Cap CrumbGraph Crumbgraph as sort of a capping edge


-- Braiding transformations. recurse until there are no braids. Since i cannot encode braid elegantly?
-- perhaps i can?

data Braid = OverFromRight {continue:: CrumbGraph,opup::CoCrumbGraph, opdown::Crumbgraph } | OverFromLeft | UnderFromRight | UnderFromLeft
data CoBraid = 
	-- Not that elegant. Yes even though braid only come in two varieties, I've parametized by where you came from so needs 4

-- braid a b graph






