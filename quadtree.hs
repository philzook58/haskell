data Quadtree a = Node (Quadtree a) (Quadtree a) (Quadtree a) (Quadtree a) | Leaf a


summary (Node a b c d) = (summary a)

