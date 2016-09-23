
-- ripped fromwikipedia page for corecursion
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

data Tree a b = Leaf a  |  Branch b (Tree a b) (Tree a b)

--Holy shit
bftrav :: Tree a b -> [Tree a b]
bftrav tree = queue
  where
    queue = tree : gen 1 queue

    gen  0   p                 =         []           
    gen len (Leaf   _     : s) =         gen (len-1) s 
    gen len (Branch _ l r : s) = l : r : gen (len+1) s