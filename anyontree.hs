
--tree zipper from leanr you
data Crumb a = LeftCrumb a (Tree a) | RightCrumb a (Tree a) deriving (Show)  
data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)  

type Breadcrumbs a = [Crumb a]  

goLeft :: (Tree a, Breadcrumbs a) -> (Tree a, Breadcrumbs a)  
goLeft (Node x l r, bs) = (l, LeftCrumb x r:bs)  

goRight :: (Tree a, Breadcrumbs a) -> (Tree a, Breadcrumbs a)  
goRight (Node x l r, bs) = (r, RightCrumb x l:bs)  

type Zipper a = (Tree a, Breadcrumbs a)  


goUp :: (Tree a, Breadcrumbs a) -> (Tree a, Breadcrumbs a)  
goUp (t, LeftCrumb x r:bs) = (Node x t r, bs)  
goUp (t, RightCrumb x l:bs) = (Node x l t, bs) 

-- did i do this right. I was confused about breadcrumbs
leafRup (t, RightCrumb x l:bs) = leafRup (Node x l t, bs) 
leafRup (t, LeftCrumb x r:bs) = leafRdown (r, RightCrumb x t:bs) 

leafRdown (Empty, crumbs) = (Empty, crumbs)
leafRdown (Node x l r, crumbs) = leafRdown (l, (LeftCrumb x r):crumbs)

lrotate (t, (RightCrumb x l):(RightCrumb x' l'):bs ) = lrotate (t, RightCrumb x (Node x l' l):bs)  -- t should be empty. l will get attached to parent
lrotate (t, (RightCrumb x l):(LeftCrumb x' r'):bs ) = downrotate (r' , (RightCrumb x' t):(RightCrumb x l):bs)

downrotate (Empty, bs) = (Empty, bs)
downrotate (Node x (Node x' l' r') r, bs) = downrotate ( Node x' l' (Node x r' r), bs)
downrotate (Node x Empty r, bs) = downrotate ( Node x' l' (Node x r' r), bs)

{-
parentifyR :: Zipper a -> Zipper a
--We cmamd right
parentifyR (Empty, LeftCrumb x r:bs) =
parentifyR (t, LeftCrumb x r:bs) = 
parentifyR (t, LeftCrumb x r:bs) = 
parentifyR (t, RightCrumb x l:bs) = 
	-}