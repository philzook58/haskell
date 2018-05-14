

data Matrix a = [[a]]

instance Functor Matrix where
	fmap = map.map 

elementwiseprod opmat mat = fmap  

instance Applicative Matrix where
	pure = 
	<*> = elementwiseprod -- ? matrixprod , elementwiseplus
-- choice of kronecker product elemnetwise
-- or of ziplike elementwise product
-- or maybe matrixproduct? row column type summation but with different operators than necessarily + *
-- maybe + could be a monoid. It's sort of a fold, concatenation thing.
instance Traversable Matrix where
	sequenceA

mymatrix = [ [3,4] , [5,6]  ]
--opmatrix = fmap (fmap (*)) mymatrix 
--opmatrix' = fmap.fmap (*) mymatrix
opmatrix = fmap (*) mymatrix


funcmat i j = valueat i j
functranspose = swap
funcdagger f =  conjugate.(functranspose f)


let a = ZipList [1,2,3]
let b = ZipList [a,a,a]
seqeunceA b


-- I should consider more possible exmaples from scientific programming
-- they don't infect the haskell literature much
-- also computer graphics or geometry


-- zoomin zoomout lenses for multiscale?

-- <*> :: f (a -> b) -> f a -> f b 
-- is linearity an pplicative instance and not monadic?

--list of lists is obvious but bad. Does not enforce all lists of same size.
-- functorial indexing? fmap can be use for classical computation on indices.
-- isomorphism S and S^-1 between bases. Swap will work for transpose
-- maybe thinking about this helps with anyons in different bases.
-- if you tag basis elements and make them instances of some standardized class, you could implement operators generically in basis
-- via conversion. Howver possibly inefficient, and also standard basis is peculiar.
-- there will be some kind of graph of connecting isomorphisms
-- what about overcomplete basis




