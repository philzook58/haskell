
--data Slater basis scalar = 


--kronvec / fockvec
-- vector space with state labels a and field b
data FockVec a b = Add [KronVec] | Tens KronVec KronVec | Single Vector a b | Empty b

type QFockVec = FockVec a (Complex Double)

-- This is so a fold.? But like a bifold

inner (Add a) (Add b) = sum (zipWith inner a b)
inner (Add a) b = sum (fmap (\x -> inner x b) a)
inner a (Add b) = sum (fmap (\x -> inner a x) b)
-- Might easily get mixed Add and Tens
inner (Tens a b) (Tens c d) = (inner a c) * (inner b d) 
inner (Empty a) (Empty b) = a * b
inner (Single a) (Single b) = dot a b
inner _ _ = 0


-- for anti-inner, need to build those dot product matrix, the Gramian
-- then recompose

-- Do siplest thing first.
-- if we're just going to build these matrices, why bother?

data SimpleAFockVec = Stacked [Matrix a b] | Empty b 

-- restrict to Tens (Vector a b) KronVec | Empty

data AFockVec a b = Add [AFockVec] | Tens (Vector a b) AFockVec | Empty b

ainner (Add a) (Add b) = sum (zipWith (inner mat) a b)
--ainner (Tens a b) (Tens c d) = (inner a c) * (inner b d) - (inner a d) * (inner b c) 
