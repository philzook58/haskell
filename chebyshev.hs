import Data.Tree

--angles = fmap  (\x ->(x-1) * pi) (angles' 0 1)
angles =  angles' (pi/2) (pi/2)

angles' = subdivisiontree 
subdivisiontree val denom = (Node val [(angles' (val + denom') denom'), (angles' (val - denom') denom')]) where denom' = denom / 2

--take 3 $ levels angles 

chebpts = fmap cos angles 

takelevel 0 (Node val _) = Node val []
takelevel level (Node val xs) = (Node val (fmap (takelevel (level - 1)) xs))


--inorder (Node val (x:xs)) = (inorder x) ++ [val] ++ (inorder x)

basicinterval = subdivisiontree 0 1



data FilledTree a = FNode a a

fft : FilledTree a 


-- chebyshev should be fft + coordinate transformation
-- that is the essence of the form. Unifies chebyshev with rational chebyshev 
-- and non unit interval chebysehv

evenextension :: [a] -> [a]
evenextension x = x ++ (reverse x)

data MappedFourier a = MappedFourier {coeffs :: [a], mapcoeffs :: [a]}

-- or maybe mapfunc could be an automatically differentiable function?
-- can you invert a fourier function? hmm. if not, tough to actually evaluated function at new points
-- need an diffeomorphism - an AD data type with packaged inverse.
-- the derivatives of the inverse can be calculated from the forward derivatives

-- fourier series composition?

