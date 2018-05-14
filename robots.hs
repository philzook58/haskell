

data FrameDiff = Mat4

-- Or Use 4D homogenous represtnation

data FrameDiffs = [FrameDiff]


diffChain :: FrameChain -> FrameDiffs 
sumDiffs :: FrameDiffs -> FrameChain
-- implemented with a scan 


-- May not just want a chain. Could also want a Tree.
-- Perhaps any Traversable sturcture?


data Frame = Frame Rotation Position

type FrameChain = [Frame]

-- Uniform representation of parametized chain.
-- can cut up multparameter links into single parameter links


type ParameterChain [Double -> FrameDiff] 

-- would be nice if it were more generic than Doubles.
-- like what if something had only two configurations?


constBlock x = [const x]

-- except they should have 
xyzBlock = [ \x->(x,0,0) ,\y ->(0,y,0), \z -> (0,0,z)]

-- packs it in with identity rotation
transToFrame :: (Double -> Translation) -> (Double -> Frame)
xyzBlock' = fmap transToFrame xyzBlock

--eulerBlock [\a ->   , \b -> , \g->  ]


-- First derivative or higher important?
