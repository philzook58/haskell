-- A New approach:
-- Use hmatrix for mats and vecs
-- but build our own multidimensional structure on top

-- Never actually do dense Kron
-- fusion... Hmm This is probelmatic.
-- fuse could take GGGG to an irriducible 4 component matrix
-- This is already out of control. We can avoid N^8 memory storage with carefulness
-- or even N^5 by never computing intermediate fuse before trace
-- 

-- If we use laziness to avoid computing dense matrix products, we can wait until you want to ask for 
-- a value. before actually calculating.

-- would have to do dense kron?
-- there are block capabilities

-- Yeah. the problem is the matrix structure is not closed under these
-- multidimensional operations

-- rekron after summation - low rank approx. select a splitting (way to pair up indices)
-- to return to Kron Form. (Sum of Kron)
-- But this depends on which indices are closer later on...
-- Multidimensional H-Matrix, Ordering of kron associativity depends on closerness of points.
-- is this OPE?

-- ok. i could use hamtrix, with my own wrapped shape list objects

data Multi a = [Int] (Vector a)

-- Never do kron actually until a kron gets fused and traced.

-- with swap operators actually swapping
-- once all the things I want to sum are in the front or back
-- So then if we make Trace in my language ALWAYS tracing over last index. inseatd of parametized by 2 indecies
-- Then the swaps get built in

-- Wait! Swaps are Transposes! Instead of swapping indices, we can cluster indices into sets
-- It may be inefficient, but with a combo of transpose + reshape, we can probably swap anything

-- perhaps that is the right way to index stuff anyhow instead


-- make sure to do loop fusion of swaps
-- swap swap of same indices is idempotent
-- 



data Cluster = 


-- also build H-matrix (HAH!) strcture.


data HMatrix  = Tree (summarize, transfer, interpolate)
-- Supermatrix = Block matrix
data HMatrix = FullMatrix (Mat a) 
	| RkMatrix (Mat a) (Mat b) 
	| SuperMatrix HMatrix HMatrix HMatrix HMatrix -- Block Matrix

-- could also store error in the Rkmatrix (largest truncated svd)
-- and for FullMat largest eig
-- and carry along an error bound

-- Whoa. Wait. That's it I think

-- implement matrix multiplication
-- addition
mult (FullMatrix x) (FullMatrix y) = FullMatrix (x `dot` y)
mult (SuperMatrix a b c d) (Supermatrix e f g h)  = SuperMatrix (a `mult` e + b `mult` f ) -- yada yada, recursive block matrix multiplcation
-- Is this actually how this is done?
mult (RkMatrix a b) (RkMatrix c d) = RkMatrix a ((b `dot` c) `dot` d)

-- What do I need to implement for this to be automatic?
sum (FullMatrix x) (FullMatrix y) = FullMatrix (x + y)
sum (SuperMatrix a b c d) (Supermatrix e f g h) = Supermatrix (sum a e) (sum ) --yada yada 
-- recompress summation of RkMatrix
-- Is this actually how this is done?
-- look at compactSVDtol in hmatrix. Good function.
sum (RkMatrix a b) (RkMatrix c d) = where 
	a' = eig ((trans a `dot` a) + (trans c `dot` c)
	b' = eig ((b `dot` trans b) + (d `dot` trans d)

-- One wonders if figuring of H2 matrices is even worth it. Asymptotically H matrices are already pretty good
--
-- idle thoughts: H2 is 
-- where the basis Has to be good for entire row or column
-- Is this?
-- recursive RkMatrix?
-- Replacing Super with Hstack Vstack?
-- ColMatrix (Mat a) HMatrix
-- RowMatrix HMatrix (Mat b)
data H2Matrix = 

data HMatrix a = FullMatrix (Mat a) 
	| RkMatrix (Mat a) (Mat a) 
	| RkMatrix' HMatrix HMatrix -- Another suggestion. Kind of like Trace? Or MatProd
	| SuperMatrix HMatrix HMatrix HMatrix HMatrix -- Block Matrix
	| KronMat HMatrix HMatrix -- A suggestion.
	| Zero -- Might be useful, a polymorphic zero value. The sum of RkMatrices can be written as a supermatrix without compression
	| Id -- Identity matrix, Might also be useful sometimes.
-- if we want to tighten up, do we even want fullmatrix form? The intention is that the only fullMatrices
-- will be small and it so, having them in Rk matrix form doesn't hurt much
-- but for (nonasymtpotic) efficiency sakes we might as well.
-- They are easy anyway
-- for recursive RkMatrix, we need them as a base case.


-- Hmm Sum can already be taken care of by trace,
-- kron Aij Bkl ~ Cik dot Djl -- would be very useful as long as the dotted deimension is small
-- changing associativity structure. has cost
-- sum_m kron Cikm Dmjl is another way of stating it  


data HSS = 
	  Node {b12:: Mat, b21::Mat, lR::Mat, lW::Mat, rR::Mat, lW::Mat, left::HSS, right::HSS}
	| Leaf {d:: Mat, urows::Mat, vcols::Mat}

-- traverse up tree then down tree in order to matvec

-- this is a fold?
-- or this might be an applicative pattern
upcompute :: Tree (Mat,Mat) -> Vector -> Tree Vector
upcompute = hstack (lR * lv) (rR * rv) -- builds hierarchical summary vector

-- this is applicative
applyb :: Tree Mat -> Tree Vec -> Tree Vec 
applyb (Leaf d) (Leaf v) = Leaf (d * v)
applyb (Node (b12,b21)) (Leaf (v1,v2)) = Leaf (b12 * v2) (b21 * v1) -- apply b at every level and d at bottom

downcompute (Leaf vrows) (Leaf v) = vrows 

-- ok. A thought. Generalize Rk to 

-- (a,a)->b is function to give matrix element
-- or maybe you might want
-- a -> (Vector a b)
-- Use ID or SVD. For our purposes, might be fine to use SVD (in hmatrix package)
-- The indices should be in a Tree like structure?

-- A pipeline going from dense to HMatrix
-- first function might just blockify fully dense according to some tree structure
-- a second function would low rankify according to a tree structure.
-- Should we

hmatrixify :: ((a,a) -> b) -> HMatrix

-- traverse the tree and mutiply out Rk matrices and combine
densify ::


admissible in out = 

-- or foereign import HLib? Cripes that extends our scope


-- Use Anharmonic SHO as test case. Still has time dependence. (Or 1D laplace. which has A trasnfer matrix formulation? (Uh yeah, propagating the shcrooedinger eq.))


