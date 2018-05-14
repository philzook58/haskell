data VecPoly valtype index =   Term (Vector index b) (VecPoly valtype (index :. Int))


-- COuld use free monad pattern. What would that gain me?
data TensExpr = J 
	| G 
	| Id
	| VecLit  [Double] -- Just placeholder. Obviously should be
	| MatLit [[Double]]
	| Scalar Double
	| Zero -- 2 dim zero matrix
	| Kron TensExpr TensExpr 
	| Trace Int TensExpr 
	| Sum TensExpr TensExpr
	| Fuse Int Int TensExpr  -- Call Diag? to match numpy -- Only trace last index? + Swap gives same functionality
--	| SMult Double TensExpr
	| Swap Int Int TensExpr
	| DirectSum Int TensExpr TensExpr
	| SymKron TensExpr TensExpr  -- In regards to building useful constructs for qft
	| ASymKron TensExpr TensExpr 
	| Sym Int Int TensExpr
-- There maybe be an advantage to containg
-- Stack and Block Forms.
-- Maybe just stack. DirectSum Int TensExpr TensExpr which
-- can be conbined to do Block.
-- SparseLit Maybe be Nice
-- Or BlockSparse? Nah. BlockSparse is the Kron of sparseLit and other stuff... uh... right?
-- No. Maybe that is useful.

-- instead of labelling indices with numbers, could any tensexpr have just one index "exposed" in a sense
-- like currying of multi argument functions



-- some convenience functions
matvec a b = (Trace 1 (Fuse 1 2 (Kron a b)))
plus a b = (Sum a b)
dotvec a b = (Trace 0 (Fuse 0 1 (Kron a b)))
vecmatvec a b c = (dotvec a (matvec b c))
trace axis a = (Trace axis a) 
matmat a b = matvec
fusetrace a axis1 axis2 = Trace (min axis1 axis2) (Fuse axis1 axis2 a)
matvec' a b = fusetrace (Kron a b) 1 2
fusekron a b axisa axisb = Fuse axisa ((indexnum a) + axisb) (Kron a b)
stack axis a b = DirectSum axis a b 
hstack a b = DirectSum 0 a b
blockmat a b c d = DirectSum 0 (DirectSum 1 a b) (DirectSum 1 c d)



shapeconcat a Z = a
shapeconcat a (x :.  

-- Should grad put the new index at front or back by default?
-- Let's say at nack for now. So that it leaves most indexes unchanged
-- This is all just symbolic differntation
-- which feels like a defeat.
grad J = Id
grad G = Zero
grad c@(Kron a b) = Sum (Swap ((indexnum a) + 1) ((indexnum c) + 1) (Kron (grad a) b)) (Kron a (grad b)) -- Close but Not right The new index is showing up in a random position
grad (Fuse a b c) = Fuse a b (grad c)
grad (Trace index c) = Trace index (grad c)

--effective action is a legendre trasformation. requires inverting power series.
-- will this require... Matrix inverses?


-- Wait, I can't do this.
--ungrad J = Kron (Scalar 0.5) (Fuse 0 1 (Kron J J))
--ungrad G = 

indexnum (Kron a b) = (indexnum a) + (indexnum b)
indexnum (Fuse _ _ a) = (indexnum a) - 1
indexnum (Trace _ a) = (indexnum a) - 1
indexnum (Sum a b) = max (indexnum a) (indexnum b) -- This is using overloaded summation -- Maybe this is a bad idea. since it won't mesh well
indexnum J = 1
indexnum G = 2
indexnum Id = 2
indexnum Zero = 2


getshape (Kron a b) =  (getshape a) ++ (getshape b)
getshape 


-- Find which branch the trace goes in and push it inside
simplify (Trace index (Kron a b)) = Kron (Trace index a) b
-- Also 
-- could resuviely continue into a and b
simplify (Fuse i j (Kron a b)) = Kron (Fuse i j a) b
simplify (


interp :: TensExpr -> Acc a -- Interpret my expressions into an Accelerate computation
interp (Scalar x) = unit x
interp (VecLit x) = use x
interp (Kron a b) = 


instance Num TensExpr where
	a + b = Sum a b
	a * b = Kron a b
	fromInteger x = Scalar (fromInteger x)
	negate x = Kron (Scalar -1) x

-- Labelling the indices is hard. Giving explicit number labels sucks.
-- 
-- Fusion of labels changes the name of labels after the largest fusion.

type VecPoly' = [TensExpr]
-- The intention is that each term will have a higher polynomial of 

integT x = (Scalar 0) : 

expT :: VecPoly' -> VecPoly'
expT x = (deriv (expT x)) / (deriv x) -- This is not good either. --- (Scalar 1) : (integT (expT x))



