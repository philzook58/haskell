

data Operator =  (Double   | 


class QOperator where
	normalprod :: QOperator -> QOperator -> QOperator
	expectprod :: QOperator -> QOperator -> Double
	liftCNumber :: Double -> QOperator

-- The basic data to start from is the 2 point green's function.
-- and the normal ordering :phi phi: 

data Prod a b = Prod a b


-- Simple algebraic expressions.
data QOperator = Prod QOperator QOperator | CNum Double | Sum QOperator QOperator 

data QOperator' = Prod [QOperator] | CNum Double | Sum [QOperator] | A Int | ADag Int

data QOperator'' a = SumofProd [[a]]

data BasicOp index = A index | ADag index | CNum Double

type OpExpr a = QOperator'' (BasicOp a)

normalorder 
expectation 


instance Operator a => QOperator [a]
	where 

-- The product is defined recursively using wick's theorem.
-- porduct is implicitly T ordered product. (or whatever actually)
-- (*) = 

-- The question is then, what the hell does an operator data type actually contain.
-- Basically, probably an abstract syntac tree of the operators
-- showing their multiplication and addition.
-- This is somewhat disappointing. Basically doing computer algebra again

-- If instead we really define the quantum field to be a operator valued function

data QuantumField = Double -> Operator

-- Or something more vector like. That has a listing 

data QuantumField' = [(Double, Operator)]

--To have anything appear more numerical, we need to reemphasize the underlying space.
-- not the hilbert space. That probably needs to stay hidden because it is too big.
 

-- The Quantum Field is a vector where each entry is an algebraic quantity.
-- In the sense that the algebraic quantities can be mapped to an underlying Hilbert space
-- This is a block vector or long bloc matrix.

-- The product of quantum fields is a matrix where each entry is algebraic.
-- If this matrix has the hierarchical/HSS/H2 structure, that is an OPE.
-- the transfer matrices are low rank. And they combine on an operator matrix in the middle.

-- The Interpolative decomp is perfect because it selects columns.




instance (QOperator a) => Num a where
	(+) a b = 
	(*) a b = (normalprod a b) + (expectprod a b)


-- different representations?


--use powser in g parameter for perturbation series.
-- maybe a newtype actually so we can overload addition and such

type Powser a = [a]

phi_of_t :: Double -> Powser Operator

a_of_t :: Double -> Powser Operator





