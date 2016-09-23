

--This is all clearly highly inelegant
-- But why? Well there is a ton of boiler plate
-- The genereal concept of basis vectors

--Also the main thing I want is for the thing to use a adag = adag a + 1

import Data.Complex as Complex
-- try to make a single state
data Basis = Empty | Full

data SHOBasis = Ket Int deriving (Show)

-- do I have to do this?
-- state is sum of complex coefficient time ket
data State = Nil | Term (Complex Float) SHOBasis State deriving (Show)

-- essentially this is List of tuples (amp, ket). 


wrapKet :: SHOBasis -> State
wrapKet ket = Term 1.0 ket Nil




-- Operators need to be summable and multplied by constants
vecsum state1 state2 = 
opsum :: (State-> State) -> (State-> State) -> (State -> State)
opsum x y = \state -> (x state) vecsum (y state)

fmap Nil = Nil
-- f has to return new amplitude and new ket 
-- fmap will essentially make almost linear
-- vector monad takes next step to gurantee linearity
-- operators are then defined ket -> State
fmap f (Term amp ket remainder) = Term (f amp ket) (fmap f remainder) 

constMult :: (Complex Float) -> State -> State
consMult Nil = Nil
constMult const (Term amp ket remainder) = Term (* amp const) ket (constMult const remainder) 

constMult const state = fmap (*const) state
--Could implement the vectorized monad here

adag :: State -> State

adag Nil = Nil
adag (Term amp (Ket n) remainder) = Term (amp * (sqrt (fromIntegral (n + 1)))) (Ket (n + 1)) (adag remainder)

a Nil = Nil
-- Can I do this?
a (adag state) = (vecsum (adag (a state)) state)
a (Term amp (Ket 0) remainder) = Term 0 (Ket 0) (a remainder)
a (Term amp (Ket n) remainder) = Term (amp * (sqrt (fromIntegral n))) (Ket (n-1)) (a remainder)


f = (a . adag . a) (wrapKet (Ket 1))



-- Use Error monad to achieve a(a) Op -> Error possiblyOP



-- Coniutations, could pass down a callback function to use if you've changed something downstream.

