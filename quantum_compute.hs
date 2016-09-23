--Linearity can be guarantee by writing operators in monadic format.

--Quantum computation - the gates take classical parameters.
-- This is like the State Monad param -> State (s -> (ret, s'))
-- It's the state monad except bind has to be a little different, includes the linearity of 
-- param -> Quant (\basis -> vector)
--There should be some relative

-- kronecker product for parallel computation?

-- maybe measure pops out one level to a stateful computation
-- ApplyHadmard trueorfalse = if true then (>> hadamard) oelse

-- push a = State (\stack -> ((),stack++ a) )
-- pop = State (\stack -> (head stack, tail stack))
-- bind (State f) op = op retval

-- build the quantum teleporter
-- 


-- maybe this is a doubled monad. The Vector monad inside the state man, and 

-- A quantum interpeter has a lot of overhead. It does need to do all possiblities in parralel
data Ket = Sho Int | Empty | Full
data AmpKet = [(Complex Float, Ket)]

-- this is... not in the pattern of State. ampket is something like (amp, ket) like (retval, state)
--newtype QuantState = Quantstate (Ket -> AmpKet)

-- so perhaps we don't measure perhaps we do. Then further computation can depend on measurement
newtype QuantState measurement = QuantState (Ket -> (measurement, AmpKet))

-- My picture is that what linear operators you may wish to apply depends on what classical measurements you receive




--Should the parameters be quantum? Or could? contringent on some quantum state, do something else... Kind of a controlled Not feel?
-- Maybe that's a way to cross link 

-- couuld shift into a nondeterministic calculation once measured.
-- work all in desnity matrices


instance Monad QuantState where
	return x = QuantState (\ket -> )
	QuantState s >>= f = 

-- God why do it this way. You could definetly make conrete hilbert spaces in numpy.