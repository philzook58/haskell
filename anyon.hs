
--data Anyon = Fuse Anyon Anyon Anyon | Braid  

data ParticleType = Tau | I
-- 
-- A split node is the splitting of an anyon into two anyons
-- Each Branch is Labell
data AnyonTree = SplitNode AnyonTree  ParticleType AnyonTree ParticleType | Nil
-- I guess it tacitly starts with an identity node unless you include this
data RootTree = ParticleType AnyonTree


--assoc (a,(b,c)) = (F a b c d e f) ((d,e),f) 
N_ijk Tau Tau = I

data Anyon = Fusion Anyon Anyon Anyon

data FixedAnyon -- at ends. temrinal
data IntermediateAnyon -- on intermediate edges

tensorProduct a b = 
braid a b = b a

-- hilbert space as list of amplitude * vectors
--data Hilbert = Term (Complex, Vec) Hilbert | Nil
data Hilbert a = [(Complex Float, a)]
data ClassicalSpin = Up | Down
data Spin = Hilbert Spin

tensorProduct hilb1 hilb2 = map (\(amp1, base1) -> map (\(amp2, base2) -> (amp1 * amp2, (base1, base2))) hilb2) hilb1 

-- look at how pipnoi does this


-- arrows?

-- the parrallel product should correspond to a Kron
-- *** looks like fusion vertex
-- 