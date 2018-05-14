-- OK

-- A NEW START

-- we're going to make a generating function

-- cabal install fft
-- binds to fftw3 which is good.
--import Math.FFT

import Numeric.GSL.Fourier
import Numeric.LinearAlgebra.Data
import Numeric.LinearAlgebra

x = fft (fromList [1,2,3,4])

newtype FourierDomain a = FourierDomain {compf :: Vector a} 
newtype RealDomain a = RealDomain {compr :: Vector a}   


-- all of this is wrong. Lists won't work.
jgj :: (Numeric a) -> Matrix a -> Vector a -> Vector a -> a
jgj g j1 j2 =  dot j2 (g #> j1)
jgj' g j =  jgj g j j -- split . jgj
djgdj g dj1 dj2 _ = jgj g dj1 dj2 --dump original j on floor const jgj? No that adds one more to the front
jGjD g = (jgj' g) : (djGjD g)
djGjD g = 2 * (jgj g) : (djGdjD g) 
djGdjD g = (djgdj g) : (constFM ((const. const . const) 0))
constFM f = f : constFM (const f) 

-- will this even go through?
data Unfold a b f = Unfold b (Unfold a (f a b) f)




