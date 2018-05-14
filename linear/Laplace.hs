-- runghc Laplace.hs

import Numeric.GSL.Fourier
import Numeric.LinearAlgebra
import Data.Complex

-- Some of the namespace crashes with the algebra stuff.
import qualified Graphics.Rendering.Chart.Easy as Plt
import Graphics.Rendering.Chart.Backend.Diagrams(toFile)

fftRow m = fromRows $ (map fft) $ toRows m
fftCol m = fromColumns $ (map fft) $ toColumns m


ifftRow :: Matrix (Complex Double) -> Matrix (Complex Double)
ifftRow m = fromRows $ (map ifft) $ toRows m
ifftCol m = fromColumns $ (map ifft) $ toColumns m


circulant :: Vector (Complex Double) -> Matrix (Complex Double)
circulant v = ifftCol $ fftRow $ diag $ fft v

{-
-- nevermind
rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate n xs = zipWith const (drop n (cycle xs)) xs
-}




num = 20

c = assoc num 0 [(0,-2),(1,1),(num-1,1)] :: Vector C
kmat = circulant c

sol = eigSH (trustSym kmat)
-- doesn't check hermiticty
--sol = eigSH' kmat
eigvals = fst sol

-- a plotting function
signal :: [Int] -> [(Double,Double)]
signal xs = [ (fromIntegral x, realPart $ (c ! x)  )| x <- xs ]


pltVec label v = (Plt.line label [zip [0..] (toList v)])

main = toFile Plt.def "example1_big.svg" $ do
      Plt.plot (Plt.line "Matrix Row" [signal [0..(num-1)]])
      Plt.plot (pltVec "eigenvalues" eigvals)
