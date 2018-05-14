import Data.Complex

newtype Hilbert a = Hilbert [(Complex Float, a)] deriving Show
data ClassicalSpin = Up | Down deriving Show
newtype Spin = Spin (Hilbert ClassicalSpin) deriving Show

class Functor

tensorProduct hilb1 hilb2 = map (\(amp1, base1) -> map (\(amp2, base2) -> (amp1 * amp2, (base1, base2))) hilb2) hilb1 