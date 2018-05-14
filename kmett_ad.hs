import Numeric.AD

dsin = diff sin

-- What the hell is this machine this god has built?

-- compute value and derivative
y1 = diff' (exp . log) 2


-- embed constants
y2 = diff (\ x -> (auto 3) * sin x) 0


-- Wokrs on traversable instances

-- would need to make DSL traversable if that is approach.


-- diffs for higher order derivs
y3 = take 10 $ diffs sin 1


y4 = grad sum [1,2,3]

-- can we do the power series of derivatives thing?

g = 1.0
jgj j = exp (j * (auto 4) * j)
jgj' j = exp (sum $ map (\j' -> j' * j') j)

y5 = diffs jgj 0

