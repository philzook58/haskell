


type Vec3 = [Float]



(.+) :: Vec3 -> Vec3 -> Vec3
a .+ b =  zipWith (+) a b
(.-) :: Vec3 -> Vec3 -> Vec3
a .- b = zipWith (-) a b

norm2 :: Vec3 -> Float
norm2 a = foldr (\x y-> y + x*x) 0 a

origin = [0,0,0]::Vec3

green0 x x' = 1 / sqrt (norm2 (x .- x'))

type HomogPlane = [Float] 

type Cavity = [HomogPlane]

{- -- A way of oing different represntaitons of planes
class Plane
	reflect:: (a::Plane) -> Vec3 -> a -> Vec3 
	-}

reflect:: Vec3 -> HomogPlane -> Vec3
reflect x p = undefined

--quickCheck reflect (reflect x p) p == x


reflectcavity x cavity = map (reflect x) cavity

a .+. b = (+) <$> a <*> b


{-
-- iterative procedure
-- make green's functions a functor on the ooutput value. so I can easily add them
green = green0 .+. green0 reflect green
greenterms = green0:green0 reflect green
green = iterate (\green -> green .+. green0 reflect green) green0
-}