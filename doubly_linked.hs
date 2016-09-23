
import Data.Tuple

type DLL a = ([a],[a])

left:: DLL a -> DLL a 
left (ys,x:xs) = (x:ys,xs) 

right = swap . left . swap

current :: DLL a -> a
current (ys,x:xs)= x 
--repeat x = x:(repeat x)
ones = repeat 1
zeros = repeat 0


heavi = (zeros,ones)


--more symmetrical
-- this is a zipper btw
type DLL2 a = ([a],a,[a])

right2 (ys,x,z:zs) = (x:ys,z,zs)
left2 (y:ys,x,zs) = (ys,y,x:zs)
current2 (ys,x,zs) = x
take2 n (ys,x,zs)=  x : take (n-1) zs
neighbors n a = take2 (2*n+1) $ iterate left2 a !! n


--pos = 1:((head pos) + 1)
--pos = unfoldl (+1) 1
pos = iterate (+1) 1
neg = map (* (-1)) pos
z :: DLL2 Int
z = (neg,0,pos)