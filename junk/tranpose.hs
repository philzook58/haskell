import Control.Applicative

transpose :: [[a]] -> [[a]]
--transpose [] = []
--transpose ((a:[]):[]) = [[a]]
transpose ((a:as):bs) =   (a: map head bs) : (transpose (as: (map tail bs)))
--transpose ((a:as): =   (a: map head bs) : (transpose (as: (map tail bs)))
--transpose x = x
transpose _ = []

testmat =  [[1,2,3],
			[4,5,6],
			[7,8,9]]

--transpose2 [] = []
--transpose2 (a:as) = map (map (:) a)) (transpose2 as)
split [] = ([],[])
split (a:[]) = ([a],[])
split (a:b:cs) = ((a:ds),(b:fs)) where (ds,fs) = split cs

ocombine [] bs = bs
ocombine as [] = as
ocombine (a:as) (b:bs) = if a > b then b:(ocombine (a:as) (bs)) else a:(ocombine as (b:bs))

mergesort [] = []
mergesort (a:[]) = [a]
mergesort a =  ocombine s1 s2 where (l1, l2) = split a
                                    s1 = mergesort l1
                                    s2 = mergesort l2