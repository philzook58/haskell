neighborPair x = zip x (tail x)
compress x = [head x] ++ (map snd (filter (\ a -> fst a /= snd a) $ neighborPair x))

pack x = compress $ map (\a -> filter ( == a) x) x

encode y = map (\x  -> (length x, head x) ) $ pack y

data SingleMult a b = Single a | Multiple  b a
   deriving Show

wrapPair (a,b)
   | a==1 = Single b
   | otherwise = Multiple a b

encodeMultiple x = map wrapPair (encode x)
--encodeMultiple y = (map  (\(a,b) ->     ) $ encode

decodeModified x = foldl (\x y-> x ++ (singleMultHelper y)) [] x
   where singleMultHelper (Single b) = [b]
         singleMultHelper (Multiple a b) = take a (repeat b)

encodeModified = 
