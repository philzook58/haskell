--pack = foldl skipdups [[]]
--   where skipdups [[]] a = [[a]]
--         skipdups (x:xs) a =

neighborPair x = zip x (tail x)
compress x = [head x] ++ (map snd (filter (\ a -> fst a /= snd a) $ neighborPair x))

pack x = compress $ map (\a -> filter ( == a) x) x

encode y = map (\x  -> (length x, head x) ) $ pack y 
