neighborPair x = zip x (tail x)
compress x = [head x] ++ (map snd (filter (\ a -> fst a /= snd a) $ neighborPair x))
--compress = foldl (\(current, pair) -> x == y)
