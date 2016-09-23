
-- take a list and return a list of all permutations of that list
-- I bet this is a 1 liner

-- map?

--permute' x:xs = x

--map
-- not a map becausey ou need the whole rest of the list every time
--permute'' xs = map (\x -> x) xs  


-- subproblem
-- return list of lists with every 1 element removed

removeones' :: [a] -> [a] -> [[a]]
removeones' ys (x:xs) =  [ys ++ xs] ++ (removeones' (x:ys) xs)
removeones' ys [] = []    
removeones xs = removeones' [] xs

-- could this be an unfold? We're generating a list

--permutes xs = map removeones

-- could using nondeterminstic list combination help?
-- using remove ones recursively would generate an list of empty lists of size n!
-- 

permutes [] = return [] -- return should = [[]]
permutes x = do  s <- removeones x
                 y  <- permutes s
                 return y 

-- does work. I think this is a full monad. Not doable as an applicative?
-- this is pretty close
