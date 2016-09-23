myLength x = foldl (\x y -> x+1) 0 x
myLength' (x:xs) = 1 + myLength xs
myLength'' = sum . map (\_ -> 1) 
