dupli = foldl (\acc y-> acc ++ [y,y] )  []

repli x a = foldl (\acc y-> acc ++ (take a (repeat y)) )  [] x 
