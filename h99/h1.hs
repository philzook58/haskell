myLast = head . reverse
myLast' = foldr1 (flip const)
