{-# LANGUAGE ExistentialQuantification #-}


-- map :: forall a b. (a -> b) -> [a] -> [b]


 data ShowBox = forall s. Show s => SB s
 
 heteroList :: [ShowBox]
 heteroList = [SB (), SB 5, SB True]