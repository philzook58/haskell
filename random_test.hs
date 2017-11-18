import System.Random

main = do
   gen <- newStdGen
   let ns = randoms gen :: [Float]
   print $ take 10 ns
   print $ take 10 (randomRs (0.0,10.0) gen :: [Float])

getrandFloat :: State StdGen Float
getrandFloat = State
