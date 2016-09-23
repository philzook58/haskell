module Main where
import System.Environment

-- adds numbers from command line and accepts a string that it prints back out. Monads are f-ed up
main :: IO ()
main = do
    args <- getArgs
    num <- getLine
    putStrLn ("Hello, " ++ show  (foldr1 (+)  (map read args)))
    putStrLn num
