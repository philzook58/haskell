import System.Environment
import Text.Parsec
import Data.List.Split


splitcomma = splitOn "," 

parseboard :: String -> [Int]
parseboard str = map read (splitcomma str)

data Moves = Left | Right | Up | Down

data Board a = Board a a a 
					 a a a 
					 a a a deriving Show

main = do 
    args <- getArgs
    let boardstr = args !! 1
    let board = parseboard boardstr
    print board
--		searchtype = args !! 0


isSorted as = and $ map (\(a,b) -> a <= b) neighbs where neighbs = zip as (tail as) 


emptyPos board = elemIndex 0 board
possibleMoves board = 



