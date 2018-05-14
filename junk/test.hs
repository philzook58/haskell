--lst :: [Integer]
lst = [1,2,3]
myfun = (sum (map (3*) lst))
x :: Integer
x = 3

sumtorial :: Integer -> Integer
sumtorial 0 = 0
sumtorial n = n + sumtorial (n-1)

data Thing = Shoe
  | Ship
 deriving Show

y :: Thing
y = Ship
