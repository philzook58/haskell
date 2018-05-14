data Nat = L () | R Nat deriving Show

z :: Nat
z = L ()

s :: Nat -> Nat
s n = R n 

data In f = In {out :: f }