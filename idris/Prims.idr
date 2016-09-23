module Prims

x : Int
x = 42

foo : String
foo = "Sausage machine"

bar : Char
bar = 'Z'

quux : Bool
quux = False

reverse : List a -> List a
reverse xs = revAcc [] xs where
  revAcc : List a -> List a -> List a
  revAcc acc [] = acc
  revAcc acc (x :: xs) = revAcc (x :: acc) xs


-- calcluates a type from a bool value
isSingleton : Bool -> Type
isSingleton True = Nat
isSingleton False = List Nat

-- calulating return type in type classificaiton. whaaaaat.
-- I on't think haskell 
mkSingle : (x : Bool) -> isSingleton x
mkSingle True = 0
mkSingle False = []

