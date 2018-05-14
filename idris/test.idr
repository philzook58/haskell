
module test

import Data.Vect

myfunc : Nat -> Nat
myfunc x = x + 1

-- always returns True
booly : (a : Type) -> Bool
booly Bool = True
booly Nat = False


-- I guess I can't use types as values...
-- makes sense. Kind of. Types aren't values...
-- runtime has no access to type info.
-- except that Int is a value of Type
-- shouldn't this not compile then?
typy : (a : Type) -> Type
typy Nat = Bool
typy Bool = Nat



succor : Nat -> Nat
succor x = S x




{-
-- This does not compile
idtype : (a : Type) -> a
idtype Nat = Z
-- Type mismatch between Nat (Type of 0) and Nat (Expected type)?
-}






-- This works fine
isSingleton : Bool -> Type
isSingleton True = Nat
isSingleton False = Bool

AddType : Nat -> Type
AddType Z = Nat
AddType (S k) = Nat -> (AddType k) 

adder : (n:Nat) -> Nat -> AddType n
adder Z acc = acc
adder (S k) acc = \x => adder k (acc+x)

data DataStore : Type -> Type where
  MkData : (size : Nat) -> (items : Vect size schema) -> DataStore schema
  
  
