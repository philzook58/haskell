import Data.List

--types are first class

StringOrNat : (isStr : Bool) -> Type
StringOrNat False = Nat
StringOrNat True = String

lengthOrDouble : (isStr : Bool) -> StringOrNat isStr -> Nat
lengthOrDouble False x = x + x
lengthOrDouble True x = 0


--printf
printf : (f : String) -> Type



my_append : List a -> List a -> List a
my_append [] [] = ?my_append_rhs_3
my_append [] (x :: xs) = ?my_append_rhs_4
my_append (x :: xs) ys = ?my_append_rhs_2


--my_map

data Vect : Nat -> Type -> Type where
  Nil : Vect Z a
  (::) : a -> Vect k a -> Vect (S k) a

--%name Vect xs, ys zs

append : Vect n a -> Vect m a -> Vect (n+m) a
append [] y = ?append
append (x :: z) y = ?append_rhs_2

zip :  Vect n a -> Vect n a -> Vect n (a,a)
zip [] [] = ?zip_rhs_3
zip (x :: z) (y :: w) = ?zip_rhs_1

transpose_mat : Vect n (Vect m elem) -> Vect m (Vect n elem)
transpose_mat [] = ?empties
transpose_mat (x :: y) = ?transpose_mat_rhs_2
