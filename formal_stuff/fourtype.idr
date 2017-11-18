

data FourType : Type where
  IsFour : (n:Nat) -> n=4 -> FourType
  
four : FourType
four = IsFour 4 Refl

four' : FourType
four' = IsFour (2+2) Refl

succEq : (m=n) -> ((S m) = (S n))
succEq prf = rewrite prf in Refl


maybeEq : (n:Nat) -> (m:Nat) -> Maybe (m=n)
maybeEq Z Z = Just Refl
maybeEq (S x) (S y) = do prf <- (maybeEq x y)
                         pure (succEq prf)
maybeEq _ _ = Nothing

maybeEqFour : (m:Nat) -> Maybe (m=4)
maybeEqFour = maybeEq 4 


maybeFour : Nat -> Maybe FourType
  
maybeFour x = do prf <- (maybeEqFour x)
                 pure (IsFour x prf)

