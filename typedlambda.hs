-- check out logic3.hs

-- Nah I f'ed this up.
-- I don't want bools next. I want product types
data MyTypes = TUnit | TFunc MyTypes MyTypes | TBool | TErr deriving (Show, Eq)

data Term = Unit MyTypes | Lam MyTypes Term | Var Int | Ap Term Term | IfThenElse Term Term Term | LitBool Bool MyTypes deriving Show


type Gamma = [MyTypes]

infunctype (TFunc a b) = a
outfunctype (TFunc a b) = b


typecheck :: Gamma -> Term -> MyTypes
typecheck gamma (Unit TUnit) = TUnit
typecheck gamma (Lam vartype term) = TFunc vartype (typecheck gamma' term) where gamma' = (vartype : gamma)
typecheck gamma (Ap term1 term2) = if intype == vartype then outtype else TErr where intype = infunctype (typecheck gamma term1)
                                                                                     outtype = outfunctype (typecheck gamma term1)
                                                                                     vartype = typecheck gamma term2 
typecheck gamma (LitBool x TBool) = TBool
typecheck  
typecheck (a:gamma) (Var 0) = a
typecheck (a:gamma) (Var n) = typecheck gamma (Var (n-1)) 
typecheck _ _ = TErr
 


n = Unit TUnit

myexpr = (Lam TUnit (Var 0))
myexpr2 = Ap myexpr (Unit TUnit)
myexpr3 = Ap (Lam (TFunc TUnit TUnit) (Ap (Var 0) (Unit TUnit))) (Lam TUnit (Unit TUnit)) -- higher order function. Takes

myexpr4 =  (Lam (TFunc TUnit TUnit) (Ap (Var 0) (Unit TUnit)))
nestedexpr = (Lam TUnit (Lam TUnit (Var 1))) -- With no unique stuff, pretty hard to check.
failexpr = Ap (Lam TUnit (Var 0)) (Lam TUnit (Unit TUnit))
nullgamma = []
