
data MyTypes = TUnit | TFunc MyTypes MyTypes | TErr deriving (Show, Eq)

data Term = Unit | Lam MyTypes Term  | Var Int | Ap Term Term deriving Show
 


--checkType term type env = undefined


inferType :: Term -> [MyTypes] -> MyTypes
inferType Unit env = TUnit
inferType (Var 0) (a:env) = a
inferType (Var j) (a:env) = inferType (Var (j-1)) env
inferType (Ap t1 t2) env = outtype where (TFunc intype outtype) = inferType t1 env -- Check type of input of t1 and t2 match. 


inferType (Lam intype term) env = (TFunc intype (inferType term (intype:env)))
