
data MyTypes = TUnit | TFunc MyTypes MyTypes | TProd MyTypes MyTypes | TErr deriving (Show, Eq)

data Term = Unit MyTypes | Lam MyTypes Term | Var Int | Ap Term Term | Fst Term | Snd Term | Pair Term Term deriving Show


type Gamma = [MyTypes]

infunctype (TFunc a b) = a
outfunctype (TFunc a b) = b


typecheck :: Gamma -> Term -> MyTypes
typecheck gamma (Unit TUnit) = TUnit
typecheck gamma (Lam vartype term) = TFunc vartype (typecheck gamma' term) where gamma' = (vartype : gamma)
--typecheck gamma (Ap term1 term2) = if intype == vartype then outtype else TErr where intype = infunctype (typecheck gamma term1)
--                                                                                     outtype = outfunctype (typecheck gamma term1)
--                                                                                     vartype = typecheck gamma term2 
typecheck gamma (Ap (Lam vartype body) term2) = if vartype == typecheck gamma term2 then typecheck (vartype:gamma) body else TErr                                                                              
typecheck gamma (Pair term1 term2) = TProd (typecheck gamma term1) (typecheck gamma term2)
typecheck gamma (Fst (Pair  term1 term2)) = typecheck gamma term1 
typecheck gamma (Snd (Pair  term1 term2)) = typecheck gamma term2 
typecheck (a:gamma) (Var 0) = a
typecheck (a:gamma) (Var n) = typecheck gamma (Var (n-1)) 
typecheck _ _ = TErr

-- why even both having TUnit? I think I need it
