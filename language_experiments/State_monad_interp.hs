import Control.Monad.State

data MyTypes = TUnit | TFunc MyTypes MyTypes | TErr deriving (Show, Eq)

data Term = Unit MyTypes | Lam MyTypes Term | Var Int | Ap Term Term deriving Show

type Gamma = [MyTypes]

infunctype (TFunc a b) = a
outfunctype (TFunc a b) = b


typecheck :: Term -> State Gamma MyTypes
typecheck gamma = return TUnit
typecheck gamma (Lam vartype term) = TFunc vartype (typecheck gamma' term) where gamma' = (vartype : gamma)
typecheck (Ap term1 term2) = if intype == vartype then (return outtype) else (return TErr) where intype = infunctype (typecheck gamma term1)
                                                                                     outtype = outfunctype (typecheck gamma term1)
                                                                                     vartype = typecheck gamma term2 

typecheck (Var 0) = pop
typecheck (Var n) = pop >>> typecheck (Var (n -1))
typecheck _ _ = TErr


pop = State $ \(a:as) -> (a,as)


n = Unit TUnit

myexpr = (Lam TUnit (Var 0))
myexpr2 = Ap myexpr (Unit TUnit)
myexpr3 = Ap myexpr4 (Lam TUnit (Unit TUnit)) -- higher order function. Takes
-- func and gives it Unit
myexpr4 =  (Lam (TFunc TUnit TUnit) (Ap (Var 0) (Unit TUnit)))
nestedexpr = (Lam TUnit (Lam TUnit (Var 1))) -- With no unique stuff, pretty hard to check.
myexpr5 = Ap nestedexpr (Unit TUnit)
myexpr6 = (Lam TUnit (Ap (Lam TUnit (Var 1)) (Unit TUnit)))
myexpr7 =  Ap myexpr6 (Unit TUnit)
failexpr = Ap (Lam TUnit (Var 0)) (Lam TUnit (Unit TUnit))
nullgamma = []


eval :: [Term] -> Term -> Term
eval env (Unit _) = Unit TUnit
eval env (Ap (Lam _ body) term2) = eval ((eval env term2):env) body
eval env (Ap (Var n) term2) = eval env (Ap func term2) where func = eval env (Var n) 
eval (a:env) (Var 0) = a
eval (a:env) (Var n) = eval env (Var (n-1))
eval env (Lam x y) = Lam x y -- I want to eval y but 
eval _ x = x


run expr = eval [] expr




