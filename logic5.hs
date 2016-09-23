
-- look at logic3 for notes on simply typed
data MyTypes = TUnit | TFunc MyTypes MyTypes | TSum MyTypes MyTypes | TErr  deriving (Show, Eq)
data Term = Unit MyTypes | Lam MyTypes Term | Var Int | Ap Term Term | CaseLR Term Term Term | VLeft Term MyTypes | VRight MyTypes Term deriving Show

-- CaseLR is condition application. lterm is a function that will take a value that was wrapped in VLeft

type Gamma = [MyTypes]

infunctype (TFunc a b) = a
outfunctype (TFunc a b) = b


typecheck :: Gamma -> Term -> MyTypes
typecheck gamma (Unit TUnit) = TUnit
typecheck gamma (Lam vartype term) = TFunc vartype (typecheck gamma' term) where gamma' = (vartype : gamma)
typecheck gamma (Ap term1 term2) = if intype == vartype then outtype else TErr where intype = infunctype (typecheck gamma term1)
                                                                                     outtype = outfunctype (typecheck gamma term1)
                                                                                     vartype = typecheck gamma term2 

typecheck (a:gamma) (Var 0) = a
typecheck (a:gamma) (Var n) = typecheck gamma (Var (n-1)) 
typecheck gamma (VLeft term rtype) = TSum (typecheck gamma term) rtype
typecheck gamma (VRight ltype term) = TSum ltype (typecheck gamma term)
typecheck gamma (CaseLR cond lchoice rchoice) = if and [outr == outl, ltype == inl, rtype == inr] then outr else TErr where 
                                                                                    TFunc inr outr = typecheck gamma rchoice 
                                                                                    TFunc inl outl = typecheck gamma lchoice 
                                                                                    TSum ltype rtype = typecheck gamma cond
typecheck _ _ = TErr

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
-- type this to give it a try
-- typecheck nullgamma myexpr


eval :: [Term] -> Term -> Term
eval env (Unit _) = Unit TUnit
eval env (Ap (Lam _ body) term2) = eval ((eval env term2):env) body
eval env (Ap (Var n) term2) = eval env (Ap func term2) where func = eval env (Var n) 
eval (a:env) (Var 0) = a
eval (a:env) (Var n) = eval env (Var (n-1))
eval env (Lam x y) = Lam x y -- I want to eval y but 
eval _ x = x


run expr = eval [] expr