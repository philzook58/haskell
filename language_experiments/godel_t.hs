-- Godel's system T
-- remove Unit?

-- remove


data MyTypes = Nat | TFunc MyTypes MyTypes | TErr deriving (Show, Eq)

data Term = Zero | Succ Term | RecurseN Term Term Term | Lam MyTypes Term | Var Int | Ap Term Term deriving Show



type Gamma = [MyTypes]

infunctype (TFunc a b) = a
outfunctype (TFunc a b) = b


typecheck :: Gamma -> Term -> MyTypes
typecheck gamma (Zero) = Nat
typecheck gamma (Succ x) = if (typecheck gamma x == Nat) then Nat else TErr
typecheck gamma (RecurseN a b c) = if and [x == y, y == typec, typea == Nat] then typec else TErr where typea = typecheck gamma a
                                                                                                        (TFunc x y) = typecheck gamma b
                                                                                                        typec = typecheck gamma c

typecheck gamma (Lam vartype term) = TFunc vartype (typecheck gamma' term) where gamma' = (vartype : gamma)
typecheck gamma (Ap term1 term2) = if intype == vartype then outtype else TErr where intype = infunctype (typecheck gamma term1)
                                                                                     outtype = outfunctype (typecheck gamma term1)
                                                                                     vartype = typecheck gamma term2 

typecheck (a:gamma) (Var 0) = a
typecheck (a:gamma) (Var n) = typecheck gamma (Var (n-1)) 
typecheck _ _ = TErr
 

-- convenience function for building terms in Haskell. Could expand them out at loss of clarity

plus t1 t2 = RecurseN t1 (Lam Nat (Succ (Var 0))) t2
-- alternatively use built-in substitution mechanism. This syntax is unbearable for larger expressions.
plus' t1 t2 = Ap (Lam Nat (Ap (Lam Nat (RecurseN (Var 1) (Lam Nat (Succ (Var 0))) (Var 0))) t2)) t1
con 0 = Zero 
con x = Succ (con (x-1)) 
mult t1 t2 = RecurseN t1 (Lam Nat (plus t2 (Var 0))) Zero
square t = Ap (Lam Nat (mult (Var 0) (Var 1))) t 

-- test a higher order function also




expr1 = plus (con 3) (con 2)
expr2 = plus Zero Zero
expr3  = mult (con 2) (con 2)
expr4 = mult Zero (con 1)
expr5  = mult (con 2) (con 3)
expr6 = square (con 2)


nullgamma = []

-- typecheck nullgamma myexpr


eval :: [Term] -> Term -> Term
eval env (Zero) = Zero
eval env (Succ x) = Succ (eval env x) 
eval env (Ap (Lam _ body) term2) = eval ((eval env term2):env) body
eval env (Ap (Var n) term2) = eval env (Ap func term2) where func = eval env (Var n) 
eval (a:env) (Var 0) = a
eval (a:env) (Var n) = eval env (Var (n-1))
eval env (Lam x y) = Lam x y -- I want to eval y but 
eval env (RecurseN (Succ x) y z) = eval env (RecurseN x y (Ap y z)) 
eval env (RecurseN Zero y z) = (eval env z)
-- I'm uncomfortable with the following. Should I really have to check all the cases
-- sometimes x might be a var, or unevaluted applied lambda expression
-- could this get into an infinite loop?
eval env (RecurseN x y z) = eval env (RecurseN (eval env x) y z)
eval _ x = x


run expr = eval [] expr




