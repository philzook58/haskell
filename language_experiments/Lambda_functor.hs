-- generic types start. 
data MyTypes = TUnit | TFunc MyTypes MyTypes | TProd MyTypes MyTypes | TVar Int | TFunctor MyTypes | TErr deriving (Show, Eq)

-- Map is going to push function underneath functor type
data Term = Unit MyTypes | Lam MyTypes Term | Var Int | Ap Term Term | Map Term Term MyTypes | Fst Term | Snd Term | Pair Term Term deriving Show
-- Map Term Term Mytypes -- term1 is f term 2 is functor value [1] and MyTypes is the Functor type with TVars
-- Map Term -- the term is f. Map f has type Functor a -> Functor b. i.e. it is a TFunc
--
-- Can also add Mu and Nu into type system as kind of modifiers of functors

-- how do I introduce functor types?
-- in map? Map Term MyTypes where f is term and mytypes is the bound TVar?
-- TFunctor has type TFUnctor Body Var?
-- TApp? 

type Gamma = [MyTypes]

infunctype (TFunc a b) = a
outfunctype (TFunc a b) = b

-- to keeptrack of TVar int requires a second context.

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
--typecheck gamma (Map t) = TFunc (TFunctor a) (TFunctor b) where (Lam a b) = typecheck gamma t
-- no . xtype will be the collapsed Functor. i.e. xtype == Functor with all TVars replaced with a
typecheck gamma (Map f x functortype) = if xtype == Functor a then TFunc (TFunctor a) (TFunctor b) else TErr where
   (Lam a b) = typecheck gamma f
   xtype = typecheck gamma x
   
                                                                              
typecheck _ _ = TErr

evalfunctor t:tenv (TVar 0) = t
evalfunctor t:tenv (TVar n) = evalfunctor tenv (TVar (n-1))
evalfunctor tenv TUnit = TUnit
evalfunctor tenv (TProd x y) = TProd (evalfunctor tenv x) (evalfunctor tenv y)
evalfunctor tenv (TFunctor x) = TProd (evalfunctor tenv x) (evalfunctor tenv y)



-- why even both having TUnit? I think I need it

eval :: [Term] -> Term -> Term
eval env (Unit _) = Unit TUnit
eval env (Ap (Lam _ body) term2) = eval ((eval env term2):env) body
eval env (Ap (Var n) term2) = eval env (Ap func term2) where func = eval env (Var n) 
eval (a:env) (Var 0) = a
eval (a:env) (Var n) = eval env (Var (n-1))
eval env (Lam x y) = Lam x y -- I want to eval y but 
eval env tenv (Map a b (Functor TUnit)) = Unit
eval env tenv (Map a b (Functor (Pair x y))) = (Pair (eval env tenv (Map a b (Functor x))) (eval env tenv (Map a b (Functor y))))
eval env (t:tenv) (Map a b (Functor (Functor x))) = -- now we should respond to Tvar 1
eval env (t:tenv) (Map a b (Functor (TVar 0)) = eval env tenv (Ap a b)
-- Functor keyword unneccessary. Only going to show up in map context
-- also should check that it is not in negative function position.
-- curried map (Lam Map f (Var 0) (Functor))
eval _ x = x


run expr = eval [] expr


