
data LogExpr = Pred String [LogExpr] | Var String | Atom String |
	Or LogExpr LogExpr | And LogExpr LogExpr | Not LogExpr 

-- Or could use CNF form 
-- [[LogExpr]]
-- first list is ands
-- second is ors 
-- Then predicate expressions.

-- Unify feels very much like interpeter dispatch
-- the environment threading is like lambda binding
-- substitution 

-- Can use state monad for threading through substitution list

-- totally symmettrci between the two arguments. How do you DRY that out?
-- Should return maybe if unifiable
--
unify :: LogExpr -> LogExpr -> [(String, LogExpr)] -> [(String, LogExpr)]
unify (Atom x) (Atom y) subs = if (x == y) then subs else fail
unify (Var x) (Var y) subs 
	| (x == y) = subs
	| -- let k = lookup x subs in if k == (Var y) then subs else lookup y subs  
	| 
unify (Var x) expr subs = (x, unify expr subs) : subs
unify (Or expr1 expr2) (Or expr3 expr4) subs =  unify expr2 expr4 subs' where subs' = unify expr1 expr3 subs  
-- AND is same as or



-- Substitute is fold like? Or map like?
-- what if the susbtitution table has circular references?
-- or is undigested?
substitute (Var x) subs = if k == Nothing then (Var x) else (fromJust k) where k = lookup x subs
substitute (Pred p expr) subs = Pred p (substitute expr subs)
substitute (Atom x) _ = (Atom x)


substituten n expr subs = substituten (n-1) (substitute expr subs)

-- or should just keep subsitutting until before and after are equal
substituterec expr subs = if (expr' == expr) then expr' else (substituterec expr' subs) where expr' = substitute expr subs

