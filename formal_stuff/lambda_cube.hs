data Expr = Var Int | 
--	Const Int |
 	Apply Expr Expr | 
	Lambda Int Expr Expr | -- \ Var Num:Type. Term
	Pi Int Expr Expr -- is this right? Do I introduce a new type variable?
	Kind |
	Sort |
	Nat |
	Succ Expr |
	Zero


beta (Apply (Lambda x type term1) term2) = substitute x term1 term2
beta other = other

substitute x (Var n) term = if (n == x) then term else (Var n)  
substitute x (Apply expr1 expr2) term = Apply (substitute x expr1 term) (substitute x expr2 term) 
substitute x (Lambda a expr1 expr2) term = Lambda a (substitute x expr1 term) (substitute x expr2 term) 
substitute x (Pi a expr1 expr2) term = Pi a (substitute x expr1 term) (substitute x expr2 term) 
substitute x expr term = expr 


--typecheck env (Const n) = Const (n+1)
-- I should put the Nothings in there.
typecheck env term@(Apply _ _) = typecheck env (beta term) -- application rule

typecheck env (Var x) = lookup x env -- start rule I think
typecheck _ Kind = Sort
typecheck env (Lambda x expr1 expr2) = Pi vartype (typecheck env' expr2) where env' = (x,vartype ):env
							 vartype = (typecheck env expr1)
typecheck env (Pi x expr1 expr2) =  if vartype == Kind then typecheck env' expr2   where env' = (x,vartype):env
					 	vartype = (typecheck env expr1)

typecheck env (Pi x expr1 expr2) =  if vartype == Kind then do 
				vartype <- typecheck env expr1
				return (typecheck ((x,vartype):env) expr2)  
			else Nothing 

typecheck _ Nat = Kind
typecheck _ (Succ n) = if (typecheck env n) == Nat then Just Nat else Nothing
typecheck _ Zero = Nat  



-- a -> Vect n a -> Vect (n+1) a

-- 


