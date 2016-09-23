Syntax
~~~~~~

> type Name = String
> 
> data Expr = Let Decl Expr
>           | App Expr Expr
>           | Var Name
>           | Int Int
>           
> data Decl = Fun Name [Name] Expr


Semantics
~~~~~~~~~

> data Val  = IntVal Int | FunVal (Val -> Val)
> type Env  = [(Name,Val)]
> 
> evalExpr :: Expr -> Env -> Val
> evalExpr expr env = case expr of
>   Let d e   -> evalExpr e (evalDecl d env ++ env)
>   App e1 e2 -> case evalExpr e1 env of
>                  FunVal f -> f (evalExpr e2 env)
>                  _ -> error "Type error."
>   Var x     -> case lookup x env of
>                  Just v  -> v
>                  Nothing -> error "Undefined variable."
>   Int x     -> IntVal x
> 
> evalDecl :: Decl -> Env -> Env
> evalDecl decl env = case decl of
>   Fun f xs e -> [(f,fun env xs)]
>     where fun env (x:xs) = FunVal (\v -> fun ((x,v):env) xs)
>           fun env []     = evalExpr e env


Examples
~~~~~~~~

> test e  = evalExpr 
>         ( Let (Fun "id" ["x"] $ Var "x") 
>         $ Let (Fun "const" ["x","y"] $ Var "x") e) []
>
> ex1 = test (Var "id" `App` Int 2)
> ex2 = test (Var "const" `App` Int 2 `App` Int 3)
> ex3 = test (Var "const" `App` Var "id" `App` Int 2 `App` Int 3)
 
> instance Show Val where
>   show val = case val of
>     IntVal x -> show x
>     FunVal _ -> "<function>"
> 


