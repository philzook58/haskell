> {-# OPTIONS -fglasgow-exts #-}

Parameterized Syntax
~~~~~~~~~~~~~~~~~~~~

> type Name       = String
> 
> data Expr e d   = Let d e
>                 | App e e
>                 | Var Name
>                 | Int Int
> 
> data Decl e d   = Fun Name [Name] e


Parameterized Semantics 
~~~~~~~~~~~~~~~~~~~~~~~

> data Val        = IntVal Int | FunVal (Val -> Val)
> type Env        = [(Name,Val)]
> 
> class Eval e d | e -> d, d -> e where 
>   expr :: e -> Env -> Val
>   decl :: d -> Env -> Env 
> 
> instance (Eval e d) => Eval (Expr e d) (Decl e d) where
>   expr e env = case e of
>     Let d e   -> expr e (decl d env ++ env)
>     App e1 e2 -> case expr e1 env of
>                    FunVal f -> f (expr e2 env)
>                    _ -> error "Type error."
>     Var x     -> case lookup x env of
>                    Just v  -> v
>                    Nothing -> error "Undefined variable."
>     Int x     -> IntVal x
> 
>   decl d env = case d of
>     Fun f xs e -> [(f,args env xs)]
>       where args env (x:xs) = FunVal (\v -> args ((x,v):env) xs)
>             args env []     = expr e env


Language 1: Tying the Knot 
~~~~~~~~~~~~~~~~~~~~~~~~~~

> newtype Expr1 = E1 (Expr Expr1 Decl1)
> newtype Decl1 = D1 (Decl Expr1 Decl1)
> 
> instance Eval Expr1 Decl1 where 
>   expr (E1 e) env = expr e env
>   decl (D1 e) env = decl e env
 
Examples:

> var1 x      = E1 $ Var x
> int1 x      = E1 $ Int x
> app1 f x    = E1 $ App f x
> let1 d e    = E1 $ Let d e
> fun1 f xs e = D1 $ Fun f xs e
>  
> test1 e = expr
>         ( let1 (fun1 "id" ["x"]        $ var1 "x") 
>         $ let1 (fun1 "const" ["x","y"] $ var1 "x") e) []
> 
> ex1     = test1 $ var1 "id"    `app1` int1 2
> ex2     = test1 $ var1 "const" `app1` int1 2    `app1` int1 3
> ex3     = test1 $ var1 "const" `app1` var1 "id" `app1` int1 2 `app1` int1 3


Language 2: Tying the Know with an Extension
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 

> data Expr2    = E2 (Expr Expr2 Decl2)
>               | Add Expr2 Expr2
> newtype Decl2 = D2 (Decl Expr2 Decl2)
> 
> instance Eval Expr2 Decl2 where
>   expr (E2 e) env     = expr e env
>   expr (Add e1 e2) env = case (expr e1 env, expr e2 env) of
>                            (IntVal x, IntVal y) -> IntVal (x+y)
>                            _ -> error "Type error."
>   decl (D2 d) env = decl d env
 
Examples:

> var2 x      = E2 $ Var x
> int2 x      = E2 $ Int x
> app2 f x    = E2 $ App f x
> let2 d e    = E2 $ Let d e
> fun2 f xs e = D2 $ Fun f xs e
> 
> test2 e = expr
>         ( let2 (fun2 "id" ["x"]        $ var2 "x") 
>         $ let2 (fun2 "const" ["x","y"] $ var2 "x") e) []
> 
> ex4     = test2 $ var2 "id"    `app2` int2 2
> ex5     = test2 $ var2 "const" `app2` int2 2    `app2` int2 3
> ex6     = test2 $ var2 "const" `app2` var2 "id" `app2` int2 2 `app2` int2 3
> ex7     = test2 $ var2 "id" `app2` (int2 3 `Add` int2 7)


> instance Show Val where
>   show val = case val of
>     IntVal x -> show x
>     FunVal _ -> error "<function>"

           



 
