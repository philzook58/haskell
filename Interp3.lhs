> {-# OPTIONS -fglasgow-exts #-}

"A Pattern for Almost Compositional Functions"
Bjorn Bringert, Aarne Ranta (Chalmers)


Syntax
~~~~~~

> type Name = String
> 
> data Expr
> data Decl
> 
> data Term term cat where
>   Let :: term Decl -> term Expr -> Term term Expr
>   App :: term Expr -> term Expr -> Term term Expr 
>   Var :: Name -> Term term Expr 
>   Int :: Int  -> Term term Expr 
>   Fun :: Name -> [Name] -> term Expr -> Term term Decl


(Paramtrized) Semantics
~~~~~~~~~~~~~~~~~~~~~~~

> data Val cat where
>   IntVal :: Int -> Val Expr
>   FunVal :: (Val Expr -> Val Expr) -> Val Expr
>   Env    :: Env -> Val Decl
> 
> type Env = [(Name, Val Expr)]
> 
> class Eval term where
>   eval :: term cat -> Env -> Val cat
> 
> instance (Eval term) => Eval (Term term) where
>   eval term env = case term of
>     Let d e     -> case eval d env of
>                      Env env' -> eval e (env' ++ env) 
>                      _ -> error "Type error (in GHC)."
>     App e1 e2   -> case eval e1 env of
>                      FunVal f -> f (eval e2 env)
>                      _ -> error "Type error."
>     Var x       -> case lookup x env of
>                      Just v  -> v
>                      Nothing -> error "Undefined variable."
>     Int x       -> IntVal x
>   
>     Fun f xs e  -> Env [(f,args env xs)]
>         where args env (x:xs) = FunVal (\v -> args ((x,v):env) xs)
>               args env []     = eval e env


Language 1: Tying the Knot
~~~~~~~~~~~~~~~~~~~~~~~~~~

> newtype Lang1 cat = L1 (Term Lang1 cat)
> 
> instance Eval Lang1 where
>   eval (L1 term) env = eval term env
 
Examples:

> var1 x      = L1 $ Var x
> int1 x      = L1 $ Int x
> app1 f x    = L1 $ App f x
> let1 d e    = L1 $ Let d e
> fun1 f xs e = L1 $ Fun f xs e
>  
> test1 e = eval
>         ( let1 (fun1 "id" ["x"]        $ var1 "x") 
>         $ let1 (fun1 "const" ["x","y"] $ var1 "x") e) []
> 
> ex1     = test1 $ var1 "id"    `app1` int1 2
> ex2     = test1 $ var1 "const" `app1` int1 2    `app1` int1 3
> ex3     = test1 $ var1 "const" `app1` var1 "id" `app1` int1 2 `app1` int1 3


Language 2: Tying the Knot with an Extension
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

> data Lang2 cat where 
>   L2  :: Term Lang2 cat -> Lang2 cat
>   Add :: Lang2 Expr -> Lang2 Expr -> Lang2 Expr
> 
> instance Eval Lang2 where
>   eval (L2 term) env = eval term env
>   eval (Add e1 e2) env = case (eval e1 env, eval e2 env) of
>                           (IntVal x, IntVal y) -> IntVal (x+y)
>                           _ -> error "Type error."
 
Examples:

> var2 x      = L2 $ Var x
> int2 x      = L2 $ Int x
> app2 f x    = L2 $ App f x
> let2 d e    = L2 $ Let d e
> fun2 f xs e = L2 $ Fun f xs e
> 
> test2 e = eval
>         ( let2 (fun2 "id" ["x"]        $ var2 "x") 
>         $ let2 (fun2 "const" ["x","y"] $ var2 "x") e) []
> 
> ex4     = test2 $ var2 "id"    `app2` int2 2
> ex5     = test2 $ var2 "const" `app2` int2 2    `app2` int2 3
> ex6     = test2 $ var2 "const" `app2` var2 "id" `app2` int2 2 `app2` int2 3
> ex7     = test2 $ var2 "id" `app2` (int2 3 `Add` int2 7)


> instance Show (Val cat) where
>   show term = case term of
>     IntVal x -> show x
>     FunVal _ -> "<function>"
>     Env xs   -> show xs

