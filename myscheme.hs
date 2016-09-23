
--data Expr a = Atomic a | If  Expr Then Expr Else Expr | 


{- 
--Step One: A simple addition avaluator
--I'm more interested in the meat after the lexing and parsing stage now

data Expr = Plus  Expr Expr | Times Expr Expr | Atomic Int deriving Show


eval (Atomic x) = x
eval (Plus x y) = (eval x) + (eval y)
eval (Times x y) = (eval x) * (eval y)


-- okay so that was easy
-}


--Step two
-- Add in variables
-- now we need to pass down the environment that has bindings for variables
-- and pass back the env also

{-
import qualified Data.Map.Strict as Map
import Data.Maybe
--I'm more interested in the meat after the lexing and parsing stage now
data Expr = Plus Expr Expr | Times Expr Expr | Atomic Int | Let String Expr Expr | Var String deriving Show


-- Threading env though all of this is bad. Should use a monad to do so
eval env (Atomic x) = x
eval env (Plus x y) = (eval env x) + (eval env y)
eval env (Times x y) = (eval env x) * (eval env y)
eval env (Let name expr1 expr2) = eval (Map.insert name expr1 env) expr2
eval env (Var name) = eval env (fromJust $ Map.lookup name env)

start expr = eval Map.empty expr 

-- start $ Let "x" (Atomic 3) (Var "x")
-- 3

-- start $ Let "x" (Atomic 3) (Plus (Var "x") (Atomic 4))
-- 7

-}


import qualified Data.Map.Strict as Map
import Data.Maybe
--I'm more interested in the meat after the lexing and parsing stage now
data Expr = Plus Expr Expr | Times Expr Expr | Atomic Int | Let String Expr Expr | Var String | Lambda String Expr | Apply Expr Expr deriving Show


-- Threading env though all of this is bad. Should use a monad to do so
eval env (Atomic x) = x
eval env (Plus x y) = (eval env x) + (eval env y)
eval env (Times x y) = (eval env x) * (eval env y)
eval env (Let name expr1 expr2) = eval (Map.insert name expr1 env) expr2
eval env (Var name) = eval env (fromJust $ Map.lookup name env)
eval env (Apply (Lambda dummyname bodyexpr) arg) = eval env (Let dummyname (Atomic (eval env arg)) bodyexpr)
eval env (Apply f arg) =  eval env (Apply (eval env f) arg)


define env name expr = Map.insert name expr env
--define :: String -> Expr -> Map.Map -> Map.Map
--define = Map.insert
emptyenv = Map.empty
startenv = define emptyenv "f" (Lambda "x" (Plus (Var "x") (Var "x")))

-- backtracking of scope not available? Or is it there?
prog1 = eval startenv (Apply (Var "f") (Atomic 3)) 


--apply dummy 


-- The apply function is using the syntax tree as an applicative functor? tree1 <*> tree2 Applies tree1 to tree2?




