

-- lookup key ls
import Data.Maybe

data MyTypes = TUnit | TFunc MyTypes MyTypes | TErr deriving (Show, Eq)
data Term = Unit MyTypes | Lam MyTypes String Term | Var String | Ap Term Term deriving Show

letTerm label value body = (Ap (Lam TUnit label body) (value)) -- should actually get type of value

eval :: [(String,Term)] -> Term -> Term
eval env (Unit _) = Unit TUnit
eval env (Ap (Lam _ label body) term2) = eval ((label,(eval env term2)) :env) body
eval env (Ap (Var label) term2) = eval env (Ap func term2) where func = eval env (Var label) 
eval env (Var label) = stripMaybe (lookup label env) where 
															stripMaybe Nothing = (Var label)
															stripMaybe (Just term) = term
--eval env (Lam x y) = Lam x y -- I want to eval y but 
eval _ x = x


myexpr = (Ap  (Lam TUnit "x" (Var "x"))  (Unit TUnit))
myexpr' = (Lam TUnit "x" (Var "x"))