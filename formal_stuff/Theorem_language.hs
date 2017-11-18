data Theorem a = Atom a |
		And Theorem Theorem |
		Or Theorem Theorem |

		Not Theorem |
		Implies Theorem Theorem |
		ForAll String Theorem |
		Exists String Theorem |
		Var String |
		Bottom
-- overload equality with unification
-- keep Formula and Theorem as sepearte data types? Or lock theorems away using parametrism?
-- lock away in Proof monad, with only qed as escape
-- no. actually it is a comonad? qed escapes but 

data FOL = Var String | Relation String [FOL] | Function [FOL]

--either pattern matches or fails

eqI a = Relation "=" a a
eqE (Relation "=" a b) term = substitute a b t1 


selfimpl p = (Impl p p) -- harrison has this as a derived theorem.
modusponens (Impl a b) a' = if (a == a') then b else fail 

explode (Bottom) b = b
orI a b = Or a b

-- This approach feels crazy when Haskell already has so much power available


implI hyp thm
disjI p q = Or p q
disjE (Implies p r1) (Implies q r2) (Or p1 q1) = if and (r1 == r2) and (p1 == p) and (q1 == q) then r1 else fail



type Ctx = [Theorem] -- List of true theorems
type Tactic = Ctx -> Theorem -> (Ctx, [Theorem]) -- subgoals produced

-- state + List
intro ctx (Implies a b) = (a:ctx,  [b]) 
intro ctx x = (ctx, [x]) -- otherwise ignore the command



proofinterp :: [goals] -> [tactics] -> [goals]

