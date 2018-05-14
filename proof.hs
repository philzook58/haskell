
data PropFormula = Atomic String | Disj PropFormula | Conj PropFormula | Not PropFormula


impl a b = (Not a) Conj b


-- Goals,  
type ProofState = (PropFormula, )

implI goal  = 

disjI (((Disj a b):goals), assumpt) = (a:b:goals, assumpts)

type Goal = PropFormula
type Assumptions = [PropFormula]
type ProofState = (Goal, Assumptions)
type ProofStack = [ProofState]

conjI :: ProofState -> ProofStack
conjI ((Conj a b), assumpt) = [(a, assumpt), (b, assumpt)]

data Picker = Left | Right

disjI Left (Disj a b, assumpts) = [( a, assumpts)]
disjI Right (Disj a b, assumpts) = [( b, assumpts)]



conjE Left (g, (Disj x y) : assumpts) = (g, x : assumpts)
conjE Left (g, (Disj x y) : assumpts) = (g, y : assumpts)



-- pop goal off of stack
qed ((x,y):gs) = if x == y then gs


type TermProof = (PropFormula , ProofTree)
data ProofTree = DisjI TermProof | ConjI TermProof TermProof | 


-- There is nonlocality in implI A->B for example. The top of the proof must be assumption of A 



checkProof' :: PropFormula -> ProofTree -> Bool
checkProof' goal ()

-- Relatively straightforward. Not clean though. 
-- Nondeterminisitc State monad
-- \s -> [(s,a)]
-- makes divierging goals goes in the list

-- what is the state. what is the response?
-- state is assumptions and goals?
-- Assumptions or Context
type ProofM = NDState (Goal, Assumptions)
-- with also a EitherT on it for errors and IO for printing

data BelowLine = (|-) Assumpts Goal

implE :: a -> (a -> b) -> b
-- implI ~ \x -> ...
implI :: ProofTree a b -> (a -> b) 

conjI :: a -> b -> (a, b)
conjE1 = (a,b)->a


-- There must surely be equivalence between different proof systems
-- But we'd have to lift and lower into and out of the type system



-- Tree Fragments
-- Trees building up from leaves
-- and trees building from the roots
-- and there is a blank space that can connect evntually.

data TreeEnd a = Node TreeEnd TreeEnd | Leaf a





