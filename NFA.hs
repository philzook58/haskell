



type Action = Int
data State1 = P | Q deriving Show
type NFA = Action -> State1 -> [State1]



nfa1 :: NFA
nfa1 0 P  = [P]
nfa1 1 P  = [P,Q]
nfa1 _ Q  = []

start1 = [P]

-- NFA that recognizies strings of 0 and 1 that end in 1

runNfa :: State1 -> NFA -> [Action] -> [State1]
runNfa state _ [] = return state
runNfa state nfa (x:xs) = do 
       state' <- nfa x state
       runNfa state' nfa xs


runNfa' initstates nfa actionlist = initstates >>= (\state -> runNfa state nfa actionlist)

accept1 P = False
accept1 Q = True

 