


-- We could easily? codify the Feynman rules to construct the k space guys
-- the difficulty is 

-- 
-- writing production of diagrams as some kind of recursion scheme

-- Writing the basic datatype of diagrams

data DiagramF a b c d = Vertex a b c d | Prop a b | External a 

-- Kind of sucky. I need to direct my propagators.

-- DSL? Feynman Combinators?
-- piponis cup and cap. could use state monad... current state is current feynman function and mapping from indices to 
-- argument places


-- Leave integration strategy factored out until later

-- integrate f curries
integrate f strategy = strategy f
-- this is the same as 
integrate = swap ($)

-- how to specify which variable of f needs to be integrate out?


