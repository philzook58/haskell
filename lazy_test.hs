


-- Can't show the function part. Need custom implementation of show
data FuncOrInt = Actual Int | Almost (FuncOrInt -> FuncOrInt) FuncOrInt


-- I wanted sort of to use the built in function strcture to repsresent expressions
-- operaotrs take states to states
--Maybe what I really want is a lisp macro system

-- Doing this way only gives us plus 1 lookahead? No, actually we can go quite far ahead.
-- Its just that the syntax blows

-- Maybe this is an Either State (State -> State)
--No THAT is one lookahead.

-- Or Either State (Either State -> State) .. No to go further

-- MaybeState = Actual State | ToBe (MaybeState -> MaybeState) MaybeState 

f (Actual 3) = (Actual 4)
f (Almost friend b) = friend (f b)

friend (Actual 4) = Actual 1

strip (Actual b) = b


-- strip (f (Almost friend (Actual 3)))


-- what i kind of want is like a parser. Take expression, de tree it, and then write a parser That makes a tighter or different
--parse. On the face of it, it feels like parsers ought to be expoential too.

-- what we could have is lowest parts of a tree could tell higher parts what they can be
-- or top of tree could send requests down tree



