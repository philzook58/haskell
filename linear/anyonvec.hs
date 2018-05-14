
-- Ok. New idea.
-- I've mentioed before that the simplifcation stage of an anyon diagram corresponds to beta reduction

-- The three legged object comes in two flavors

-- using function vectors


v1 :: a -> b -> Vec c Float

v2 :: c -> Vec (a,b) Float

-- These are the vectors that specify specific examples

-- suggesition we could also push the covraint arguments back into the doubled contravaint position
-- going to a cps style

v2' :: (a -> b -> c) -> Float

-- combined mutiple guys could look like this maybe?
-- the parametric nature of d maens that reallly the first function is going to be plugged into the next one.
v3 :: forall d. (a -> b -> d) -> (d -> c -> e) -> Float


-- define a dot that rearranges stuff