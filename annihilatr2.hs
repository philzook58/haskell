

data Op = Is Op -> Op | Nil
-- overload show

-- Purest form. Very clean. But can you make the algorithm actually stop? 
a :: Op -> Op


-- Monadic variant
-- If you have a transformation do it and return succedd. If you don't call lower operators. if they fail, also fail
a :: Op -> Error Op

-- isn't there something like this in parsers? you try all the possible parsing rules and bubble up failure

-- Coninuation passing style. Maybe uphill should be asingle Op
-- array form seems like it allows more lookahead. Or lookbehind
-- similar feeling to unwinding a tree and then rebuilding it.
a:: ([Op], [Op]) -> ([Op], [Op]) 
a uphill downhill 




a x = if x == adag then opsum ident (adag a) else donothing


-- possible combination order
combo = (((a adag) a) adag)