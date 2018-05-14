{-# LANGUAGE ExistentialQuantification #-}

data Box = forall a. BoxC a
-- (a , a)
-- BoxC :: * -> Box

-- need that last bit so I can actually see anything
data Operable = forall a. Constructor a (a -> a) (a -> String)
-- corresponds to the dependent pair (a, (a, (a->a), a-> String))
-- Constructor is of type * -> (* -> *) -> Operable

useop :: Operable -> Operable
useop (Constructor x f g) = Constructor (f x) f g
showme (Constructor x f g) = g x

--showme (useop (Constructor 3 (+3) show))


-- yeah. It really restricts what you can do to it. Kind of cool
-- There isn't a typeable way to extract from it?
