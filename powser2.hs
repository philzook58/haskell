

data FixFun f a = FixF a (FixFun f (f a))
--data FreeFun f a = FixF (FixFun f (f a))
-- Fix ~ Stream
-- Free ~ List
-- combo of two monoid, plus and times, and plus and compose




