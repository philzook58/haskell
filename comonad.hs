
--also known as env
import Control.Comonad

data CoReader env val = CoReader env val deriving Show

extract (CoReader env val) = val
extend :: (CoReader a b -> b) -> CoReader a b -> CoReader a b
extend f chunk@(CoReader env val) = CoReader env (f chunk)


f (CoReader n m) = m + n

start = CoReader 3 0
val = extend f $ extend f start
-- 6
-- yes it passes along the 3 and uses it.

data Stream a = Cons a (Stream a)

-- I waslead around to it has to be this. But this is kind of funky
-- the first value has access to everything
-- the next has access to everything minus the head.
-- It feels wrong for causality. It's more like everyhitn only depends on the future (f you consider stream = now:future)
-- I guess that is because I associate the forward time with pulling off the stream.
-- But I need to ssociate it with pushing on the stream.
-- exactly anti-causal

-- if you Cons a past
--step a past = Cons a past
step = Cons
-- and then extend it, e =>> f.
-- it sucks that you have to extend every time you add. 

instance Comonad (Stream a) where
	extract :: Stream a -> a
	extract Cons x _ = x  
	extend :: (Stream a -> a) -> Stream a -> Stream a
	extend f s@(Cons x r) = Cons (f s) (extend f r)


-- comonaic values are consumed
-- comonadic reader

-- monadic values are produced. reader monad produces reader functions


-- Causal Streams
-- relative neighborhoods versus absolute neighborhoods

-- Can I use comonad to perform location based pattern matching

