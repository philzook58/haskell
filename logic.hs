
-- https://www.olivierverdier.com/posts/2015/10/09/type-logic-haskell/

type a :&: b = (a,b)
type a :+: b = Either a b

reflexivity :: a -> a

reflexivity x = x
-- this is id
andCommut :: a :&: b -> b :&: a
andCommut (x,y) = (y,x)
-- swap


modusPonens :: (a -> b) :&: a -> b
modusPonens f x = f x

--this is apply


contramap :: (a -> b) -> Not c b -> Not c a
contramap f nb = nb . f


class Absurd c a where
    absurd :: c -> a

type Pot c a = Not c (Not c a)

-- conituation monadd?