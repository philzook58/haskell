{-# LANGUAGE TypeOperators #-}
import Data.Void
import Data.Type.Equality


type Not a = a -> Void

type Conj a b = (a,b)
type Disj a b = Either a b
type Impl a b = a -> b



-- Gamma, the context. Is that the implicit context of the interpeter ghci?

conjelim1 conj = fst 
conjelim2 conj = snd

disjintro1 = Left
disjintro2 = Right 

disjelim f g (Left a) = f a
disjelim f g (Right b) = g b


data Cats = Millie | Willie | Silly
data Meow = Softmeow | Purr | Loudmeow
type CatsMeow = Cats -> Meow-- if cat implies meow
--type DogsBark

-- implication elimination
implelim :: Impl a b -> a -> b 
implelim f x = f x


--proofofdoublenotLEM :: Not (Not (Disj a  (Not a))) -- the type and proof of the oduble negated form of double negation
-- unfold definition a bit
--t1 :: Not (Disj a  (Not a)) -> Void

t2 :: ((Disj a  (Not a)) -> Void) -> Void
t2 f = f (Right (\a -> f (Left a))) 

-- is a way of saying let's say you have a way of handing me a.
-- Then I can apply f to get Void
-- the trick is that I have an a in both covariant and contravariant positions. I'm being handed a and producing a.
-- I can wire these two into each other.
-- i bet this pattern exists under another guise somewhere.
-- I can't touch void so it really doesn't matter
tricky :: (Either a (a->b) -> b) -> b
tricky f = f (Right (\a -> f (Left a))) 

-- Djinn got this immediately correct. Pretty cool.

-- suppose a is true is a way of putting a function on the other side? supposing a is asssuming you'll give me a.




--t3 f = f Right (nota)
--       	   where nota a = f (Left a) 
--
--suppose a = 

-- suppose not a is true
-- then nx is a function from type a -> Void


-- Huh


--weakeneing
-- if Gamma |- B true then A true |- B true
-- so If I have named a value of B in context
-- then I can make a dumb weak functio for any A that tosses away

weaker :: a -> Int
weaker _ = 3


-- Kind of comonadic
-- A or B implies that 
--  nah this is dumb
eitherTheorem :: (Either a b) -> Either (Either a b -> a) (Either a b -> b)
eitherTheorem (Left x) = Left f where f (Left y) = y
                                      f (Right _) = x
eitherTheorem (Right x) = Right f where f (Right y) = y
                                        f (Left _) = x



-- an idea on how to fake some aspects of dependent types
-- I need ancillary definitions of PTrue and PFalse
-- this is muddying the waters. That's probably not a good idea.
data PBool = PTrue | PFalse deriving Show
type PTrue = ()
type PFalse = ()

true_eq :: PTrue :~: PTrue
true_eq = Refl
false_eq :: PFalse :~: PFalse
false_eq = Refl

pand :: PBool -> PBool -> PBool
pand PTrue PFalse = PFalse
pand PTrue PTrue = PTrue 
pand PFalse _ = PFalse 

and_theorem :: Void-- for all a, pand a PFalse = PFalse. 
and_theorem = undefined


data Nat = Z | Succ N




