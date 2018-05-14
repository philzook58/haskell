Functor Vector

The basic idea is that $v\otimes$ is a functor. By composition of many of these functors, one can build big states.
The inspiration comes from String Diagrams.
At the root, This may be because Functors form a Monoidal Category under composition

Obviously I should parametrize over Float to more numbers, but it is a good enough stand-in.

> {-# LANGUAGE RankNTypes #-}
> import Data.Functor.Compose
> import Data.Functor.Product
> import Control.Monad.Free

> data Spin a = Spin Float a Float a 

> instance Functor Spin where
>	fmap f (Spin x a y b) = Spin x (f a) y (f b)

A ListVec is the simplest kind of vector. 

> type ListVec basis kron = [(Float, basis, kron)]

Very similarly you can make a function vector. Or a MapVector
Vectors are represented by the type of their base.

> type FunVec basis kron = basis -> (Float, kron)




One can build bigger states by functor composition

> type Kron f g = Compose f g

> type Qubit2 = Kron Spin Spin
> type Qubit4 = Kron Qubit2 Qubit2

Direct Sums can be formed using the Functor Product

> type DSum f g = Product f g

We're using a product of sums form. What about sum of products?

Now, to form big vectors, we should use sharing to avoid expoential blowup
What is nice about this is we don't have to manually densify any kronecker products
When needed they will break the pointer structure themselves. 
What is very bad about this is that it is easy to screw this up.
One has to end the Functor chain somehow. 
Both Unit and Float seem like a natural end

> vectorexample = let v0 = Spin 1.0 () 0.0 ()
>                     v1 = Spin 1.0 v0 0.0 v0 
>                 in v1

When we fmap in, is it smart enough to not do it twice? I'm not sure.

In a sense Free is like List. Pure allows us to end the chain of Krons

> type Qubits = Free Spin

> zerovec 0 = Pure ()
> zerovec n = let v = zerovec (n-1) in Free $ Spin 1.0 v 0.0 v

Oooh. This is a problem. I need to be polymorphic over the number of functor applications
Possibly use Nat? For fixed size Qubits
zerovec 0 = Spin 1. () 0. ()
zerovec n = let v = zerovec (n-1) in Spin 1.0 v 0.0 v


The representable type is something like a log. It'd be nice to be able to take the exponential

> data Bit a = Zero a | One a
> type Exp f a = a -> f a -- no this isn't right
> data Scalar a = Scalar Float a
data Quantize f = Compose Scalar (Exp f)

Matrix operations take the form of Natural Transformations. This naturality expresses the obilbiousness
to what happens to the right. This form unfortunately does not constrain you in a type safe
way to linear operations. You may unwisely choose to perform nonlinear operations

> type LinOp v = forall a. v a -> v a


There is something off about this. Shouldn't Dual Vec also compose? As a wild guess,
shouldn't it be contravariant functor? Is this actually Cup?

> type Dual v basis kron = v basis kron -> (Float, kron)

The dot product is an end. There is nothing you can do with it except plug the vector into it's dual

> type Dot v kron = forall basis. (Dual v basis kron, v basis kron) 

Recursive Dot? 
is Dot Cup?

This might be the same as adjoint. Really this means that they are inverse functors.

> class Dottable f where
>    cup :: f f a -> a
>    cap :: a -> f f a

This is basically and adjoint relationship. We could perhaps insist that 
Dagger is different from the other. Use phantom type parameter to differentiate dag from reg?
Since they are disctinct and conversion requires complex conjugation

> newtype Conj f a = Conj (f a)

instance Adjoint (Conj Spin) Spin  

-- this is a natural transformation that can end a string

> type Dual' f = forall a. f a -> a

Similalrly a -> f a puts in a  new functor
This has the type signature of pure.
But maybe it's more like f () -> a -> f a
Since we need the data inside f to build it. If we use a canonical |0> vector maybe

Traverse gives us the ability to swap
fmap .fmap .fmap to index  inwards

if These single paramter functors are somewhat like lists, we could construct zippered lists.
a 2 type parameter family. The reversed list in one direction

Hughes Lists 

Profunctor vector keeps negative position also? Seperately?

