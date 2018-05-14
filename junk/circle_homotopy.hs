data Circle = A | B | C
-- Make a circle with Three points
-- types = spaces
-- values = points

-- Paths are axiomatic paths and seqeunces of paths
-- dependant typing would be nice here?
data AxiomPaths = AB | BC | CA | AC | CB | BA
data Path = Seq [AxiomPaths] | Axiom AxiomPaths

-- agda
data CirclePath : (a : Circle ) -> (b : Circle) -> CirclePath a b
    null : (a : Circle) -> CirclePath a a -- yeaaaa. Wait so data constructors are axiomish?
    connect : CirclePath a b -> CirclePath b c -> CirclePath a c
    sym : CirclePath a b -> CirclePath b a
    AB : CirclePath A B -- These are "true" AB is a value of CirclePath A B
    BC : CirclePath B C
    CA : CirclePath C A
data Path : (a : Type ) -> (b : Type) -> Path a b

