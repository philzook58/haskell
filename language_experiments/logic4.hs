-- Dependant types

data MyTypes = Universe Int | Forall MyTypes MyTypes | TUnit | DepUnit Term | TVar Int -- | ThereExists MyTypes MyTypes
-- dependent unit is a unit type that depends on a value
-- It allows the universes to at least have things in them
-- TUnit and DepUnit kind of feel like Natural Numbers

-- If I lost DepUnit, I'm talking polymorphism rather than dependant types.
-- It is important to understand gradiation of power.


-- Do I need The Type possiblity?
data Term =  Var Int | Lam MyTypes Term | Ap Term Term | Unit MyTypes -- | Type MyTypes -- | NatVal Int

type Context = [MyTypes]


typechecktype TUnit = Universe 0
typechecktype (Universe n) = Universe (n + 1)
typechecktype (Forall t1 t2) = Universe (max n m) where Universe n = typechecktype t1, Universe m = typechecktype t2 
typechecktype (DepUnit t) = typechecktype $ typecheckterm t -- and add 1 to that universe
typechecktype TVar

typecheckterm 



-- A family. Take a parameter of type Tunit to a type of
family1type = Forall TUnit (Universe 0) 
family1 = Lam TUnit (Type TUnit)

--typecheck gamma family1 = TFunc TUnit (Universe 0)

pifunctiontype = Forall TUnit (DepUnit (Var 0))
pifunctioninstance = Lam TUnit (Unit (DepUnit (Var 0)))


-- big problems here. Pass in type to function. Curried. 
-- TVar in type if paramtrtically typed. Var in type if depednetaly typed
idfunction = Lam (Universe 0) (Lam (Var 1) (Var 0))
idfunctiontype = Forall (Universe 0) (ForAll (TVar 1) (TVar 1))

unituniverse 0 = Unit TUnit
unituniverse n = Unit (Depunit unituniverse (n - 1))

apid1 = Ap idfunction (Unit TUnit)
apid2 = Ap idfunction unituniverse 1
--TFunc and Forall should be the same dif.  So i removed TFunc


