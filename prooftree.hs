-- The proof tree is upside down

data ProofTree = ImplI |
				 ImplE Theorem Theorem ProofTree ProofTree|
				 Goal Theorem ProofTree |
				 Goal2 Theorem Gamma ProofTree |
				 AndI Theorem Theorem ProofTree ProofTree| -- The proofs of the indiviual pieces of the and
				 AndE Theorem ProofTree | -- The theorem from the context you are anding with
				 Assume Theorem |
				 AndI2 Int Int ProofTree ProofTree| -- choose two guys in the context to and together
				 Truth |
				 Assume2 Int |
				 Assume3 |-- Is this how it would work? 
				 VarIntro |
				 Hole

data Theorem = Impl Theorem Theorem | Prop String | And Theorem Theorem | Or Theorem Theorem | Not Theorem

type Gamma = [Theorem]

checkProof :: Gamma -> ProofTree -> Bool
checkProof gamma Assume = True
checkProof [] _ = True ?
checkProof [(Prop _)] VarIntro = True
checkProof _ VarIntro = False
checkProof xs AndI2 =  


checkProof' theorem proof = checkProof [theorem] proof