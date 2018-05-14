
eqelim : {A: Type} -> {C : A -> A -> Type} -> (x = y) -> (C y y) -> (C x y)
eqelim Refl x = x 

transeq : {A:Type} ->  {x:A} -> (x = y) -> (y = x)
 
transeq {A} Refl = eqelim {A} Refl Refl

transeq2 : (x=y) -> (y=x)
transeq2 Refl = Refl

--transeq3 : (x=y) -> (y=x)
--transeq3 prf = replace ?what1 ?what2

elim2 : {A:Type} ->{x:A}-> {y:A} -> {C: A -> A-> Type} ->(x=y) -> (C x x) -> (C x y)
elim2 prf y = replace prf y 

symeq :{A:Type}  -> {x:A}-> {y:A} ->(x = y) -> (y = x)
symeq {A} {x} {y} prf = elim2 {A} {y} {x} ?what Refl
