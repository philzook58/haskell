
--data HOAS a = Lam (a -> a) |	App (HOAS a) (HOAS a) | Unit
data Expr = Lam (Expr -> Expr) | App Expr Expr | Unit


--lam :: (a -> a) ->  Expr
--lam = 

data Value = Fn (Value -> Value)
unFn (Fn x) = x



--eval :: Expr -> Expr
--eval (App (Lam f) b) = f (eval b)
--eval (Lam f) = Lam f
--eval Unit = Unit

eval :: Expr -> Value
eval (Lam x) = Fn (eval . x . uneval)
eval (App y z) = unFn (eval y) (eval z)

uneval :: Value -> Expr
uneval (Fn x) = Lam (uneval . x .eval)

-- converison between a brujin index form and hoas form should be possible i feel

idh = Lam id
diverge = App (Lam y) (Lam y) where y = \x -> (App x x) 

simpleprog = App idh Unit

data PHOAS a = Varp a | Appp (PHOAS a) (PHOAS a) | Lamp (a -> PHOAS a)

idp = Lamp (\x -> Varp x)

