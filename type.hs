-- So

-- what is the simplest problem in types I can code up

data Type = Int | String | Func Type Type | Or Type Type | And Type Type

typeCheck
