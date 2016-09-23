

-- S -> ( T )
-- T -> ( T ) | () T | ()

import Text.ParserCombinators.Parsec


data Parens = Left Parens | LeftRight Parens | LeftRightFinal

leftparen :: Parser Char
leftparen  = char '('
rightparen :: Parser Char
rightparen = char ')'
