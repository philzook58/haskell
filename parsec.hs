
import Text.Parsec

matchtrue :: Parsec String
matchtrue = string "true"

symbol :: Parsec Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"