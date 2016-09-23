import Test.QuickCheck

quickCheck ((\s -> s == s) :: [Char] -> Bool)

