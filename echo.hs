

-- if == "stop" then stop otherwise keep echoing

test "stop" _ = return ()
test str func = func 



loop = do
	x <- getLine
	putStrLn x
	test x loop



loop2 = do
	x <- getLine
	putStrLn x
	case x of
		"stop" -> return ()
		_ -> loop2

main = loop2






{-
main :: IO ()
main = do 
	x <- getLine
	putStrLn "hello"
	putStrLn x
	-}

