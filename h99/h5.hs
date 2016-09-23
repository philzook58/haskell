myReverse [] = []
myReverse x = last x : (myReverse init x)

myReverse'' = reverse
