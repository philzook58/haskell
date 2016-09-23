
-- simplest zipper is a list


data ZipList a = [a] a [a]

left (ZipList x:xs y zs) = ZipList xs x y:zs
right (ZipList xs y z:zs) = ZipList y:xs x zs

inspect (ZipList xs y zs) = y
replace a (ZipList xs y zs) = ZipList xs a zs
push a (ZipList xs y zs) = ZipList xs a y:ys 
pop (ZipList xs y z:zs) = (y, ZipList xs z zs)
