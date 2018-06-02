--53

 shift :: [a] -> [a]
 shift [] = []
 shift (x:xs) = xs ++ [x]

 rotateaux :: [a] -> [a] -> [[a]]
 rotateaux l [] = [l]
 rotateaux l (x:xs) = f x (rotateaux l xs)
 			where
 				f x y = (shift (head y)) : y

 rotate :: [a] -> [[a]]
 rotate l = rotateaux l (tail l)

 rotate' l = foldr f [l] [1..(length l)-1]
 			where f x y = (shift (head y)) : y
