 --a)

 quadrados :: [Int] -> [Int]
 quadrados [] = []
 quadrados (x:xs) = x^2:quadrados xs
 --b)

 quadrados' xs = [x^2 | x <- xs]
