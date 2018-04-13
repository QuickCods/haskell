 maiores :: Ord a => [a] -> [a]
 maiores [] = []
 maiores [x] = []
 maiores (x:y:ys) | x >= y = x:maiores(y:ys)
 				  |otherwise = maiores (y:ys)
