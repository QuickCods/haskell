 crescente :: Ord a => [a] -> Bool
 crescente [] = True
 crescente [x] = True
 crescente (x:y:ys) = (x <= y) && crescente (y:ys)

 crescente' xs = and [x <= y | (x,y) <- zip xs (tail xs)]

 partes 0 = [[]]
 partes n = [x:xs | x <- [1..n], xs <- partes (n - x), crescente (x:xs)]
