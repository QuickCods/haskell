--a)

 somapares :: [(Int,Int)] -> [Int]          --admissivel
 --(geral) Num a => [(a,a)] -> [a]
 somapares [] = []
 somapares ((x,y):xs) = (x+y):somapares xs

--b)

 somapares' xs = [x+y | (x,y) <- xs]
