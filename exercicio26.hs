 divprop :: Int -> [Int]                                                -- exercicio 24
 divprop n = [x | x <- [1..(n-1)], (n `mod` x) == 0]

 primo :: Int -> Bool
 primo x | (length (divprop x)) > 1 || x < 2 = False
         | otherwise = True
