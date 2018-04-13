 divprop :: Int -> [Int]                                                    -- exercicio 24
 divprop n = [x | x <- [1..(n-1)], (n `mod` x) == 0]

 perfeitos :: Int -> [Int]
 perfeitos n = [x | x <- [1..n], (sum (divprop x)) == x]
