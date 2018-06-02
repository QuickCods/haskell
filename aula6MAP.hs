 -- MAP

 soma1 :: [Int] -> [Int]
 soma1 [] = []
 soma1 (x:xs) = (f x):soma1 xs
         where f x = x +1

 pares :: [Int] -> [Bool]
 pares [] = []
 pares (x:xs) = (f x):pares xs
         where f x = x `mod` 2 == 0

 aplicaatodos :: (a -> b) -> [a] -> [b]
 aplicaatodos f [] = []
 aplicaatodos f (x:xs) = (f x):aplicaatodos f xs

 pares' l = aplicaatodos f l
         where f x = x `mod` 2 == 0

 pares'' l = map f l
         where f x = x `mod` 2 == 0
