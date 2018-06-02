-- FOLDR

 somatodos :: [Int] -> Int
 somatodos [] = 0
 somatodos (x:xs) = f x (somatodos xs)
             where f x y = x + y

 todospar :: [Int] -> Bool
 todospar [] = True
 todospar (x:xs) = f x (todospar xs)
             where
               f x y = if x `mod` 2 == 0
                       then y
                       else False

 colalistas :: [[a]] -> [a]
 colalistas [] = []
 colalistas (l:ls) = f l (colalistas ls)
             where
               f x y = x ++ y

 repetedireq :: (a -> b -> b) -> b -> [a] -> b
 repetedireq f y0 [] = y0
 repetedireq f y0 (x:xs) = f x (repetedireq f y0 xs)

 colalistas' l = repetedireq f [] l
             where
               f x y = x ++ y

 colalistas'' l = foldr f [] l
             where
               f x y = x ++ y
