--49)a)

 (+++) :: [a] -> [a] -> [a]
 (+++) [] l = l
 (+++) (x:xs) l = x : (+++) xs l
             where f x y = x : y

 (++++) :: [a] -> [a] -> [a]
 (++++) l1 l2 = foldr f l2 l1         -- foldr (:) l2 l1
             where f x y = x : y

--49)c)

 myreverse :: [a] -> [a]
 myreverse [] = []
 myreverse (x:xs) = f x (myreverse xs)
             where 
             	 f x y = y ++ [x]

 myreverse' :: [a] -> [a]
 myreverse' l = foldr f [] l
             where 
             	 f x y = y ++ [x]
