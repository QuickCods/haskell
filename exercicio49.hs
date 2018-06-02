-- a)

 (+++) :: [a] -> [a] -> [a]
 (+++) [] l = l
 (+++) (x:xs) l = x : (+++) xs l
             where f x y = x : y

 (++++) :: [a] -> [a] -> [a]
 (++++) l1 l2 = foldr f l2 l1         -- foldr (:) l2 l1
             where f x y = x : y

-- c)

 myreverse :: [a] -> [a]
 myreverse [] = []
 myreverse (x:xs) = f x (myreverse xs)
             where 
             	 f x y = y ++ [x]

 myreverse' :: [a] -> [a]
 myreverse' l = foldr f [] l
             where 
             	 f x y = y ++ [x]
               
 -- d)

 lreverse :: [a] -> [a] -> [a]
 lreverse acc [] = acc
 lreverse acc (x:xs) = lreverse (f acc x) xs
 			where f acc x = x:acc

 lreverse' l = foldl (\acc x -> x:acc) [] l

 lreverse'' l = foldl f [] l
 			where f acc x = x:acc
