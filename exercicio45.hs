 -- a)
 
 merge :: Ord a => [a] -> [a] -> [a]
 -- merge [] [] = [] é desnecessário, o que vem a seguir faz isso
 merge l1 [] = l1
 merge [] l2 = l2
 merge (x:xs) (y:ys) | x <= y = x:merge xs (y:ys)
                     | y < x = y:merge (x:xs) ys
                     
 -- b)
 
 metades :: [a] -> ([a],[a])
 metades [] = ([],[])
 metades (x:xs) = (l1,l2)
                where
                	(ll1,ll2) = metades xs
                	l1 = x:ll2
                	l2 = ll1

 {-alternativa
 metades' :: [a] -> ([a],[a])
 metades' l = (l1,l2)
 	  where
		n= div (length l) 2
		l1= take n l
		l2= drop n l
 -}

 msort :: Ord a => [a] -> [a]
 msort [] = []
 msort [x] = [x]
 msort l = merge (msort l1) (msort l2)
         where
         	(l1,l2) = metades l
