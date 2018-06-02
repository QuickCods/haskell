 power2 = 1:map (2*) power2
 power3 = 1:map (3*) power3
 power5 = 1:map (5*) power5

 merge :: Ord a => [a] -> [a] -> [a]
 merge (x:xs) (y:ys) | x <  y = x:merge xs (y:ys)
 					 | x == y = x:merge xs ys                   --merge sem repetidos
 					 | x >  y = y:merge (x:xs) ys

 power2e3 = 1:merge (map (2*) power2e3)
                    (map (3*) power2e3)

 merge3 l1 l2 l3 = merge l1 (merge l2 l3)

 hamming = 1:merge3 (map (2*) hamming)
                    (map (3*) hamming)
                    (map (5*) hamming)
