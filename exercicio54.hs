-- a)

 lmax :: Ord a => [a] -> a
 lmax (x:xs) = lmaxacc x xs
 		where
 			lmaxacc acc [] = acc
 			lmaxacc acc (y:ys) = lmaxacc (max acc y) ys

 lmax' l = foldl max (head l) (tail l)

 lmax'' l = foldl1 max l

 rmax :: Ord a => [a] -> a
 rmax [x] = x
 rmax (x:xs) = max x (rmax xs)

 rmax' l = foldr max (last l) (init l)

 rmax'' l = foldr1 max l
