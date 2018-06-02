{-
[n0,n1,n2,...] -> [0,n0,n0+n1,n0+n1+n2]
-}

 somas :: [Int] -> [Int]
 somas l = 0:zipWith (+) (somas l) (l)
