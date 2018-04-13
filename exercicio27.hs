 pascal :: Int -> [[Int]]
 binom n k = div num den
         where
           num = product [1..n]
           den = (product [1..k]) * (product [1..(n-k)])
 pascal n = [[binom x y] | x <- [0..n], y <- [0..x]]
