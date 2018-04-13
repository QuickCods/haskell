 fact n = product [1..n]
 binom n k = div num den                                 -- a)
            where
            	num = fact n
            	den = (fact k) * (fact(n-k))
 binom_rapid n k = div num den                           -- b)
                  where
                  	num = product [n-k+1..n]
                  	den = product [1..k]
