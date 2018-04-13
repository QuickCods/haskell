 k :: Integer -> (Integer -> Integer) -> Integer
 k 0 f = f 0
 k n f = max (f n) (k (n-1) f)
