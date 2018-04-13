 g :: (Integer -> Integer) -> Integer -> Integer
 g f 0 = f 0
 g f n = (f n) + g f (n-1)
