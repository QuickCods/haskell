 --a
 factorial 1 = 1
 factorial x = x * (factorial (x-1))

 --b
 rangeProduct n m | n > m = n * (rangeProduct (n-1) m)
                  | otherwise = m

 --c
 fact n = rangeProduct n 1
