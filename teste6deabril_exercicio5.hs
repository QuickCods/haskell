 --a)

 itera :: Int -> (a -> a) -> a -> a
 itera 0 f v = v
 itera n f v | n > 0 = f (itera (n-1) f v)

 --b)

 mult :: Int -> Int -> Int
 mult m n = itera m (+n) 0
