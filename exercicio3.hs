 area :: (Num a, Fractional a, Floating a) => a -> a -> a -> a
 area x y z = sqrt (p * (p-x) * (p-y) * (p-z))
          where p = (x + y + z) / 2
