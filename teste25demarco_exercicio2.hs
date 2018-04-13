 numEqual :: Eq a => a -> a -> a -> Int               --ou  numEqual :: Int -> Int -> Int -> Int
 numEqual n m p | n == m && m == p = 3
 	            | n == m || m == p || n == p = 2
 	            | otherwise = 0
