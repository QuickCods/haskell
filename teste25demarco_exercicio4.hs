 enquantoPar :: Integral a => [a] -> [a]
 enquantoPar [] = []
 enquantoPar (x:xs) | x `mod` 2 == 0 = x:enquantoPar xs
 				          	| otherwise = []
