 metades :: [a] -> ([a],[a])
 metades l = (esq,dir)
            where
            	 n1 = length l `div`2            --metades = (take (length l `div` 2) l, drop (length l `div` 2) l)
            	 esq = take n1 l
            	 dir = drop n1 l
