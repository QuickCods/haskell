{-
a)
4
b)
[]
c)
22
d)
[True,True,True,False,False]
e)
26                --??
f)
[(2,1),(3,2),(4,3)]
g)
[(2*x-1,2^x) | x <- [1..]]
h)
[2,4,2,10,12]          --acho eu
i)
[Bool,Bool]            --acho eu
j)
funcao :: Num c => c -> c       --acho eu
k)
data Arv = Folha a | No (Arv a) (Arv a)           --acho eu
l)
f :: [a] -> Int             --maybe
-}

-- 2 a

 nafrente :: a -> [[a]] -> [[a]]
 nafrente x l = [x:p | p <- l] 
 -- nafrente c xs = map (c:) xs

-- b

 ocorreN :: a -> [a] -> Int -> Bool
 ocorreN x|n = length (filter (==x) l) == n
 -- ocorreN x|n [1 | y <- l, y == x] == n

-- 3 a

 subs :: [a] -> [[a]]
 subs [] = [[]]
 subs (x:xs) = map (x:) sxs {- [x:l | l <- sxs] -} ++ sxs
            where sxs = subs xs

-- b

 subsAsc :: Ord a => [a] -> [[a]]
 subsAsc [] = [[]]
 subsAsc (x:xs) = [x:l | l <- sxs, null l || x < head l] ++ sxs
               where sxs = subsAsc xs

-- 4

 soma_aux :: Int -> IO Int
 soma_aux n = do x <- getLine
                 if (read x) == 0 then return n else soma_aux (n + (read x))

 soma :: IO ()
 soma = do n <- soma_aux 0
          putStrLn (show n)

-- 5 a

 data ArvT a = Folha a | No (ArvT a) (ArvT a) (ArvT a)

 arv :: ArvT Int
 arv = No (Folha 1) (No (Folha 4) (Folha 5) (Folha 8)) (Folha 9)

-- b

 nelementos :: ArvT a -> Int
 nelementos (Folha _) = 1
 nelementos (No e c d) = nelementos e + nelementos c + nelementos d

-- c

 mapTree :: (a -> b) -> ArvT a -> ArvT b
 mapTree f (Folha v) = Folha (f v)
 mapTree f (No e c d) = No (mapTree f e) (mapTree f c) (mapTree f d)

-- 6

 scanl' :: (b -> a -> b) -> b -> [a] -> [b]
 scanl' f v xs = v:zipWith f (scanl' f v xs) xs

-- 7 a

--Por indução sobre t
--caso base t = Folha x

 nelementos (Folha x) = 1 = 2*1-1      --*check*

--caso indução t = No e c d
{- hipotese:nelementos e = 2*n1 - 1
            nelementos c = 2*n2 - 1
            nelementos d = 2*n3 - 1 -}

 nelementos (No e c d) = nelementos e + nelementos c + nelementos d

 2*n1 - 1 + 2*n2 - 1 + 2*n3 - 1 = 2*n1 + 2*n2 + 2*n3 - 2 -1 = 2(n1 + n2 + n3 - 1) -1 = 2*n4 - 1
