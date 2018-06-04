-- exame de recurso

-- 3a

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

 soma :: IO()
 soma = soma_aux 0
       where soma_aux n = do x <- getLine
                             let y = read x
                             in if y == 0 then
                                do putStrln (show n)
                                  return ()
                                else soma_aux (n+y)

-- soma_aux :: Int -> IO Int
-- putSrtln :: String -> IO()

{-
 soma = do n <- soma_aux 0
 soma = soma_aux 0
       where soma_aux n = do x <- getLine
                             let y = read x
                             in if y == 0 then
                                   return ()
                                else soma_aux (n+y)
-}

-- 6

 scanl :: (b -> a -> b) -> [a] -> [b]
 scanl f v xs = v:zipWith f (scanl f v xs) xs

-- 5 a

 data ArvT a = Folha a | No (ArvT a) (ArvT a) (ArvT a)

 arv :: ArvT Int
 arv = No (Folha 1) (No (Folha 4) (Folha 5) (Folha 8)) (Folha 9)
 ---------------------------------------------------------------
 data Arv a b = Folha a | No b (Arv a b) (Arv a b)

-- b

 nelementos :: ArvT a -> Int
 nelementos (Folha v) = 1
 nelementos (No e c d) = nelementos e + nelementos c + nelementos d

-- c

 mapTree :: (a -> b) -> ArvT a -> ArvT b
 mapTree f (Folha v) = Folha (f v)
 mapTree f (Folha e c d) = No (mapTree f e) (mapTree f c) (mapTree f d)

-- exame

-- 3 a

 timesMat m1 m2 = [[if any (== 1) (zipWith (*) (m1!!l) (map (!!c) m2)) then l else 0 | c <- [0..n]] | l <- [0..n]]
               where n = length m1 - 1

-- b

 menor m1 m2 = and [and [ (m1!!l!!c) <= (m2!!l!!c) | c <- [0..n]] | l <- [0..n]]
              where n = length m1 - 1

-- 1 h

 60

-- 5

 inff :: Int
 inff = 0:zipWith (+) inff [1..] [0,3,6..]

 inff' = [ (i * (i+1)) `div` 2 | i <- [0..]]

-- 1 j

 [(!!2),length] :: [[Int] -> Int]

 ___________


 -- 46

 bits :: Int -> [[Bool]]
 bits 0 = []
 bits n = [x :l | x <- [False,True], l <- bits (n-1)]

 -- 47 

 permutacoes [] = [[]]
 permutacoes (x:xs) = [ps | p <- permutacoes xs, ps <- distribuir x p]

 distribuir x [] = [[x]]
 distribuir x (y:ys) = (x:y:ys) : (map (y:) (distribuir x ys)) -- [y:xys | xys <- distribuir x ys]

 -- recurso

 -- 1 g

 [(2*x-1,2`x) | x <- [1..]]
