{-
a)
[[],[5,2]]
b)
[1,4,7,10]
c)
11
d)
[0,3,6,9]
e)
[1,3,5,7,2]
f)
[(1,3),(2,2)]
g)
[3*x + 1 | x <- [0..10]]
h)
60
i)
f :: [a] -> a
j)
[(!!2),length] :: [[Int] -> Int]
k)
Num a => N -> a
l)
??
-}

-- 2 a

 maioresQ :: Ord a => [a] -> a -> [a]
 maioresQ [] x = []
 maioresQ l x = [p | p <- l, p > x]        -- doesn't compile bichs

-- b

 tamanhoS :: [String] -> [Int]
 tamanhoS l = [length x | x <- l]

-- 3 a

 timesMat m1 m2 = [[if any (== 1) (zipWith (*) (m1!!l) (map (!!c) m2)) then l else 0 | c <- [0..n]] | l <- [0..n]]
               where n = length m1 - 1

-- b

 menor m1 m2 = and [and [ (m1!!l!!c) <= (m2!!l!!c) | c <- [0..n]] | l <- [0..n]]
              where n = length m1 - 1

-- 5

 inff :: [Int]
 inff = 0:zipWith (+) inff [1..]

-- 6

 data Arv a = Vazia | No a (Arv a) (Arv a)

-- a

 listar :: Arv a -> [a]
 listar Vazia = []
 listar (No x esq dir) = listar esq ++ [x] ++ listar dir

-- b

 simetrica :: Arv a -> Arv a
 simetrica Vazia = Vazia
 simetrica (No x esq dir) = No x (simetrica dir) (simetrica esq)

-- 7

 listar vazia = []
 listar (No x esq dir) = listar esq ++ [x] ++ listar dir

 simetrica vazia = vazia
 simetrica (No x esq dir) = No x (simetrica dir) (simetrica esq)

 listar t = reverse ( listar ( simetrica t))

 --Por indução sobre t:
 -- t = vazia

 listar vazia = [] = reverse ([]) = reverse (listar vazia) = reverse (listar (simetrica vazia))


 listar esq = reverse (listar (simetrica esq))
 listar dir = reverse (listar (simetrica dir))

 reverse (listar (simetrica (No x esq dir))) =
 reverse (listar (No x (simetrica dir) (simetrica esq))) =
 reverse (listar (simetrica esq)) ++ [x] ++ (listar (simetrica dir)) =

{-
reverse (xs ++ ys) =
reverse ys ++ reverse xs
reverse ((xs ++ [x]) ++ ys) =
reverse ys ++ reverse (xs ++ [x]) =
reverse ys ++ [x] ++ reverse xs
-}

 (H.I.)listar esq ++ [x] ++ listar dir =
 listar (No x esq dir)
