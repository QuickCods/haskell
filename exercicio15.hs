 --1)

 f :: (Int,Int) -> Int

 --2)
 
 f :: Int -> Int
 g :: Int -> Int

 --3)
 
 f :: Int -> (Int -> Int)
 g :: Int

 --4)
 
 f :: ([Int] -> [Int]) -> [Int] -> Int
 g :: [Int] -> [Int]

 --5)
 f :: (Int, Int) -> [Int -> Int]
