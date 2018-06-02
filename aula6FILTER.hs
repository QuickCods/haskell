-- FILTER

 sopares :: [Int] -> [Int]
 sopares [] = []
 sopares (x:xs) | gosto x = x:sopares xs
                | otherwise        = sopares xs
                     where gosto x = x `mod` 2 == 0

 somaiores10 :: [Int] -> [Int]
 somaiores10 [] = []
 somaiores10 (x:xs) | gosto x      = x:somaiores10 xs
                    | otherwise    = somaiores10 xs
                     where gosto x = x > 10

 soosqueeugosto :: (a -> Bool) -> [a] -> [a]
 soosqueeugosto gosto [] = []
 soosqueeugosto gosto (x:xs) | gosto x   = x:soosqueeugosto gosto xs
                             | otherwise = soosqueeugosto gosto xs

 somaiores10' l = soosqueeugosto gosto l
              where gosto x = x > 10

 somaiores10'' l = filter gosto l
              where gosto x = x > 10

 somaiores10''' l = filter (\x -> x > 10) l

 meumap f l = [ f x | x <- l ]

 meufilter gosto l = [ x | x <- l, gosto x]
