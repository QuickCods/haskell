 fact 0 = 1
 fact n = n * fact (n-1)


 facts = [ fact i | i <- [0..] ]

 facts' = map fact [0..]

 fib 0 = 0
 fib 1 = 1
 fib n = fib (n-1) + fib (n-2)

 fibs = [ fib i | i <- [0..] ]

 fibs' = map fib [0..]

 ones = 1:ones

 inteiros = 0:map (1+) inteiros

 facts'' = 1:zipWith (*) facts'' (tail inteiros)

 fibs'' = 0:1:zipWith (+) (fibs'') (tail fibs'')
