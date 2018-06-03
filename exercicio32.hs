 procura_raiz :: Integer -> Integer -> Integer
 procura_raiz n r | r^2 == n               = r
                  | r^2 < n && (r+1)^2 > n = r
                  | r^2 < n = procura_raiz n ( r+1 )

 raizq :: Integer -> Integer
 raizq n = procura_raiz n 0
