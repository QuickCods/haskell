 permutacoes [] = [[]]
 permutacoes (x:xs) = [ps | p <- permutacoes xs, ps <- distribuir x p]

 distribuir x [] = [[x]]
 distribuir x (y:ys) = (x:y:ys) : (map (y:) (distribuir x ys)) -- [y:xys | xys <- distribuir x ys]
