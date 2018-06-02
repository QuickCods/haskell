 data Arv a = No a (Arv a) (Arv a)
            | Folha
                deriving Show

-- 70

 soma_nos :: Num a => Arv a -> a
 soma_nos (Folha) = 0
 soma_nos (No v ae ad) = v + (soma_nos ae) + (soma_nos ad)
