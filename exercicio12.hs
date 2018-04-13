 curta :: [a] -> Bool               -- significa lista booleana
 curta l = (length l) <= 2          -- criar uma função que aceita até dois elementos
 curta [] = True
 curta (x:[]) = True                -- [x]
 curta (x:y:[]) = True              -- [x,y]
 curta _ = False                    -- qualuqer coisa
