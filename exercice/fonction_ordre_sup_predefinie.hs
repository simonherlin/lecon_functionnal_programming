-- redefinition fonction map
map2 :: (a -> b) -> [ a ] -> [ b ] -- curryfication une focntion qui donne une fonction qui donne une fonction
map2 _ [] = []
map2 f (h:t) = f h : map2 f t

-- filter
-- supprime tous les éléments de la liste list qui ne vérifie pas le prédicat
-- exemple [2, 1, 3, 5, 4] supprime tous les nombres paire
-- retourne [1,2,5]
filter2 :: (a -> Bool) -> [ a ] -> [ a ]
filter2 _ [] = []
filter2 pred (h:t) 
    | pred h = h : filter2 pred t
    | otherwise = filter2 pred t



main = do
