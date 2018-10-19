-- redefinition fonction map
map2 :: (a -> b) -> [a] -> [b] -- curryfication une focntion qui donne une fonction qui donne une fonction
map2 _ [] = []
map2 f (h:t) = f h : map2 f t

-- filter
-- supprime tous les éléments de la liste list qui ne vérifie pas le prédicat
-- exemple [2, 1, 3, 5, 4] supprime tous les nombres paire
-- retourne [2,4]
filter2 :: (a -> Bool) -> [a] -> [a]
filter2 _ [] = []
filter2 pred (h:t) 
    | pred h = h : filter2 pred t
    | otherwise = filter2 pred t

-- dropwhile predicat list
dropWhile2 :: (a -> Bool) -> [ a ] -> [ a ]
dropWhile2 _ [] = []
dropWhile2 pred (h:t) 
    | pred h = dropWhile2 pred t
    | otherwise = h:t


-- partition predicat list
partition2 :: (a -> Bool) -> [a] -> ([a], [a])
partition2 _ [] = ([], [])
partition2 pred (h : t)
    | pred h = (h :l1, l2)
    | otherwise = (l1, h:l2)
    where (l1, l2) = partition2 pred t

-- all predicat list
all2 :: (a -> Bool) -> [a] -> Bool
all2 _ [] = True
all2 pred (h : t) = pred h && (all2 pred t)

-- any predicat list
any2 :: (a -> Bool) -> [ a ] -> Bool
any2 _ [] = False
any2 pred (h:t) = pred h || (any2 pred t)

--fold left
foldl2 :: (a -> b -> b) -> b -> [a] -> b
foldl2 _ acc [] = acc
foldl2 f acc (h:t) = foldl2 f (f h acc) t

-- fold right
foldr2 :: (a -> b -> b) -> b -> [a] -> b
foldr2 _ acc [] = acc
foldr2 f acc (h:t) = f h (foldr2 f acc t)

--

main = do
    print $ filter2 (\x -> mod x 2 == 0) [2, 1, 3, 5, 4]
    print $ dropWhile2 (\x -> mod x 2 == 0) [2, 4, 3, 6]
    print $ partition2 (\x -> mod x 2 == 0) [2, 4, 3, 6]
    print $ all2 (\x -> mod x 2 == 0) [2, 4, 6]
    print $ all2 (\x -> mod x 2 == 0) [2, 4, 5]
    print $ any2 (\x -> mod x 2 == 0) [1, 3, 5]
    print $ any2 (\x -> mod x 2 == 0) [1, 3, 6]
    print $ foldl2 (+) 0 [1, 2, 3, 4, 5, 6]
    print $ foldr2 (+) 0 [1, 2, 3, 4, 5, 6]