-- fonction récurssive terminal
occu :: Int -> [Int] -> Int -> Int
occu _  [] acc = acc
occu x (h:t) acc
    | x == h = occu x t (acc +1)
    | otherwise = occu x t acc

-- fonction récursive profond
countx :: Int -> [Int] -> Int
countx _ [] = 0
countx x (h:t)
    | x == h = countx x t + 1
    | otherwise = countx x t

-- avec foldl
counx2 x l = foldl (\acc y -> if y == x then acc + 1 else acc) 0 l

-- fonction dont le résultat est une fonction avec un argument x qui compte le nombre
-- d' élément égaux à X dans une liste L
countF [] = \x -> 0
countF (h:t) =
    \x -> case x == h of
            True -> countF t x + 1
            False -> countF t x 

countF2 [] = \x -> 0
countF2 (h:t) =
    \x ->   if h == x
            then countF2 t x + 1
            else countF2 t x

occurs l le =
    map (countF2 l) le


    
main = do
    print $ countx 2 [1, 2, 2, 3, 4, 2]
    print $ occu 2 [1, 2, 2, 3, 4, 2] 0
    --print $ occurs [1, 2] [1, 2, 2, 3, 4, 2]
    print $ countF2 [1,2,3,4,5] 4
    print $ occurs [1, 2, 3, 1, 2, 4, 1, 2, 5, 6] [1, 2, 3]