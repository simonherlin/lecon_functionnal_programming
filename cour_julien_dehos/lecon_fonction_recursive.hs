factorielle1 :: Int -> Int
factorielle1 n = if n==1
                 then 1
                 else n * factorielle1 (n-1)

factorielle2 :: Int -> Int
factorielle2 n
    | n==1 = 1
    | otherwise = n * factorielle2 (n-1)

factorielle3 :: Int -> Int
factorielle3 1 = 1
factorielle3 n = n * factorielle3 (n-1)

-- recursivité non terminal
taille :: [Int] -> Int
taille [] = 0
taille (x:xs) = 1 + (taille xs)


-- recursivité terminal
taille' :: [Int] -> Int -> Int
taille' [] n = n
taille' (x:xs) n = taille' xs (n+1)


main = do
    print $ factorielle1 5
    print $ factorielle2 6
    print $ factorielle3 7
    print $ taille [1..42]
    print $ taille' [1..42] 0