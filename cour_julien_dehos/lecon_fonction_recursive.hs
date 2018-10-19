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

main = do
    print $ factorielle1 5
    print $ factorielle2 6
    print $ factorielle3 7