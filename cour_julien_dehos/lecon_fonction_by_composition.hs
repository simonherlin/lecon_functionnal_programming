h :: Int -> Int
h x = x^2
g :: Int -> Int
g x = x+1
f = g . h

main = do
    print $ f 3