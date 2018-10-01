facto :: Int -> Int
facto 0 = 1
facto n = n * facto (n - 1)

main = do
  facto 6
