
puissance1 :: Float -> Int -> Float
puissance1 a n
  | n == 0 = 1
  | otherwise = puissance1 a (n - 1) * a

puissance2 :: Float -> Int -> Float
puissance2 a n =
  if n == 0
  then 1
  else puissance2 a (n - 1) * a

puissance3 :: (Fractional a, Integral b) => a -> b -> a
puissance3 _ 0 = 1
puissance3 a n | n > 0 = puissance3 a (n - 1) * a
               | n < 0 = 1 / puissance3 a (-n)


puissance4 :: Float -> Int -> Float -> Float
puissance4 a n acc
puissance4 _ 0 acc = acc
puissance4 

main = do
  print $ puissance1 2 3
  print $ puissance2 2 3
  print $ puissance3 2 3
