pgcd :: Int -> Int -> Int
pgcd a 0 = a
pgcd a b = pgcd b (mod a b)


main = do
  print $ pgcd 70 462
