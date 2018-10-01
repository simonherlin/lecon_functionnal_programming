tour :: Int -> Int -> IO ()
tour x 0 =
  putStrLn $ "Perdu loser. Le nombre était " ++ (show x)
tour x n = do
  p <- askNumber
  if p == x
    then putStrLn "Ouai Youpi tu as gagané"
    else do
      if p < x
        then putStrLn "Le nombre proposé est trop petit"
        else putStrLn "Le nombre proposé est trop grand"
    tour x (n - 1)

askNumber :: IO Int
askNumber = do
  putStr "Proposer votre nombre"
  pString <-

play :: Int -> IO ()
play n = tour n 5

main = do
  num <- randomRIO (1, 100) :: IO Int
  play num
