paire a = if (mod a 2) == 0
  then "Paire"
  else "Impaire"

paire2 :: Integral a => a -> String
paire2 num
  | m == 1 = "Impaire"
  | m == 0 = "Paire"
  where m = mod num 2

paire3 :: Integral a => a -> String
paire3 x =
  case (mod x 2) of
    0 -> "Paire"
    1 -> "Impaire"

main = do
  print $ paire 4
  print $ paire 5
  print $ paire2 4
  print $ paire2 5
  print $ paire3 4
  print $ paire3 5
