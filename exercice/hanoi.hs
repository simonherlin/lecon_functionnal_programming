hanoi :: Int -> Char -> Char -> Char -> [(Char, Char)]
hanoi 0 _ _ _ = []
hanoi n a b c = hanoi (n - 1) a c b ++ ( (a,c) : hanoi (n - 1) b a c)

main = do
  print $ hanoi 30 'A' 'B' 'C'
