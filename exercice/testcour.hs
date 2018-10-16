one = 1
two = 2

bOne True = 1
bOne False = 0

bTwo True = 2
bTwo False = 0

ajoutF :: (Bool -> Int) -> (Bool -> Int) -> Int
ajoutF x y = x True + y True

addOne :: [Int] -> [Int]
addOne [] = []
addOne (h : t) = (h + 1) : addOne t

deleteOne :: [Int] -> [Int]
deleteOne [] = []
deleteOne (h : t) = (h - 1) : deleteOne t

rev :: [Int] -> [Int]
rev [] = []
rev x = reverse x

map2 :: (a -> b) -> [a] -> [b]
map2 _ [] = []
map2 f (h:t) = f h : map2 f t

main = do
  -- print $ ajoutF one two
  print $ ajoutF bOne bTwo
  print $ addOne [1, 2, 3]
  print $ deleteOne [2, 3, 4]
  print $ rev [1, 2, 3]
  print $ map2 addOne [1, 2, 3]
