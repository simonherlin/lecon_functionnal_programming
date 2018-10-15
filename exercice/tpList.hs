--maxL :: [Int] -> Int -> Int
--maxL [] n = n
--maxL (h:t) n
--  | h > n = maxL t h
--  | otherwise = maxL t n

maxL :: [Int] -> Int
maxL [a] = a
maxL (h:t) =
  if m < h
  then h
  else m
  where m = maxL t

maxL2 :: [Int] -> Int
maxL2 [a] = a
maxL2 (h:t) =
  let m = maxL t
  in  if m < h
      then h
      else m

-- lsit eof liste
--maxLL :: [[Int]] -> Int
maxLL [] = []
maxLL (h : t) =
  maxL h : maxLL t

findE :: [Int] -> Int -> Bool
findE [] _ = False
findE (h : t) a = (h==a) || findE t a
  --if h == a
  --then True
  --else findE t a

data Result a = Found a | NotFound

instance Show a => Show (Result a) where
  show (Found x) = show x
  show NotFound = "[not found]"

--indByKey :: a -> [(a, b)] -> Result b
findByKey [] _ = NotFound
findByKey ((key, value) : t) x
  | x == key = Found value
  | otherwise = findByKey t x

--split :: [Int] -> [Int] -> [Int] -> [[Int]]
split [] lg ld = (lg, ld)
split (h : t) lg ld = split t ld (h:lg)

split2 [] = ([], [])
split2 (h:t) =
  let (lg, ld) = split2 t
  in (ld, h:lg)

distance (x, y) (x2, y2) =
  sqrt ((x - x2)^2 + (y - y2)^2)

longueur [] = 0
longueur [p] = 0
longueur (p1 : p2 : t) =
  distance p1 p2 + longueur (p2 : t)

addtask t [] = [t]
addtask (t, p) ((th, ph) : ta)
  | p > ph = ((t, p) : (th, ph) : ta)
  | otherwise = (th, ph) : addtask (t, p) ta

main = do
  print $ maxL [5, 2, 50, 3, 12, 29, 64]
  print $ maxL2 [5, 2, 50, 3, 12, 29, 64]
  print $ maxLL [[5,2,3], [53, 12, 89], [9, 30, 29]]
  print $ findE [2, 1, 4, 9] 4
  print $ findByKey [('c', 3), ('a', 2), ('d', 15)] 'a'
  print $ split [1,2,3,4,5,6,7,8] [] []
  print $ split2 [1,2,3,4,5,6,7,8]
  print $ longueur [(0, 0), (3, 4), (13, 4)]
