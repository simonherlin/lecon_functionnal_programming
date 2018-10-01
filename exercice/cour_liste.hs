traiter [] = executer "fin de tache"
traiter (h:t) = do
    executer h
    traiter t

executer tache = putStrLn tache

longueur :: [ n ] -> Int
longueur [] = 0
longueur (h:t) = longueur t + 1 -- moins lourd que 1 + longueur t

sommeListe :: Num n => [ n ] -> n
sommeListe [] = 0
sommeListe (h:t) = sommeListe t + h

double :: Num a => [ a ] -> [ a ]
double [] = []
double (h:t) = ( 2*h : double t )


conc :: [ String ] -> String
conc [] = ""
conc (h:t) = h ++ conc t

extractPaire :: [Int] -> [Int]
extractPaire [] = []
extractPaire (h:t)
  | mod h 2 == 0 = (h : extractPaire t )
  | otherwise = extractPaire t

concatList :: [Int] -> [Int] -> [Int]
concatList [] a = a
concatList (h:t) a = h : (concatList t l)

addItem :: Int -> Int -> [Int] -> [Int]
addItem 


main = do
  traiter ["cafe", "manger des frites", "dsormir", "travailler"]
  print $ length ["coucou", "tête de chêvre", "Ah que coucou", "salut"]
  print $ longueur ["coucou", "tête de chêvre", "Ah que coucou", "salut"]
  print $ sommeListe [3,21,44,91]
  print $ double [1,2,3,4,5,6]
  print $ concat ["je", "veux", "du", "cafe"]
  print $ extractPaire[1,2,3,4,5,6]
  print $ concatList [1,2,3] [4,5]
