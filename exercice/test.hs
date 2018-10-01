{- main = do
  putStrLn "- Hello world!"
  putStrLn "- Pardon?"
-}
-- commentaire une ligne
{-
  commentaire block
-}

{-a = 1
a = 2
newA = a + 1

main = do
  print a
  print newA
  print $ a + 20
  -- permet d'évaluer tout ce qui est à droite
-}

{-
x = 5
a = (x >= 12)
b = (x <=2)
c = (x<6)

main = do
  print ((a && b) || c)
  print (a && (b || c))
  print (xor a b)

-}

-- liste
-- ["lundi", "mardi", "mercredi"]


--(30,x,y) = (30,3,2)
--(30,x,y) = (30,3,2,21)

--(_,_,)


squareArea :: (Integral a) => a -> a
squareArea cote = cote * cote

triangleArea :: (Fractional a) => a -> a -> a
triangleArea base height = (base * height) / 2

sayMe :: (Integral a) => a -> String
sayMe 1 = "One !"
sayMe 2 = "Two!"
sayMe 3 = "Three!"
sayMe 4 = "Four!"
sayMe x = "Not between 1 and 4"

first :: (a, b, c) -> a
first (x, _, _) = x

non :: Bool -> Bool
non True = False
non False = True

et :: Bool -> Bool -> Bool
et True True = True
et _ _ = False

ou :: Bool -> Bool -> Bool
ou False False = False
ou _ _ = True

nand :: Bool -> Bool -> Bool
nand a b = non(et a b)


juryTell :: (RealFloat a) => a -> a -> a -> String
juryTell n1 n2 n3
  | moyenne < s1 = "Seconde session"
  | moyenne <= s2 = "passable"
  | moyenne <= s3 = "pas mal"
  | otherwise   = "intéressant"
  where moyenne = (n1+n2+n3) / 3
        (s1, s2, s3) = (10, 12, 14)


cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
  let sideArea = 2 * pi * r * h
      topArea = pi * r ^2
  in  sideArea + 2 * topArea

main = do
  print $ juryTell 16 12 15
  print $ nand True True
  print $ nand False True
  print $ nand False False
  print $ ou True False
  print $ ou False False
  print $ non True
  print $ non False
  print $ et True True
  print $ et True False
  print $ et False False
  print $ squareArea 10
  print $ triangleArea 5 3
  print $ sayMe 2
  print $ sayMe 5
  print $ sayMe 1
