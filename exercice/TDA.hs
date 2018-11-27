data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)

isEmpty :: Tree a -> Bool
isEmpty Empty = True
isEmpty _ = False

treeEmpty = Empty

treeCons:: a -> Tree a -> Tree a -> Tree a
treeCons elem tl tr = Node elem tl tr

racine :: Tree a -> a
racine (Node elem _ _) = elem

right :: Tree a -> Tree a
right (Node _ _ tr) = tr

left :: Tree a -> Tree a
left (Node _ tl _) = tl

isLeaf :: Tree a -> Bool
isLeaf (Node _ Empty Empty) = True
isLeaf _ = False

--isLeaf' :: Tree a -> Bool
--isLeaf' = (not(isEmpty a) && (isEmpty(left a)) && (isEmpty(right a)))

search :: (Eq a) => a -> Tree a -> Bool
search _ Empty = False
search x (Node elem tl tr) = (x == elem) || (search x tl) || (search x tr)

search' x t = not (isEmpty t) && 
                ( (x == (racine t) || search' x (left t)) || search' x (right t))

searchOrdo _ Empty = False
searchOrdo x (Node elem tl tr) 
    | x == elem = True
    | x < elem = searchOrdo x tl
    | otherwise = searchOrdo x tr

nbNode :: Tree a -> Int
nbNode Empty = 0
nbNode (Node _ tl tr) = 1 + nbNode tl + nbNode tr

height :: Tree a -> Int
height Empty = 0
height (Node _ tl tr) = 1 + max (height tl) (height tr)

someElem Empty = 0
someElem (Node elem tl tr) = elem + (someElem tl) + (someElem tr)

prefix :: Tree a -> [a]
prefix Empty = []
prefix (Node elem tl tr) = elem:(prefix tl) ++ (prefix tr)

infix2 :: Tree a -> [a]
infix2 Empty = []
infix2 (Node elem tl tr) = (infix2 tl) ++ (elem: (infix2 tr))

postfix :: Tree a -> [a]
postfix Empty = []
postfix (Node elem tl tr) = (postfix tr) ++ (postfix tl) ++ [elem]

main = let
        a = treeCons 21 (treeCons 8 treeEmpty treeEmpty) treeEmpty
        in do
            print $ a
            print $ isEmpty a
            print $ racine a
            print $ left a
            print $ right a
            print $ search 8 a
            print $ search' 8 a
            print $ nbNode a
            print $ height a
            print $ prefix a
            print $ infix2 a
            print $ postfix a
            