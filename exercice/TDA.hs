data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)
data Operator = Plus | Minus | Mult | Div deriving Show
data Lexeme = Number Double | Op Operator | X | Var String deriving Show

leaf el = Node el Empty Empty

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

--bfs :: Tree a -> [a]
bfsAlgo [] = []
bfsAlgo (Empty:tail) = bfsAlgo tail
bfsAlgo ((Node elem tl tr):tail) = elem : bfsAlgo (tail ++ [tl,tr])

bfs t = bfsAlgo [t]

-- TP
maxInTree Empty = 0
maxInTree (Node elem Empty Empty) = elem
maxInTree (Node elem lt Empty) = max elem (maxInTree lt)
maxInTree (Node elem Empty rt) = max elem (maxInTree rt)
maxInTree (Node elem lt rt) = max elem (max (maxInTree lt) (maxInTree rt))

maxTreeOrdo Empty = 0
maxTreeOrdo (Node elem Empty Empty) = elem
maxTreeOrdo (Node elem Empty rt) = maxTreeOrdo rt
maxTreeOrdo (Node elem lt Empty) = elem
maxTreeOrdo (Node elem lt rt) = maxTreeOrdo rt

--maxT Empty m = m
--maxT (Node el lt rt) m = maxT lt (max rt (max el m))

--eval :: Tree Lexeme -> Double
eval (Node (Number n) _ _) _ = n
--eval (Node (Var) _ _) x = x
eval (Node (Op Mult) lr rt) x = (eval lr x) * (eval rt x)
eval (Node (Op Div) lr rt) x = (eval lr x) / (eval rt x)
eval (Node (Op Plus) lr rt) x = (eval lr x) + (eval rt x)
eval (Node (Op Minus) lr rt) x = (eval lr x) - (eval rt x)

--assoc :: a -> [(a, b)] -> b
--assoc y [] = error "not found"
--assoc y ((key, value) : t) 
--    | y == key = value
--    | otherwise = assoc y t


-- evalVar =(Node (Var name) Empty Empty) vars = assoc name vars


-- recherche
--searchItem :: Int -> Tree a -> Bool
searchItem _ Empty = False
searchItem x (Node elem tl tr)
    | elem == x = True
    | elem < x = searchItem x tl
    | otherwise = searchItem x tr


-- insert
insertion x Empty = Node x Empty Empty
insertion x (Node elem tl tr)
    | x <= el = (Node elem (insertion x tl) tr)
    | otherwise = (node elem tl (insertion tr))

-- supression
cutMax Empty = error "empty"
cutMax (Node el tl Empty) = (ag, el)
cutMax (Node el tl tr) = 
    (Node el tl trcut, m)
    where (trcut, m) = cutMax tr

deleteEl _ Empty = error "empty"
deleteEl x (Node el Empty tr)
    | x == el = ad
    | otherwise = Node el Empty (deleteEl x tr)
deleteEl x (Node el tl tr)
    | x < el = (Node el (deleteEl x tl) tr)
    | x > el = (node el tl (deleteEl tr))
    | otherwise = let (tlcut, m) = cutMax tl
                    in (Node m tlcut tr)


rl :: Tree a -> Tree a
rl Empty = Empty
rl (Node el tl tr) =
    Node (racine tr) (Node el tl (left tr)) (Node right tr)

rr :: Tree a -> Tree a
rd Empty = Empty
rd (Node el tl tr) =
    Node (racine tl) (left tl) (Node el (right tl) tr)

countT Empty a b = []
countT (Node el tl tr) a b
    | el < a = countT tr a b
    | b < el = countT tl a b
    | otherwise = el:(countT tl a b) ++ (countT tr a b)

main = let
        a = treeCons 21 (treeCons 8  (treeCons 5 treeEmpty treeEmpty) treeEmpty) (treeCons 16 treeEmpty treeEmpty)
        b = treeCons (Op Plus) 
                (treeCons (Op Mult) 
                    (leaf (Number 12) )
                    (treeCons (Op Minus) 
                        (leaf (Number 56))
                        (leaf (Number 11 ))))
                (treeCons (Op Plus) 
                    (leaf (Number 3))
                    (leaf (Number 78)))
        c = treeCons 12
                (treeCons 7 
                    (treeCons 7 Empty Empty) 
                    (treeCons 10 
                        (treeCons 8 Empty Empty) 
                        (treeCons 11 Empty Empty)) 
                (treeCons 20 
                    Empty 
                    (treeCons 26 
                        (treeCons 22 Empty Empty) 
                        Empty))        
        in do
            -- print $ a
            --print $ isEmpty a
            --print $ racine a
            --print $ left a
            --print $ right a
            --print $ search 8 a
            --print $ search' 8 a
            --print $ nbNode a
            --print $ height a
            --print $ prefix a
            --print $ infix2 a
            --print $ postfix a
            --print $ bfs a
            --print $ maxInTree a
            --print $ maxTreeOrdo a
            --print $ eval b
            print $ searchItem 8 c