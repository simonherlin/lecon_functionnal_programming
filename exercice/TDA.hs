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

isLeaf' :: Tree a -> Bool
isLeaf' = not(isEmpty a) && (isEmpty(left a)) && (isEmpty (right a))
 
main = left
        a = treeCons 21 (treeCons 8 treeEmpty treeEmpty) treeEmpty
        in do
            print $ a
            print $ isEmpty a
            print $ racine a
            print $ left a
            print $ right a