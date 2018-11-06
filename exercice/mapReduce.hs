import Data.List

map_reduce_1 the_split the_map the_reduce dat = the_split dat

map_reduce_2 the_split the_map the_reduce dat = 
        concat (map the_map (the_split dat))

map_reduce :: Ord t1 => (t2 -> [a]) -> (a -> [(t1, b)]) -> ([b] -> t) -> t2 -> [(t1, t)]
map_reduce the_split the_map the_reduce dat = 
    map (\(a,b) -> (a,(the_reduce b))) (shuffle (foldl (++) [] (map the_map (the_split dat))))
        

cut :: Char -> [Char] -> [ [Char] ]
cut _ [] = [[]]
cut sep (h:t)
    | h == sep = "" : w : rest
    | otherwise = (h:w):rest
    where (w:rest) = cut sep t  

wc_split :: [Char] -> [ [Char] ]
wc_split [] = []
wc_split l = cut '\n' l

wc_map :: [Char] -> [ ([Char], Int) ]
wc_map d = 
    let l = cut ' ' d
    in  map (\x -> (x, 1)) l

join' :: Ord a => [(a,b)] -> [(a,[b])]
join' [] = []
join' [(k,v)] = [(k,[v])]
join' ((x1,x2):xs)
    | a == x1 = (a,x2:b):t
    | otherwise = (x1,[x2]):(a,b):t
    where ((a,b):t) = join' xs

shuffle :: Ord a => [(a,b)] -> [(a,[b])]
shuffle l = join' (sortBy (\(a,_) (b,_) -> compare a b) l)

wc_reduce :: [Int] -> Int
wc_reduce = foldl (+) 0

main =
    let dat = "la la la la \nvoila le temps \ndes cerises \ndes bonnes cerises \ndes oiseaux sont la aussi"
    in do
        print $ map_reduce_1(\x -> x) (\x -> x) (\x -> x) dat 
        -- print $ cut '\n' dat
        print $ wc_split dat
        print $ map_reduce_1 wc_split (\x -> x) (\x -> x) dat  
        print $ map_reduce_2 wc_split wc_map (\x -> x) dat 
        print $ shuffle [("la", 1),("ka", 1), ("la", 1)] 
        print $ map_reduce wc_split wc_map wc_reduce dat