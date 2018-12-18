module Ope (
    Op(Plus, Moins)
)
where

import System.IO

data Op = Plus Integer Integer
        | Moins Integer Integer
        deriving (Show)


main = do
        print $ Plus 3 4
        h <- openFile "test.txt" WriteMode
        hPutStr h "YES !!"
        hClose h