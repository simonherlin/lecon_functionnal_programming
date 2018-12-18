-- fonction sans nom
addOne = \x -> x + 1

curry_add = \x -> \y -> x + y

add a =
  \x -> x + a

cacheX a =
  let x = 1
  in
    let f = \x -> x + a --  dans un lambda le x n'est pas le meme que la variable c'est locale
    in f 2

--fix point combinator
y f = f (y f)

premier a b = a

fact' = \rec x -> if x == 0 then 1 else x * rec (x-1)
factoriel = y fact'

main = do
  print $ (\x -> x + 1) 4
  print $ (\x y -> x + y) 3 5
  print $ addOne 3
  print $ curry_add 2 3
  -- :type curry_add
  print $ add 4 10
  let add1 = add 1
      add4 = add 4 in do
        print $ add1 10
        print $ add4 20
  -- print $ y (1:)
  print $ y (premier 43)
  print $ take 12 (y (1:))
  print $ factoriel 5
