suite :: Int -> Float
suite 0 = 0.8
suite n = 0.6 * un * (un - 1)
      where un = suite (n - 1)

fibonacci :: Int -> Int
fibonacci 0 = 1
fibonacci 1 = 1
fibonacci n = fibonacci(n - 1) + fibonacci (n - 2)

fibFast :: Int -> Int
fibFast n =
  let fib = 0:1:zipWith (+) fib (tail fib)
  in fib!!n

-- recursivité terminal, on utilise les argument en mémoire
fibofast :: Int -> Int -> Int -> Int
fibofast 0 unMoinUn un = un
fibofast n unMoinUn un =  fibofast (n - 1) un (unMoinUn + un)



main = do
  print $ suite 1000
  print $ fibonacci 5
  print $ fibFast 100
  print $ fibofast 1000000 0 0
