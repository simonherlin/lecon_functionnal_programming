-- derivé
deriv f h = \x -> (f(x + h) - f x) / h

-- descente gradient
desc_grad df x0 nu esp =
  if abs(df x0) < esp
  then x0
  else desc_grad df (x0 - nu * (df x0)) nu esp

long [] = 0
long (h :t) = 1 + long t

-- obliger de donner la valeur par default
longT [] acc = acc
longT (h : t) acc = longT t (acc + 1)

-- fonction terminal sans valeur par default
long2 liste =
  longW liste 0
  where
    longW = \l acc-> case l of
      [] -> acc
      (h : t) -> longW t (acc + 1)

-- fonciton puissance
puissance n =
  \x -> x^n

puiss 0 =
  \x -> 1
puiss n =
  \x -> x * puiss(n - 1) x

puiss2 n x =
  0 _ = 1
  n x = puiss2 (n-1) x *x

main = do
  let df = deriv (\x -> x^2) 0.001 in do
    print $ df 5
    print $ desc_grad df 5 0.01 0.001
    print $ long2 [1..6]
    print $ puiss 4 2
