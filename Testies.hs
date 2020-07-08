
funcion :: [Int]
funcion = zipWith (\x y -> if elem x y then x else (-1)) [1,2,3,4] [[1,2,3], [2,3,4], [0,1], [2,3,4]]