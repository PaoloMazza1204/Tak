
funcion :: [Int]
funcion = zipWith (\x y -> if elem x y then x else (-1)) [1,2,3,4] [[1,2,3], [2,3,4], [0,1], [2,3,4]]

taken = case 3 of
            0 -> [0]
            1 -> [1]
            3 -> [3]
            2 -> [2]