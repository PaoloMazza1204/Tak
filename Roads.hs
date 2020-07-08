module Roads
( notSquaredWalks
) where

import Data.List ((\\))

type Coord = (Int, Int)
type Path = [Coord]

orthDeltas :: [Coord]
orthDeltas = [(x, y) | x <- [-1..1], y <- [-1..1], (x == 0) /= (y == 0)]

insideBoard :: Int -> Coord -> Bool
insideBoard size (x, y) = x >= 0 && y >= 0 && x < size && y < size

orthAdjacent :: Int -> Coord -> [Coord] 
orthAdjacent size (x, y) = filter (insideBoard size) [(x + dx, y + dy) | (dx, dy) <- orthDeltas]

walks :: Int -> [Path] -> [Path]
walks size paths
    -- Punto fijo, la lista de entrada y resultado tiene los mismo valores.
    | null (nextPaths \\ paths) = paths
    -- Se cambiaron los caminos, hay que seguir calculando.
    | otherwise = walks size nextPaths
    where
        step = [next:path | path <- paths, next <- orthAdjacent size (head path), not (elem next path)]
        nextPaths = filter (\p -> (length p) <= 2 * (size - 1)) step --------------- 02 12 22 
                                                                     --------------- 01 11 21 una vez se tenga el resultado hay que filtrar los que terminan en adyacentes (cuadros)
                                                                     --------------- 00 10 20

-- Devuelve True si la caminata no tiene forma cuadrada.
notSquaredWalk :: Path -> Bool
notSquaredWalk path = diffX > 1 || diffY > 1 -- No es cuadrada si no se termina en un adyacente.
    where
        startCoord = last path
        endCoord = head path
        diffX = abs ((fst startCoord) - (fst endCoord))
        diffY = abs ((snd startCoord) - (snd endCoord))

-- Devuelve las caminatas que no son cuadradas.
notSquaredWalks :: Int -> [Path] -> [Path]
notSquaredWalks size paths = filter notSquaredWalk (walks size paths)

-- Si la caminata tiene forma de L la modifica para que quede recta.
-- Se basa en que las caminatas con forma de L siempre aparecen al principio y final de la lista.
{-modifyLWalk :: Path -> Path
modifyLWalk path = if hasLShape then modifiedpath else path
    where
        x = fst (last path)
        y = snd (last path)
        hasLShape = 
        modifiedpath = drop 1 path-}
