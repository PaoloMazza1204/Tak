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
        step = [next:path | path <- paths, next <- orthAdjacent size (head path)]
        nextPaths = filter (\p -> (length p) <= 2 * (size - 1)) step