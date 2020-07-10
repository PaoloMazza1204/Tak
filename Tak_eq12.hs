{- Tak ------------------------------------------------------------------------------------------
 
Plantilla de código para el proyecto del curso de 2020 de _Programación Funcional_ para las carreras 
de Ingeniería y Licenciatura en Informática de la FIT (UCU).
Los docentes no garantizan que este código esté libre de errores. De encontrar problemas, por favor
reportarlos a la cátedra.

Leonardo Val, Ignacio Pacheco.
-}
module Tak where

import Data.Maybe (fromJust, listToMaybe)
import Data.List
import Data.Char (ord)
import System.Random
import Roads -- Librería con código relevante para calcular los posibles caminos ganadores.

{- Es posible que el paquete `System.Random` no esté disponible si se instaló el core de la Haskell 
Platform en el sistema. Para instalarlo, ejecutar los siguientes comandos:

> cabal update
> cabal install random

La herramienta `cabal` es un manejador de paquetes usado por la plataforma Haskell. Debería estar 
disponible junto con el `ghci`.

-}

{- Lo que verás cuando juegas (3x3):

A1 A2 A3
B1 B2 B3    <- Casillero.
C1 C2 C3

||WhitePlayer|| -> Muro del jugador WhitePlayer.
__BlackPlayer__ -> Plana del jugador BlackPlayer.

El tope de la casilla es el primer elemento de la lista.
           |
           v
A1: [||BlackPlayer||, __WhitePlayer__]   <- Fichas en la casilla A1.
A2: [__WhitePlayer__]   <- Fichas en la casilla A2.
.
.
.
C3: [] <- Fichas en la casilla C3.

Fichas blancas: 10 <- Fichas restantes del jugador WhitePlayer.
Fichas negras: 10 <- Fichas restantes del jugador BlackPlayer.
Jugador actual: WhitePlayer <- Jugador actual.
-}


---------------------- Lógica de juego ------------------------------------------------------------

data TakPlayer = WhitePlayer | BlackPlayer deriving (Eq, Show, Enum, Bounded) -- Jugadores.
data Chip = Wall TakPlayer | Stone TakPlayer deriving (Eq) -- Fichas.
data Box = Empty String | Stack String [Chip] deriving (Eq) -- Casillas. Son vacías o pilas de 1 o más fichas.
data PlayerChips = Whites Int | Blacks Int -- Cantidad de fichas de los jugadores.
data TakGame = Board [Box] (PlayerChips, PlayerChips) TakPlayer -- Tablero: /Casillas/Fichas/JugadorActivo.
data TakAction = Move String String [Int] | Place Chip String deriving (Eq) -- Posibles movimientos.

-- Util.
boardStrings3x3 = [x ++ y | x <- ["A", "B", "C"], y <- ["1", "2", "3"]]
emptyBoard3x3 = [Empty s | s <- boardStrings3x3]

boardStrings4x4 = [x ++ y | x <- ["A", "B", "C", "D"], y <- ["1", "2", "3", "4"]]
emptyBoard4x4 = [Empty s | s <- boardStrings4x4]

player1 = WhitePlayer
player2 = BlackPlayer
players = [player1, player2] -- Jugadores.

firstPlayer = player1 -- Jugador inicial.

-- Mensajes de posibles errores:
errorPlaceNoEmpty = "Solo puedes colocar fichas en espacios vacíos."
errorNotYourTurn = "Le toca al otro jugador."
errorLacksChips = "No tienes más fichas."
errorWrongChipsFormat = "El formato de la tupla de fichas debe ser (Whites w, Blacks b)"
errorMoveEmpty = "No se puede mover una casilla vacía."
errorNotYourStack = "Esta pila no te pertenece."
errorNotStraight = "Movimiento no válido, el camino descrito no es derecho."
errorWallInPath = "Movimiento no válido, hay un muro en el camino."
errorPath = "Desplazamiento no válido."
errorMoveNotFound = "Movimiento no válido, quizá escribiste mal el movimiento."
errorInvalidCoords = "Coordenadas no válidas."

-- Format para una tupla de fichas de los jugadores.
showChips :: (PlayerChips, PlayerChips) -> String
showChips (a,b) = (show a) ++ ('\n':(show b))

-- Format para un tablero.
showBoxes :: [Box] -> String
showBoxes xs = concat (if largo == 9 then separarEnN 3 lista else separarEnN 4 lista)
   where
      lista = map ((++" ") . show) xs
      largo = length lista
      separarEnN n [] = []
      separarEnN n lista = (take n lista) ++ ("\n":(separarEnN n (drop n lista)))

-- Format para una casilla.
showBox :: Box -> String
showBox (Empty s) = s ++ ": []\n"
showBox (Stack s fichas) = s ++ ": " ++ (show (map show fichas)) ++ "\n"

-- Devuelve True si la pila pertenece al jugador.
isFromPlayer :: TakPlayer -> Box -> Bool
isFromPlayer p (Stack _ (Stone r:_)) = p == r
isFromPlayer p (Stack _ (Wall r:_)) = p == r
isFromPlayer _ _ = False

-- Devuelve posibles movimientos.
linkTraceWithMovement :: Int -> String -> [[Int]] -> String -> [TakAction]
linkTraceWithMovement stackSize i des f = moves
   where
      moves = [Move i f d | d <- des, (distanceBetween i f) == (length d), (sum d) <= stackSize]

-- Devuelve una lista con los posibles recorridos.
possibleTraces :: Int -> [[Int]]
possibleTraces size = list1 ++ list2 ++ list3
   where
      list1 = [[x] | x <- [1..size], x <= size]
      list2 = [[x,y] | x <- [1..size], y <- [1..size], (x + y) <= size]
      list3 = [[x,y,z] | x <- [1..size], y <- [1..size], z <- [1..size], (x + y + z) <= size]

-- Devuelve la distancia entre dos casillas.
distanceBetween :: String -> String -> Int
distanceBetween s1 s2 = distance
   where
      distance = if head s1 == head s2
                 then abs ((ord (last s1)) - (ord (last s2)))
                 else abs ((ord (head s1)) - (ord (head s2)))

-- Devuelve una lista con los destinos factibles.
realMovements :: String -> Int -> [String] -> [String]
realMovements pos stackSize possibles = res
   where
      deletedPos = delete pos possibles
      res = filter (inRange pos) deletedPos
      inRange s1 s2 = (distanceBetween s1 s2) <= stackSize

-- Devuelve una lista con los destinos posibles.
possibleMovements :: Char -> Char -> Int -> [String]
possibleMovements a b boardSize = result
   where
      result = [x:[y] | x <- ['A'..lastChar], y <- ['1'..lastDigit], (x == a || y == b)] 
      lastChar = if boardSize == 3 then 'C' else 'D'
      lastDigit = if boardSize == 3 then '3' else '4'

-- Devuelve true si una casilla es vacía.
isEmpty :: Box -> Bool
isEmpty (Empty _) = True
isEmpty _ = False

-- Devuelve la cantidad de fichas en una casilla.
getAmmountOfChips :: Box -> Int
getAmmountOfChips (Empty _) = 0
getAmmountOfChips (Stack _ chs) = length chs

-- Devuelve la casilla con más fichas de un conjunto de fichas.
getLargestBox :: [Box] -> Box
getLargestBox [x] = x
getLargestBox xs = largest
   where
      lengths = map getAmmountOfChips xs
      maxStackLength = maximum lengths
      idx = fromJust (elemIndex maxStackLength lengths)
      largest = xs!!idx

-- Busca una casilla en el casillero.
getBox :: String -> [Box] -> Box
getBox s b = getLargestBox filtered
   where
      mapped = map (matchBox s) b
      filtered = filter (notNullStr) mapped
      notNullStr (Empty str) = str /= ""
      notNullStr (Stack str _) = str /= ""
      matchBox s box@(Empty str) = if s == str then box else Empty ""
      matchBox s box@(Stack str _) = if s == str then box else Empty ""

-- Devuelve True si se empata.
tie :: TakGame -> Bool
tie (Board b _ _) = sameAmmount
   where
      sameAmmount = countW == countB
      countW = count b WhitePlayer
      countB = count b BlackPlayer

-- Cuenta las ocurrencias de un jugador en el casillero.
count :: [Box] -> TakPlayer -> Int
count boxes player = length (filter (topIsFlatPlayer player) boxes)

-- Devuelve True si la pila pertenece a un jugador y su tope es una plana.
topIsFlatPlayer :: TakPlayer -> Box -> Bool
topIsFlatPlayer player (Stack _ ((Stone p):_)) = (p == player)
topIsFlatPlayer player _ = False

-- Devuelve el jugador opuesto.
oppositePlayer :: TakPlayer -> TakPlayer
oppositePlayer WhitePlayer = BlackPlayer
oppositePlayer BlackPlayer = WhitePlayer

-- Resta una ficha a un jugador.
substractChip :: TakPlayer -> (PlayerChips, PlayerChips) -> (PlayerChips, PlayerChips)
substractChip (WhitePlayer) ((Whites w), b) = (Whites (w - 1), b)
substractChip (BlackPlayer) (w, (Blacks b)) = (w, Blacks (b - 1))
substractChip _ _ = error errorWrongChipsFormat

-- Devuelve el jugador de la ficha.
getPlayer :: Chip -> TakPlayer
getPlayer (Wall p) = p
getPlayer (Stone p) = p

-- Devuelve True si el movimiento no es en línea recta.
checkNotStraight :: String -> String -> Bool
checkNotStraight s1 s2 = diffChar && diffDigit
   where
      diffChar = (head s1) /= (head s2)
      diffDigit = (tail s1) /= (tail s2)

-- Devuelve True si una casilla debería ser parte del recorrido de una fila.
takeSameRow :: Box -> String -> String -> Bool
takeSameRow (Empty s) si sf = (head si == head sf) && (head si == head s) && (condition)
   where
      ni = read (tail si) :: Int
      nf = read (tail sf) :: Int
      ns = read (tail s) :: Int
      condition = if ni < nf then (ns > ni) && (ns <= nf) else (ns < ni) && (ns >= nf)

takeSameRow (Stack s _) si sf = (head si == head sf) && (head si == head s) && (condition)
   where
      ni = read (tail si) :: Int
      nf = read (tail sf) :: Int
      ns = read (tail s) :: Int
      condition = if ni < nf then (ns > ni) && (ns <= nf) else (ns < ni) && (ns >= nf)

-- Devuelve True si una casilla debería ser parte del recorrido de una columna.
takeSameColumn :: Box -> String -> String -> Bool
takeSameColumn (Empty s) si sf = (tail si == tail sf) && (tail si == tail s) && (condition)
   where
      ni = ord (head si)
      nf = ord (head sf)
      ns = ord (head s)
      condition = if ni < nf then (ns > ni) && (ns <= nf) else (ns < ni) && (ns >= nf)

takeSameColumn (Stack s _) si sf = (tail si == tail sf) && (tail si == tail s) && (condition)
   where
      ni = ord (head si)
      nf = ord (head sf)
      ns = ord (head s)
      condition = if ni < nf then (ns > ni) && (ns <= nf) else (ns < ni) && (ns >= nf)

-- Devuelve el camino recto entre dos casillas.
getPath :: String -> String -> [Box] -> [Box]
getPath i f b = if sameRow then res1 else res2
   where
      sameRow = (head i == head f)
      resRow = [x | x <- b, takeSameRow x i f]
      resColumn = [x | x <- b, takeSameColumn x i f]
      res1 = if i > f then reverse resRow else resRow
      res2 = if i > f then reverse resColumn else resColumn

-- Devuelve True si hay una pared en el camino.
checkWallInPath :: [Box] -> Bool
checkWallInPath path = or (map isWall path)
   where
      isWall (Stack _ ((Wall _):_)) = True
      isWall _ = False

-- Devuelve true si hay un error en el camino.
checkPathError :: Box -> [Box] -> [Int]-> [Box] -> Bool
checkPathError (Stack _ stck) path des b = or [condit1, condit2, condit3, condit4]
   where
      sumDes = foldr1 (+) des
      lenDes = length des
      lenPath = length path
      lenBoxes = length b
      lenStack = length stck
      n = floor (sqrt (fromIntegral lenBoxes))
      condit1 = or (map (\x -> x < 1) des)
      condit2 = sumDes > n
      condit3 = lenDes >= n
      condit4 = sumDes > lenStack

-- Apila una casilla.
modifyBox :: [Int] -> [Box] -> Box -> Int -> Box
modifyBox des path (Stack s chs) i = newStack
   where
      newStack = addChips taken (path!!i)
      sumDes = sum des
      firstOnHand = take sumDes chs
      firstDrop = if (length firstOnHand) == des!!0
                  then firstOnHand
                  else drop ((length firstOnHand) - des!!0) firstOnHand
      secondOnHand = take (sumDes - des!!0) chs
      secondDrop = if (length secondOnHand) == des!!1
                   then secondOnHand
                   else drop ((length secondOnHand) - des!!1) secondOnHand
      thirdOnHand = take (sumDes - des!!0 - des!!1) chs
      thirdDrop = if length thirdOnHand == des!!2
                  then thirdOnHand
                  else drop (des!!2) thirdOnHand
      taken = case i of
                  0 -> firstDrop
                  1 -> secondDrop
                  2 -> thirdDrop
      addChips app (Empty s) = Stack s app
      addChips app (Stack s cs) = Stack s (app ++ cs)

-- Mueve una pila y actualiza el tablero.
replacePath :: [Int] -> [Box] -> Box -> [Box] -> [Box]
replacePath des path stck@(Stack s chs) b = resultB
   where
      is3x3 = (length b) == 9
      modifiedPath = map (modifyBox des path stck) [0..((length des) - 1)]
      sumPath = sum des
      modifiedStack = if (length chs) > sumPath then Stack s (drop sumPath chs) else Empty s
      bWithStack = map (ifStackReplace stck) b -- Mapear y reemplazar la pila en el tablero.
      ifStackReplace (Stack s1 _) b2@(Stack s2 _) = if s1 == s2
                                                    then modifiedStack
                                                    else b2
      ifStackReplace _ b2 = b2
      bWithPathOfPaths = map (ifInPathReplace modifiedPath) bWithStack -- Mapear y reemplazar el camino modificado.
      ifInPathReplace p box = map (ifMatchReplace box) p
      ifMatchReplace b1@(Stack s1 _) b2@(Stack s2 _) = if s1 == s2
                                                       then b2
                                                       else b1
      ifMatchReplace b1@(Empty s1) b2@(Stack s2 _) = if s1 == s2
                                                     then b2
                                                     else b1
      listCoords3x3 = [x:[y] | x <- ['A'..'C'], y <- ['1'..'3']]
      listCoords4x4 = [x:[y] | x <- ['A'..'D'], y <- ['1'..'4']]
      listOfCoords = if is3x3 then listCoords3x3 else listCoords4x4
      resultB = zipWith getBox listOfCoords bWithPathOfPaths

{- Actualiza el tablero a partir de un movimiento válido.
   Si el movimiento no es válido se arroja un error. -}
performAction :: TakGame -> (TakPlayer, TakAction) -> TakGame
-- Place.
performAction (Board _ (Whites 0, _) _) (WhitePlayer, (Place _ _)) = error errorLacksChips -- Fichas insuficientes.
performAction (Board _ (_, Blacks 0) _) (BlackPlayer, (Place _ _)) = error errorLacksChips -- Fichas insuficientes.
performAction (Board b chs _) (p, (Place ch s)) | isEmpty c = Board newB newChs newP -- Colocar correctamente.
   where
      c = getBox s b
      newP = oppositePlayer p
      newChs = substractChip p chs
      newB = putOnEmpty ch b c
      putOnEmpty r xs x = map (ifMatchReplace r x) xs
      ifMatchReplace rep (Empty str1) nm@(Empty str2) = if str2 == str1
                                                        then Stack str1 [rep]
                                                        else nm
      ifMatchReplace _ _ nm = nm

performAction (Board b chs _) (p, (Place ch c)) = error errorPlaceNoEmpty -- Colocar en lugar no vacío.
-- Move.
performAction (Board b _ _) (_, (Move s _ _)) | isEmpty c = error errorMoveEmpty -- Mover una casilla vacía.
   where
      c = getBox s b

performAction (Board b _ _) (p, (Move s _ _)) -- Mover una casilla no controlable.
   | (top /= p) = error errorNotYourStack
   where
      (Stack _ chs) = getBox s b
      top = getPlayer (head chs)

performAction _ (_, (Move si sf _)) -- Desplazamiento no recto.
   | notStraight = error errorNotStraight
   where
      notStraight = checkNotStraight si sf

performAction g (_, (Move si sf _)) | si == sf = g -- Se deja la pila en el mismo lugar.

performAction (Board b _ _) (_, (Move si sf _)) -- Pared en camino.
   | checkWallInPath path = error errorWallInPath
   where
      path = getPath si sf b

performAction (Board b _ _) (_, (Move si sf des)) -- Algún error en el desplazamiento.
   | checkPathError stack path des b = error errorPath
   where
      path = getPath si sf b
      stack = getBox si b

performAction (Board b cs _) (p, (Move si sf des)) = Board newB cs newP -- Movimiento correcto.
   where
      stack = getBox si b
      newP = oppositePlayer p
      path = getPath si sf b
      newB = replacePath des path stack b

-- Devuelve True si un jugador se quedó sin fichas.
noChips :: (PlayerChips, PlayerChips) -> Bool
noChips (Whites w, Blacks b) = or [w == 0, b == 0]
noChips _ = error errorWrongChipsFormat

-- Traduce coordenadas del eje cartesiano a coordenadas del tablero.
translateCoords :: Int -> (Int, Int) -> String
translateCoords size (x, y) = (toRow y):(toColumn x)
   where
      toRow a
         | a == (size - 1) = 'A'
         | a == (size - 2) = 'B'
         | a == (size - 3) = 'C'
         | a == (size - 4) = 'D'
         | otherwise = error errorInvalidCoords

      toColumn a = show (a + 1)

-- Posibles caminos derechos 3x3.
rows3x3 = [x:[y] | x <- ['A'..'C'], y <- ['1'..'3']] -- -> [A1, A2, A3, A4, B1, B2, B3...]
straightRows3x3 = [(take 3 rows3x3)] : [(take 3 (drop 3 rows3x3))] : [(drop 6 rows3x3)] : []
columns3x3 = [x:[y] | y <- ['1'..'3'], x <- ['A'..'C']] -- -> [A1, B1, C1, A2, B2, ...]
straightColumns3x3 = [(take 3 columns3x3)] : [(take 3 (drop 3 columns3x3))] : [(drop 6 columns3x3)] : []
straightRoads3x3 = (concat straightRows3x3) ++ (concat straightColumns3x3)

-- Posibles caminos derechos 4x4.
rows4x4 = [x:[y] | x <- ['A'..'D'], y <- ['1'..'4']] -- -> [A1, A2, A3, A4, B1, B2, B3...]
straightRows4x4 = [(take 4 rows4x4)]:[(take 4 (drop 4 rows4x4))]:[(take 4 (drop 8 rows4x4))]:[(drop 12 rows4x4)]:[]
columns4x4 = [x:[y] | y <- ['1'..'4'], x <- ['A'..'D']] -- -> [A1, B1, C1, D1, A2, B2, ...]
straightColumns4x4 = [(take 4 columns4x4)]:[(take 4 (drop 4 columns4x4))]:[(take 4 (drop 8 columns4x4))]:[(drop 12 columns4x4)]:[]
straightRoads4x4 = (concat straightRows4x4) ++ (concat straightColumns4x4)

-- Posibles caminos 3x3.
roads3x3From00 = notSquaredWalks 3 [[(0,0)]]
roads3x3From01 = notSquaredWalks 3 [[(0,1)]]
roads3x3From02 = notSquaredWalks 3 [[(0,2)]]
roads3x3From12 = notSquaredWalks 3 [[(1,2)]]
roads3x3From22 = notSquaredWalks 3 [[(2,2)]]

walks3x3Coords = roads3x3From00 ++ roads3x3From01 ++ roads3x3From02 ++ roads3x3From12 ++ roads3x3From22
mappedCoords3x3 = map (map (translateCoords 3)) walks3x3Coords
walks3x3 = mappedCoords3x3 ++ straightRoads3x3

-- Posibles caminos 4x4.
roads4x4From00 = notSquaredWalks 4 [[(0,0)]]
roads4x4From01 = notSquaredWalks 4 [[(0,1)]]
roads4x4From02 = notSquaredWalks 4 [[(0,2)]]
roads4x4From03 = notSquaredWalks 4 [[(0,3)]]
roads4x4From13 = notSquaredWalks 4 [[(1,3)]]
roads4x4From23 = notSquaredWalks 4 [[(2,3)]]
roads4x4From33 = notSquaredWalks 4 [[(3,3)]]

walks4x4Coords = roads4x4From00 ++ roads4x4From01 ++ roads4x4From02 ++ roads4x4From03 ++ roads4x4From13 ++ roads4x4From23 ++ roads4x4From33
mappedCoords4x4 = map (map (translateCoords 4)) walks4x4Coords
walks4x4 = mappedCoords4x4 ++ straightRoads4x4

-- Devuelve True si el camino es ganador.
verifyPath :: TakPlayer -> [Box] -> Bool
verifyPath p path = and (map (topIsFlatPlayer p) path)

-- Retorna (True, p) si se cumple una condición de fin de juego y ganó p.
endGame :: TakGame -> (Bool, TakPlayer) -- Bool
endGame (Board _ chs p) | noChips chs = (True, p) -- Un jugador sin fichas.
endGame (Board b _ p) = (res, winner)
   where
      p' = oppositePlayer p
      size = if (length b) == 9 then 3 else 4
      possibleWalks = if size == 3 then walks3x3 else walks4x4
      boxesRoad = map (map (`getBox` b)) possibleWalks
      resP = or (map (verifyPath p) boxesRoad)
      resP' = or (map (verifyPath p') boxesRoad)
      res = resP || resP'
      winner = if resP && resP' then p else (if resP then p else p')

-- Instancias con formatos.
instance Show TakAction where
   show (Move ini fin des) = "Mover "++ini++" a "++fin++" dejando "++show des
   show (Place ficha lugar) = "Colocar "++show ficha++" en "++lugar
 
instance Show PlayerChips where
   show (Whites w) = "Fichas blancas: "++(show w)
   show (Blacks b) = "Fichas negras: "++(show b)

instance Show Chip where
   show (Wall player) = "||"++(show player)++"||"
   show (Stone player) = "__"++(show player)++"__"

instance Show Box where
   show (Empty s) = s
   show (Stack s _) = s

sbs_ = showBoxes
sb_ = showBox
ap = "\nJugador activo: "
instance Show TakGame where
   show (Board b cs p) = '\n':(sbs_ b)++"\n"++(concat (map sb_ b))++('\n':(show (fst cs))++('\n':(show (snd cs))))++ap++(show p)++"\n"

-- El estado inicial del juego Tak con un tablero de 3x3, con el tablero vacío.
beginning3x3 :: TakGame
beginning3x3 = Board (emptyBoard3x3) (Whites 10, Blacks 10) firstPlayer

-- El estado inicial del juego Tak con un tablero de 4x4, con el tablero vacío.
beginning4x4 :: TakGame
beginning4x4 = Board (emptyBoard4x4) (Whites 15, Blacks 15) firstPlayer

{-La lista debe incluir una y solo una tupla para
cada jugador. Si el jugador está activo, la lista asociada debe incluir todos sus posibles movimientos para
el estado de juego dado. Sino la lista debe estar vacía.-}
actions :: TakGame -> [(TakPlayer, [TakAction])]
-- Primeros dos turnos.
actions (Board board (Whites w, Blacks b) p)
   | or [w==10,b==10,w==15,b==15] = [(p, listPlacesP), (p', listPlacesP')]
   where
      p' = oppositePlayer p
      empties = filter isEmpty board
      listPlacesP = concat (map (getPlacesFor p) empties)
      listPlacesP' = concat (map (getPlacesFor p') empties)
      getPlacesFor pl (Empty s) = (Place (Stone (oppositePlayer pl)) s):[(Place (Wall (oppositePlayer pl)) s)]

actions (Board b chs p) = [(p, actionsOfP), (p', actionsOfP')]
   where
      size = if length b == 9 then 3 else 4
      p' = oppositePlayer p
      -- Places.
      empties = filter isEmpty b
      listPlacesP = concat (map (getPlacesFor p) empties)
      listPlacesP' = concat (map (getPlacesFor p') empties)
      getPlacesFor pl (Empty s) = (Place (Stone pl) s):[(Place (Wall pl) s)]

      -- Moves.
      stacks = filter (not . isEmpty) b
      stacksP = filter (isFromPlayer p) stacks
      stacksP' = filter (isFromPlayer p') stacks
      rMovesP = concat (concat (map getRM stacksP))
      rMovesP' = concat (concat (map getRM stacksP'))
      pTraces = possibleTraces size
      getRM (Stack s cs) = map (linkTraceWithMovement (length cs) s pTraces) (realMovements s (length cs) (possibleMovements (head s) (last s) size))
      
      -- Todas las acciones.
      actionsOfP = listPlacesP ++ rMovesP
      actionsOfP' = listPlacesP' ++ rMovesP'

{-Esta función aplica una acción sobre un
estado de juego dado, y retorna el estado resultante. Se debe levantar un error si el jugador dado no es el
jugador activo, si el juego está terminado, o si la acción no es realizable.-}
next :: TakGame -> (TakPlayer, TakAction) -> TakGame
next g t@(p, _)
   | (fromJust (activePlayer g)) == p = performAction g t -- Realizar acción correspondiente.
   | otherwise = error errorNotYourTurn

{-Si el juego está terminado retorna el resultado de juego
para cada jugador. Este valor es 1 si el jugador ganó, -1 si perdió y 0 si se empató. Si el juego no está
terminado, se debe retornar una lista vacía.-}
result :: TakGame -> [(TakPlayer, Int)]
result g | not (fst (endGame g)) = [] -- No finalizado.
result g@(Board _ chs _) | (noChips chs) && (tie g) = zip players [0, 0] -- Empate.
result g@(Board b _ p) = if (p == BlackPlayer) then zip players [1, -1] else zip players [-1, 1]

{-Retorna el puntaje para todos los jugadores en el estado
de juego dado. Esto es independiente de si el juego está terminado o no.-}
score :: TakGame -> [(TakPlayer, Int)]
score (Board boxes _ _) = zip [player1, player2] [s1, s2]
    where
        s1 = count boxes player1
        s2 = count boxes player2

{-Convierte el estado de juego a un texto que puede ser impreso en la
consola para mostrar el tablero y demás información de la partida.-}
showGame :: TakGame -> String
showGame board = show board

{-Convierte una acción a un texto que puede ser impreso en la
consola para mostrarla.-}
showAction :: TakAction -> String
showAction a = show a

{-Obtiene una acción a partir de un texto que puede haber sido
introducido por el usuario en la consola.-}
readAction :: String -> TakAction
readAction s
   | isMove = move
   | isPlace = place
   | otherwise = error errorMoveNotFound
   where
      isMove = isSubsequenceOf "Mover" s
      isPlace = isSubsequenceOf "Colocar" s
      place = Place chip f
      player = if isSubsequenceOf "WhitePlayer" s then WhitePlayer else BlackPlayer
      chip = if elem '|' s then Wall player else Stone player
      f = if isMove then take 2 (drop 11 s) else drop 27 s
      i = take 2 (drop 6 s)
      des = read (drop 22 s) :: [Int]
      move = Move i f des

{- Determina a cuál jugador le toca mover, dado un estado de juego.-}
activePlayer :: TakGame -> Maybe TakPlayer
activePlayer g | fst (endGame g) = Nothing
activePlayer (Board _ _ player) = Just player

{-- Match controller -------------------------------------------------------------------------------

Código de prueba. Incluye una función para correr las partidas y dos agentes: consola y aleatorio.

-}
type TakAgent = TakGame -> IO (Maybe TakAction)

{- La función ´runMatch´ corre la partida completa a partir del estado de juego dado, usando los dos 
agentes dados. Retorna una tupla con los puntajes (score) finales del juego.
-}
runMatch :: (TakAgent, TakAgent) -> TakGame -> IO [(TakPlayer, Int)]
runMatch ags@(ag1, ag2) g = do
   putStrLn (showGame g)
   case (activePlayer g) of
      Nothing -> return $ result g
      Just p -> do
         let ag = [ag1, ag2] !! (fromJust (elemIndex p players))
         move <- ag g
         runMatch ags (Tak.next g (p, fromJust move))

{- La función ´runOnConsole´ ejecuta toda la partida a partir del estado inicial usando dos agentes
de consola.
-}
runOnConsole :: TakGame -> IO [(TakPlayer, Int)]
runOnConsole g = do
   runMatch (consoleAgent WhitePlayer, consoleAgent BlackPlayer) g

run3x3OnConsole :: IO [(TakPlayer, Int)]
run3x3OnConsole = runOnConsole beginning3x3

run4x4OnConsole :: IO [(TakPlayer, Int)]
run4x4OnConsole = runOnConsole beginning4x4

{- El agente de consola ´consoleAgent´ muestra el estado de juego y los movimientos disponibles por
consola, y espera una acción por entrada de texto.
-}
consoleAgent :: TakPlayer -> TakAgent
consoleAgent player state = do
   let moves = fromJust (lookup player (actions state))
   if null moves then do
      putStrLn "No moves!"
      getLine
      return Nothing
   else do
      putStrLn ("Select one move:" ++ (drop 1 (concat [", "++ show m | m <- moves])))
      line <- getLine
      let input = readAction line
      if elem input moves then return (Just input) else do 
         putStrLn "Invalid move!"
         consoleAgent player state

randomAgent :: TakPlayer -> TakAgent
randomAgent player state = do
    let moves = fromJust (lookup player (actions state))
    if null moves then do
       putStrLn "No moves!"
       return Nothing
    else do
       i <- randomRIO (0, (length moves) - 1)
       return (Just (moves !! i))

-- Fin
