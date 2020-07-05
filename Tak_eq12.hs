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
import Data.Char
import System.Random

{- Es posible que el paquete `System.Random` no esté disponible si se instaló el core de la Haskell 
Platform en el sistema. Para instalarlo, ejecutar los siguientes comandos:

> cabal update
> cabal install random

La herramienta `cabal` es un manejador de paquetes usado por la plataforma Haskell. Debería estar 
disponible junto con el `ghci`.

-}

{-- Lógica de juego --------------------------------------------------------------------------------

Funciones de marca sin ninguna implementación útil. Reemplazar por el código apropiado o por imports
a los módulos necesarios.
-}

data TakPlayer = WhitePlayer | BlackPlayer deriving (Eq, Show, Enum, Bounded)
data Chip = Wall TakPlayer | Stone TakPlayer -- Fichas.
data Box = Empty String | Stack String [Chip] deriving (Eq) -- Casillas. Son vacías o pilas de 1 o más fichas.
data PlayerChips = Whites Int | Blacks Int -- Cantidad de fichas de los jugadores.
data TakGame = Board [Box] (PlayerChips, PlayerChips) TakPlayer -- Tablero: /Casillas/Fichas/JugadorActivo.
data TakAction = Move Box Box [Int] | Place Chip Box -- Posibles movimientos.

-- Util.
tableroStrings3x3 = [x ++ y | x <- ["A", "B", "C"], y <- ["1", "2", "3"]]
tableroVacio3x3 = [Empty s | s <- tableroStrings3x3]

tableroStrings4x4 = [x ++ y | x <- ["A", "B", "C", "D"], y <- ["1", "2", "3", "4"]]
tableroVacio4x4 = [Empty s | s <- tableroStrings4x4]


player1 = WhitePlayer -- Jugador de las fichas blancas.
player2 = BlackPlayer -- Jugador de las fichas negras.
players = [player1, player2] -- Jugadores.

firstPlayer = player1 -- Jugador inicial.

-- Mensajes de errores:
errorPlaceNoEmpty = "Solo puedes colocar fichas en espacios vacíos."
errorNotYourTurn = "Le toca al otro jugador."
errorLacksChips = "No tienes más fichas."
errorWrongChipsFormat = "El formato de la tupla de fichas debe ser (Whites w, Blacks b)" -- No debería pasar nunca.
errorEndGame = "El juego ya ha terminado."
errorMoveEmpty = "No se puede mover una casilla vacía."
errorNotYourStack = "Esta pila no te pertenece."
errorNotStraight = "Movimiento inválido."
errorWallInPath = "Movimiento no válido, hay un muro en el camino."
errorPath = "Desplazamiento no válido."

-- Format para una tupla de fichas de los jugadores.
showChips :: (PlayerChips, PlayerChips) -> String
showChips (a,b) = (show a) ++ ('\n':(show b))

-- Format para un tablero.
showBoxes :: [Box] -> String
showBoxes xs = concat (if largo == 9 then separarEnN 3 lista else separarEnN 4 lista)
   where
      lista = map show xs -- [A1, A2, A3, \n, B1, B2, B3, \n, C1, C2, C3]
      largo = length lista
      separarEnN n [] = []
      separarEnN n lista = (take n lista) ++ ("\n":(separarEnN n (drop n lista)))

-- Format para una casilla.
showBox :: Box -> String
showBox (Empty s) = s ++ ": []\n"
showBox (Stack s fichas) = s ++ ": " ++ (show (map show fichas)) ++ "\n"

-- Cuenta las ocurrencias de un jugador en el casillero.
count :: [Box] -> TakPlayer -> Int
count boxes player = length (filter (isPlayer player) boxes)

-- Devuelve True si es un jugador.
isPlayer :: TakPlayer -> Box -> Bool -- Extras
isPlayer player (Stack ((Stone p):_)) = (p == player)
isPlayer player _ = False

-- Devuelve el jugador opuesto.
oppositePlayer :: TakPlayer -> TakPlayer
oppositePlayer WhitePLayer = BlackPLayer
oppositePlayer BlackPLayer = WhitePLayer

-- Resta una ficha a un jugador.
substractChip :: TakPlayer -> (PlayerChips, PlayerChips) -> (PlayerChips, PlayerChips)
substractChip (WhitePLayer) ((Whites w), b) = (Whites (w - 1), b)
substractChip (BlackPLayer) (w, (Blacks b)) = (w, Blacks (b - 1))
substractChip _ _ = error errorWrongChipsFormat

-- Devuelve el jugador de la ficha.
getPlayer :: Chip -> TakPlayer
getPlayer (Wall WhitePLayer) = WhitePLayer
getPlayer (Wall BlackPLayer) = BlackPLayer
getPlayer (Stone WhitePLayer) = WhitePLayer
getPlayer (Stone BlackPlayer) = BlackPLayer

-- Devuelve True si el movimiento es en línea recta.
checkNotStraight :: String -> String -> Bool
checkNotStraight s1 s2 = diffChar && diffDigit
   where
      diffChar = (head s1) /= (head s2)
      diffDigit = (tail s1) /= (tail s2)

-- Devuelve True si una casilla debería ser parte del recorrido de una fila.
takeSameRow :: Box -> String -> String -> Bool
takeSameRow (Empty s) si sf = (head si == head sf) && (condition)
   where
      ni = read (tail si) :: Int
      nf = read (tail sf) :: Int
      ns = read (tail s) :: Int
      condition = if ni < nf then (ns > ni) && (ns <= nf) else (ns < ni) && (ns >= nf)
takeSameRow (Stack s _) si sf = (head si == head sf) && (condition)
   where
      ni = read (tail si) :: Int
      nf = read (tail sf) :: Int
      ns = read (tail s) :: Int
      condition = if ni < nf then (ns > ni) && (ns <= nf) else (ns < ni) && (ns >= nf)

-- Devuelve True si una casilla debería ser parte del recorrido de una columna.
takeSameColumn :: Box -> String -> String -> Bool
takeSameColumn (Empty s) si sf = (tail si == tail sf) && (condition)
   where
      ni = ord (head si)
      nf = ord (head sf)
      ns = ord (head s)
      condition = if ni < nf then (ns > ni) && (ns <= nf) else (ns < ni) && (ns >= nf)
takeSameColumn (Stack s _) si sf = (tail si == tail sf) && (condition)
   where
      ni = ord (head si)
      nf = ord (head sf)
      ns = ord (head s)
      condition = if ni < nf then (ns > ni) && (ns <= nf) else (ns < ni) && (ns >= nf)

-- Devuelve el camino recto entre dos casillas.
getPath :: Box -> Box -> [Box] -> [Box]
getPath (Stack i _) (Empty f) b = if sameRow
                                  then res1
                                  else res2
   where
      sameRow = (head i == head f)
      resRow = [x | x <- b, takeSameRow x i f]
      resColumn = [x | x <- b, takeSameColumn x i f]
      res1 = if i > f then reverse resRow else resRow
      res2 = if i > f then reverse resColumn else resColumn

getPath (Stack i _) (Stack f _) b = if sameRow
                                    then [x | x <- b, takeSameRow x i f]
                                    else [x | x <- b, takeSameColumn x i f]
   where
      sameRow = (head i == head f)

-- Devuelve True si hay una pared en el camino.
checkWallInPath :: [Box] -> Bool
checkWallInPath path = or (map isWall path)
   where
      isWall (Stack _ ((Wall _):_)) = True
      isWall _ = False

-- Devuelve true si hay un error en el camino.
checkPathError :: Box -> [Box] -> [Int]-> [Box] -> Bool
checkPathError (Stack _ stck) path des b = condit1 || condit2 || condit3 || condit4 || condit5
   where
      sumDes = foldr1 (+) des
      lenDes = length des
      lenPath = length path
      lenBoxes = length b
      lenStack = length stck
      n = floor (sqrt (fromIntegral lenBoxes))
      condit1 = or (map (\x -> x < 1) des) -- en 3x3 [1,0]
      condit2 = sumDes > n -- en 3x3 [2,2]
      condit3 = lenDes >= n -- en 3x3 [1,1,1,1] error de usuario, no sabe la regla.
      condit4 = sumDes > lenStack -- en 3x3 [2,2] con una pila de 1 error de usuario.
      condit5 = lenDes /= lenPath -- en 3x3 de A1 a A2 con des = [1,1,1,2,3,3] error de usuario.

-- Apila una casilla.
modifyBox :: [Int] -> [Box] -> Box -> Int -> Box
modifyBox des path (Stack s chs) i = newStack
   where
      taken = take (des!!i) chs
      newStack = addChips taken (path!!i)
      addChips app (Empty s) = Stack s app
      addChips app (Stack s cs) = Stack s (app ++ cs)

-- Desapilar una pila.
replacePath :: [Int] -> [Box] -> Box -> [Box] -> [Box]
replacePath des path stck@(Stack s chs) b = -- Devolver tablero.
   where
      modifiedPath d p stck = map (modifyBox d p stck) [0..((length d) - 1)]
      sumPath = foldr1 (+) des
      modifiedStack (Stack s cs) d = Stack s (drop sumPath cs)
      -- Mapear y reemplazar la pila en el tablero.
      -- Mapear y reemplazar la stack modificada.

-- Actualiza el vector de fichas a partir de un movimiento válido.
performAction :: TakGame -> (TakPlayer, TakAction) -> TakGame
-- Place.
performAction (Board _ (Whites 0, _) _) (WhitePLayer, (Place _ _)) = error errorLacksChips
performAction (Board _ (_, Blacks 0) _) (BlackPLayer, (Place _ _)) = error errorLacksChips
performAction g@(Board b chs _) t@(p, (Place ch c@(Empty s))) = Board newB newChs newP
   where
      newP = oppositePlayer p
      newChs = substractChip p chs
      newB = putOnEmpty ch b c
      putOnEmpty r xs x = map (ifMatchReplace r x) xs --  "A2" [Empty "A1", Empty "A2", Empty "A3"...]
      ifMatchReplace rep (Empty str1) nm@(Empty str2) = if str2 == str1
                                                        then Stack str1 [rep]
                                                        else nm
      ifMatchReplace _ _ nm = nm

performAction (Board b chs _) (p, (Place ch c)) = error errorPlaceNoEmpty
-- Move.
performAction _ (_, (Move (Empty _) _ _)) = error errorMoveEmpty
performAction (Board _ _ _) (p, (Move (Stack _ chs) _ _))
   | (top /= p) = error errorNotYourStack
   where
      top = getPlayer (head chs)

performAction (Board _ _ _) (_, (Move (Stack si _) (Empty sf) _))
   | notStraight = error errorNotStraight
   where
      notStraight = checkNotStraight si sf

performAction (Board _ _ _) (_, (Move (Stack si _) (Stack sf _) _))
   | notStraight = error errorNotStraight
   where
      notStraight = checkNotStraight si sf

performAction g (_, (Move (Stack si _) (Stack sf _) _)) | s1 == s2 = g
performAction (Board b _ _) (_, (Move b1@(Stack si _) b2@(Empty sf) _))
   | checkWallInPath path = error errorWallInPath
   where
      path = getPath b1 b2 b

performAction (Board b _ _) (_, (Move b1@(Stack si _) b2@(Stack sf _) _))
   | checkWallInPath path = error errorWallInPath
   where
      path = getPath b1 b2 b

performAction (Board b _ _) (_, (Move b1@(Stack si _) b2@(Empty _) des))
   | checkPathError b1 path des b = error errorPath
   where
      path = getPath b1 b2 b

performAction (Board b _ _) (_, (Move b1@(Stack si _) b2@(Stack _ _) des))
   | checkPathError b1 path des b = error errorPath
   where
      path = getPath b1 b2 b

performAction (Board b cs _) (p, (Move b1@(Stack _ chs) b2@(Empty _) des)) = Board newB cs newP
   where
      newP = oppositePlayer p
      path = getPath b1 b2 b
      newB = replacePath des path b1 b


-- Retorna True si se cumple una condición de fin de juego.
endGame :: TakGame -> Bool

{-

A1 A2 A3
B1 B2 B3
C1 C2 C3

A1: [ |BlackPLayer| , __WhitePLayer__]
A2: [__WhitePlayer__]
A3: []
.
.
.
C3:

Fichas blancas: 10
Fichas negras: 10
Jugador actual: WhitePlayer
-}

--------------------------- Move A1 A3 [1, 1] = Desapilar A1 a A3 dejando: [1, 1]
--------------------------- Place (Stone WhitePLayer) A2 = Colocar __WhitePlayer__ en A2
-- Instancias.
instance Show TakAction where
   show (Move ini fin des) = "Desapilar " ++ show ini ++ " a " ++ show fin ++ " dejando: " ++ show des
   show (Place ficha lugar) = "Colocar " ++ show ficha ++ " en " ++ show lugar
 
instance Show PlayerChips where
   show (Whites w) = "Fichas blancas: " ++ (show w)
   show (Blacks b) = "Fichas negras: " ++ (show b)

instance Show Chip where
   show (Wall player) = " |" ++ (show player) ++ "| " --"pared del jugador " ++ show player --
   show (Stone player) = " __" ++ (show player) ++ "__ " --"plana del jugador " ++ show player 

instance Show Box where
   show (Empty s) = s
   show (Stack s _) = s







-- (fst,snd)
instance Show TakGame where
   show (Board boxes chips player) = (showBoxes boxes) ++ "\n" ++ (concat (map showBox boxes)) ++ ('\n':(show (fst chips)) ++ ('\n':(show (snd chips)))) ++ ('\n':"Jugador activo: " ++ (show player)) -- "Jugador activo: " ++ (show player) ++ ('\n':(showChips chips)) ++ "\nTablero:\n" ++ (showBoxes boxes)

-- Funciones del Tak.

-- El estado inicial del juego Tak con un tablero de 3x3, con el tablero vacío.
beginning3x3 :: TakGame
beginning3x3 = Board (tableroVacio3x3) (Whites 10, Blacks 10) firstPlayer

-- El estado inicial del juego Tak con un tablero de 4x4, con el tablero vacío.
beginning4x4 :: TakGame
beginning4x4 = Board (tableroVacio4x4) (Whites 15, Blacks 15) firstPlayer

{-La lista debe incluir una y solo una tupla para
cada jugador. Si el jugador está activo, la lista asociada debe incluir todos sus posibles movimientos para
el estado de juego dado. Sino la lista debe estar vacía.-}
actions :: TakGame -> [(TakPlayer, [TakAction])]
actions (TakGame f) = zip players [if f then [] else [TakAction], []] --TODO

{-Esta función aplica una acción sobre un
estado de juego dado, y retorna el estado resultante. Se debe levantar un error si el jugador dado no es el
jugador activo, si el juego está terminado, o si la acción no es realizable.-}
next :: TakGame -> (TakPlayer, TakAction) -> TakGame
next g (Nothing, _) = error errorEndGame
next g t@(p, _)
   | (fromJust (activePlayer g)) == p = performAction g t -- hacer acción correspondiente.
   | otherwise = error errorNotYourTurn

{-Si el juego está terminado retorna el resultado de juego
para cada jugador. Este valor es 1 si el jugador ganó, -1 si perdió y 0 si se empató. Si el juego no está
terminado, se debe retornar una lista vacía.-}
result :: TakGame -> [(TakPlayer, Int)]
result (TakGame f) = zip players (if f then [] else [1, -1]) --TODO

{-Retorna el puntaje para todos los jugadores en el estado
de juego dado. Esto es independiente de si el juego está terminado o no.-}
score :: TakGame -> [(TakPlayer, Int)]
score (Board boxes _ _) = zip [player1, player2] [s1, s2] -- [(player1, s1), (player2, s2)]
    where
        s1 = count boxes player1
        s2 = count boxes player2

{-Convierte el estado de juego a un texto que puede ser impreso en la
consola para mostrar el tablero y demás información de la partida.-}
showBoard :: TakGame -> String
showBoard board = show board

{-Convierte una acción a un texto que puede ser impreso en la
consola para mostrarla.-}
showAction :: TakAction -> String
showAction a = show a

{-Obtiene una acción a partir de un texto que puede haber sido
introducido por el usuario en la consola.-}
readAction :: String -> TakAction
--readAction s = 

{- Determina a cuál jugador le toca mover, dado un estado de juego. OJO, SEGURO DESPUÉS SE DEVUELVA NOPLAYER PARA INDICAR QUE SE FINALIZA
EL JUEGO (VER EL TIK-TAK-TOE). -}
activePlayer :: TakGame -> Maybe TakPlayer
activePlayer g | endGame g = Nothing
activePlayer (Board _ _ player) = Just player

{-activePlayer :: TakGame -> Maybe TakPlayer
activePlayer g = listToMaybe [p | (p, as) <- actions g, not (null as)]-}

--players :: [TakPlayer]
--players = [minBound..maxBound]

{-- Match controller -------------------------------------------------------------------------------

Código de prueba. Incluye una función para correr las partidas y dos agentes: consola y aleatorio.

-}
type TakAgent = TakGame -> IO (Maybe TakAction)

{- La función ´runMatch´ corre la partida completa a partir del estado de juego dado, usando los dos 
agentes dados. Retorna una tupla con los puntajes (score) finales del juego.
-}
runMatch :: (TakAgent, TakAgent) -> TakGame -> IO [(TakPlayer, Int)]
runMatch ags@(ag1, ag2) g = do
   putStrLn (showBoard g)
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
      putStrLn ("Select one move:" ++ concat [" "++ show m | m <- moves]) -- concat [", "]
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
