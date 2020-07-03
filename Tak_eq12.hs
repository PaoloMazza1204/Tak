{- Tak ------------------------------------------------------------------------------------------
 
Plantilla de código para el proyecto del curso de 2020 de _Programación Funcional_ para las carreras 
de Ingeniería y Licenciatura en Informática de la FIT (UCU).
Los docentes no garantizan que este código esté libre de errores. De encontrar problemas, por favor
reportarlos a la cátedra.

Leonardo Val, Ignacio Pacheco.
-}
module Tak where

import Data.Maybe (fromJust, listToMaybe)
import Data.List (elemIndex)
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
data Box = Empty | Stack [Chip] deriving (Eq) -- Casillas. Son vacías o pilas de 1 o más fichas.
data PlayerChips = Whites Int | Blacks Int -- Cantidad de fichas de los jugadores.
data TakGame = Board [Box] (PlayerChips, PlayerChips) TakPlayer -- Tablero: /Casillas/Fichas/JugadorActivo.
data TakAction = Unstack Box Box [Int] | Move Box Box | Place Chip Box -- Posibles movimientos.

-- Util.
player1 = WhitePlayer -- Jugador de las fichas blancas.
player2 = BlackPlayer -- Jugador de las fichas negras.
players = [player1, player2] -- Jugadores.

firstPlayer = player1 -- Jugador inicial.

showChips :: (PlayerChips, PlayerChips) -> String
showChips (a,b) = (show a) ++ ('\n':(show b))

showBoxes :: [Box] -> String
showBoxes xs = concat (map show xs) -- Cambiar a ConcatWith

count :: [Box] -> TakPlayer -> Int
count boxes player = length (filter (isPlayer player) boxes)

isPlayer :: TakPlayer -> Box -> Bool -- Extras
isPlayer player (Stack ((Stone p):_)) = (p == player)
isPlayer player _ = False

-- Instancias.
instance Show PlayerChips where
    show (Whites w) = "Fichas blancas: " ++ (show w)
    show (Blacks b) = "Fichas negras: " ++ (show b)

instance Show Chip where
    show (Wall player) = " |" ++ (show player) ++ "| "
    show (Stone player) = " _" ++ (show player) ++ "_ "

instance Show Box where
    show Empty = " ... "
    show (Stack players) = "Pila de tope: " ++ (show (players !! 0))

instance Show TakGame where
    show (Board boxes chips player) = "Jugador activo: " ++ (show player)
    ++ ('\n':(showChips chips)) ++ "\nTablero:\n" ++ (showBoxes boxes)

-- Funciones del Tak.

-- El estado inicial del juego Tak con un tablero de 3x3, con el tablero vacío.
beginning3x3 :: TakGame
beginning3x3 = Board (replicate 9 Empty) (Whites 10, Blacks 10) firstPlayer

-- El estado inicial del juego Tak con un tablero de 4x4, con el tablero vacío.
beginning4x4 :: TakGame
beginning4x4 = Board (replicate 16 Empty) (Whites 15, Blacks 15) firstPlayer

{-La lista debe incluir una y solo una tupla para
cada jugador. Si el jugador está activo, la lista asociada debe incluir todos sus posibles movimientos para
el estado de juego dado. Sino la lista debe estar vacía.-}
actions :: TakGame -> [(TakPlayer, [TakAction])]
actions (TakGame f) = zip players [if f then [] else [TakAction], []] --TODO

{-Esta función aplica una acción sobre un
estado de juego dado, y retorna el estado resultante. Se debe levantar un error si el jugador dado no es el
jugador activo, si el juego está terminado, o si la acción no es realizable.-}
next :: TakGame -> (TakPlayer, TakAction) -> TakGame
next _ _ = TakGame True --TODO

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
showAction a = show a --TODO

{-Obtiene una acción a partir de un texto que puede haber sido
introducido por el usuario en la consola.-}
readAction :: String -> TakAction
readAction = read --TODO

{- Determina a cuál jugador le toca mover, dado un estado de juego. OJO, SEGURO DESPUÉS SE DEVUELVA NOPLAYER PARA INDICAR QUE SE FINALIZA
EL JUEGO (VER EL TIK-TAK-TOE). -}
activePlayer :: TakGame -> Maybe TakPlayer
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
      putStrLn ("Select one move:" ++ concat [" "++ show m | m <- moves])
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
