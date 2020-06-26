{- Adelanto de algunas cositas del proshect -}

-- Tipos data
data TakPlayer = WhitePlayer | BlackPlayer deriving (Eq, Show, Enum) -- Jugadores. -- Show?
data Chip = Wall TakPlayer | Stone TakPlayer
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


-- Lógica del juego.

-- El estado inicial del juego Tak con un tablero de 3x3, con el tablero vacío.
beginning3x3 :: TakGame
beginning3x3 = Board (replicate 9 Empty) (Whites 10, Blacks 10) firstPlayer

-- El estado inicial del juego Tak con un tablero de 4x4, con el tablero vacío.
beginning4x4 :: TakGame
beginning4x4 = Board (replicate 16 Empty) (Whites 15, Blacks 15) firstPlayer

{- Determina a cuál jugador le toca mover, dado un estado de juego. OJO, SEGURO DESPUÉS SE DEVUELVA NOPLAYER PARA INDICAR QUE SE FINALIZA
EL JUEGO (VER EL TIK-TAK-TOE). -}
activePlayer :: TakGame -> TakPlayer
activePlayer (Board _ _ player) = player

actions :: TakGame -> [(TakPlayer, [TakAction])] {-La lista debe incluir una y solo una tupla para
cada jugador. Si el jugador está activo, la lista asociada debe incluir todos sus posibles movimientos para
el estado de juego dado. Sino la lista debe estar vacía.-}

next :: TakGame -> (TakPlayer, TakAction) -> TakGame {-Esta función aplica una acción sobre un
estado de juego dado, y retorna el estado resultante. Se debe levantar un error si el jugador dado no es el
jugador activo, si el juego está terminado, o si la acción no es realizable.-}

result :: TakGame -> [(TakPlayer, Int)] {-Si el juego está terminado retorna el resultado de juego
para cada jugador. Este valor es 1 si el jugador ganó, -1 si perdió y 0 si se empató. Si el juego no está
terminado, se debe retornar una lista vacía.-}

score :: TakGame -> [(TakPlayer, Int)] {-Retorna el puntaje para todos los jugadores en el estado
de juego dado. Esto es independiente de si el juego está terminado o no.-}
score (Board boxes _ _) = zip [player1, player2] [s1, s2] -- [(player1, s1), (player2, s2)]
    where
        s1 = count boxes player1
        s2 = count boxes player2

showGame :: TakGame -> String {-Convierte el estado de juego a un texto que puede ser impreso en la
consola para mostrar el tablero y demás información de la partida.-}
showGame board = show board

showAction :: TakAction -> String {-Convierte una acción a un texto que puede ser impreso en la
consola para mostrarla.-}

readAction :: String -> TakAction {-Obtiene una acción a partir de un texto que puede haber sido
introducido por el usuario en la consola.-}


-- Agente Jugador.