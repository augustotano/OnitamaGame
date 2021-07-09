{- Onitama ------------------------------------------------------------------------------------------
 
Plantilla de código para el proyecto del curso de 2021 de _Programación Funcional_ para las carreras 
de Ingeniería y Licenciatura en Informática de la FIT (UCU).
Los docentes no garantizan que este código esté libre de errores. De encontrar problemas, por favor
reportarlos a la cátedra.

Leonardo Val, Ignacio Pacheco. 
-}
module Onitama where

import Data.Maybe (fromJust, listToMaybe, maybeToList)
import Data.List (elemIndex, sort)
import System.Random

{-
moveCard :: OnitamaGame -> (Int, Int) -> (Int, Int) -> Maybe OnitamaCard
moveCard state@(OnitamaBoard _ aPlayer _ board cardsActivePlayer _ _) (w,x) (y,z) = if (not (null curatedAction)) then (Just (curatedAction !! 0)) else Nothing 
    where allActions = [(card, (a,b), (c,d)) |Action card (Piece (c,d) _ _) (Piece (a,b) _ _)  <- allPossibleActions state] 
          curatedAction = [card | (card, (a,b), (c,d)) <- allActions, (a,b) == (w,x) && (c,d) == (y,z)]
-}

moveCard :: OnitamaGame -> (Int, Int) -> (Int, Int) -> Maybe OnitamaCard
moveCard game (x,y) (w,z) = if (null possibleAction) then Nothing else (Just (possibleAction !! 0)) 
    where possibleAction = [card |Action card (Piece (c,d) _ _) (Piece (a,b) _ _)  <- allPossibleActions game, (a,b) == (x,y) && (c,d) == (w,z)] 

winningMove :: OnitamaGame -> [OnitamaAction]
winningMove (OnitamaBoard stalemate aPlayer inPlayer board cardsAPlayer cardsIPlayer card) = [(Action cardAction pieceOut (Piece (x,y) player tipo))| (Action cardAction pieceOut (Piece (x,y) player tipo)) <- allPossibleActions (OnitamaBoard stalemate aPlayer inPlayer board cardsAPlayer cardsIPlayer card), (x,y)==(w,z) || ((tipo==Master) && (if player==RedPlayer then (x,y)==(3,5) else (x,y)==(3,1)))]
    where (w,z) = [(a,b) | (Piece (a,b) playerX tipoX)<-board, playerX==inPlayer && tipoX==Master] !! 0

{-- Lógica de juego ---------------------------------------------------------------------------------}
{-
MODELO DE POSICIONES EN EL TABLERO:

(1,1) | (2,1) | (3,1) | (4,1) | (5,1)
(1,2) | (2,2) | (3,2) | (4,2) | (5,2)
(1,3) | (2,3) | (3,3) | (4,3) | (5,3)
(1,4) | (2,4) | (3,4) | (4,4) | (5,4)
(1,5) | (2,5) | (3,5) | (4,5) | (5,5)

El Bool es utilizado para verificar la condición de ahogamiento, el primer OnitamaPlayer indica el jugador activo y el segundo el jugador inactivo, 
la lista de Piece representa las piezas en el tablero, la primera lista de OnitamaCard son las cartas del jugador activo y la segunda son las del inactivo. 
Por último, la OnitamaCard es la carta que queda al costado del tablero.
-}
data OnitamaGame = OnitamaBoard Bool OnitamaPlayer OnitamaPlayer [Piece] [OnitamaCard] [OnitamaCard] OnitamaCard deriving (Show) 

{-
La OnitamaCard indica qué carta se usará para realizar la acción, la primera Piece representa la pieza en su posición inicial y la segunda su posición final.
En el caso de NoMoves representa cuando no se puede realizar una acción, su OnitamaCard indica qué carta el jugador quiere cambiar.
-}
data OnitamaAction = Action OnitamaCard Piece Piece | NoMoves OnitamaCard OnitamaPlayer deriving (Eq,Show)

data OnitamaCard = Tiger | Dragon | Frog | Rabbit | Crab | Elephant | Goose | Rooster | Monkey | Mantis | Horse | Ox | Crane | Boar | Eel | Cobra
    | Giraffe | Kirin | Phoenix | Turtle | Fox | Panda | Seasnake | Mouse | Tanuki| Sable | Dog | Bear | Viper | Rat | Iguana | Otter deriving (Eq, Show, Read)
data OnitamaPlayer = RedPlayer | BluePlayer deriving (Eq, Show, Enum, Read, Bounded)

--La primer tupla indica su posición actual, el OnitamaPlayer su jugador y el Type si es un aprendiz o maestro, el constructor Empty representa una casilla vacía.
data Piece = Piece (Int,Int) OnitamaPlayer Type | Empty deriving (Eq, Read) 

instance Show Piece where
    show (Piece _ RedPlayer Master) = "| R "
    show (Piece _ RedPlayer Apprentice) = "| r "
    show (Piece _ BluePlayer Master) = "| B "
    show (Piece _ BluePlayer Apprentice) = "| b "
    show (Empty) = "| _ "

data Type = Master | Apprentice deriving (Eq, Show, Read)

data GameResult p = Winner p | Loser p | Draw deriving (Eq, Show)

data OnitamaConfig = OnitamaConfig { configDeck :: [OnitamaCard], configHandSize :: Int, configStalemate :: Bool } deriving (Eq, Show)

--En variant se verifica OnitamaConfig para determinar si es una configuración válida, en el caso de serlo, ejecuta un juego con ella.
variant :: OnitamaConfig -> OnitamaGame
variant config 
    | (configHandSize config) < 8 && (configHandSize config) > 1 = variantBeginning (take ((configHandSize config) * 2 + 1) (configDeck config)) (configStalemate config) 
    | otherwise = error "Configuración inválida"

variantBeginning :: [OnitamaCard] -> Bool -> OnitamaGame
variantBeginning x stalemate = OnitamaBoard stalemate (fst players) (snd players) ([if s==3 then (Piece (s,1) RedPlayer Master) else (Piece (s,1) RedPlayer Apprentice) | s <-[1..5]] ++ [if a==3 then (Piece (a,5) BluePlayer Master) else (Piece (a,5) BluePlayer Apprentice) | a <-[1..5]]) (take handSize x) (drop handSize (take (handSize*2) x)) (last x) 
    where handSize = ((length x)-1) `div` 2
          players = whoFirst (last x)

deck :: [OnitamaCard]
deck = [Tiger, Dragon, Frog, Rabbit, Crab, Elephant, Goose, Rooster, Monkey, Mantis, Horse, Ox, Crane, Boar, Eel, Cobra]

deckSenseisPath :: [OnitamaCard]
deckSenseisPath = [Giraffe, Kirin, Phoenix, Turtle, Fox, Panda, Seasnake, Mouse, Tanuki, Sable, Dog, Bear, Viper, Rat, Iguana, Otter]

{-
La función beginning tiene el estado inicial del juego, incluyendo el orden en el cual están barajadas las cartas del mazo. Los nombres de las variables
son utilizados unicamente en esta función. 
Cubre el caso de pasar el deck entero por parametro, que solo ocurre por fuera del uso de la funcion variantBeginning.
-}
beginning :: [OnitamaCard] -> OnitamaGame
beginning x = OnitamaBoard False (fst players) (snd players) ([if s==3 then (Piece (s,1) RedPlayer Master) else (Piece (s,1) RedPlayer Apprentice) | s <-[1..5]] ++ [if a==3 then (Piece (a,5) BluePlayer Master) else (Piece (a,5) BluePlayer Apprentice) | a <-[1..5]]) (take 2 x) (drop 2 (take (4) x)) (last (take 5 x)) 
    where players = whoFirst (last (take 5 x))

whoFirst :: OnitamaCard -> (OnitamaPlayer, OnitamaPlayer)
whoFirst card
    | elem card [Giraffe, Phoenix, Dog, Bear, Seasnake, Mouse, Tanuki, Sable, Tiger, Rabbit, Ox, Crab, Goose, Monkey, Crane, Eel] = (BluePlayer, RedPlayer) 
    | otherwise = (RedPlayer, BluePlayer) 

activePlayer :: OnitamaGame -> Maybe OnitamaPlayer
activePlayer (OnitamaBoard stalemate x y z a b c) = if result (OnitamaBoard stalemate x y z a b c) == [] then Just x else Nothing

{-
La lista que devuelve la función actions incluirá una tupla para cada jugador. En el caso de que el jugador esté activo, la lista asociada incluirá
todos sus posibles movimientos para el estado de juego dado, sino la lista debe estar vacía.
-}
actions :: OnitamaGame -> [(OnitamaPlayer, [OnitamaAction])]
actions (OnitamaBoard stalemate aPlayer iPlayer board cardsActivePlayer k l) = [(aPlayer, if possibleActions /= [] then possibleActions else [NoMoves x aPlayer | x<-cardsActivePlayer] ),(iPlayer, [])]
    where possibleActions =  allPossibleActions (OnitamaBoard stalemate aPlayer iPlayer board cardsActivePlayer k l)

{-
Función encargada de devolver todos las acciones/movimientos posibles (ya filtrados por diversas condiciones) según las cartas del jugador.
Se ejecuta primero sobre la primera carta y luego se llama a sí misma de vuelta para la segunda.
-}
allPossibleActions :: OnitamaGame -> [OnitamaAction]
allPossibleActions (OnitamaBoard _ aPlayer _ pieces [] _ _) = []
allPossibleActions (OnitamaBoard stalemate aPlayer r pieces (card:cardList) s u) 
    | card==Tiger    = [Action card (Piece (x,y) player t) (Piece  (if aPlayer==RedPlayer then (x+w,y+z) else (x-w,y-z)) player t) | (Piece (x, y) player t)<-pieces, (w, z)<-[(0,-1),(0,2)], moveCheck (x,y) (w,z) aPlayer pieces && aPlayer == player ] ++ allPossibleActions (OnitamaBoard stalemate aPlayer r pieces cardList s u)  
    | card==Dragon   = [Action card (Piece (x,y) player t) (Piece  (if aPlayer==RedPlayer then (x+w,y+z) else (x-w,y-z)) player t) | (Piece (x, y) player t)<-pieces, (w, z)<-[(-2,1),(-1,-1),(1,-1),(2,1)], moveCheck (x,y) (w,z) aPlayer pieces && aPlayer == player ] ++ allPossibleActions (OnitamaBoard stalemate aPlayer r pieces cardList s u)  
    | card==Frog     = [Action card (Piece (x,y) player t) (Piece  (if aPlayer==RedPlayer then (x+w,y+z) else (x-w,y-z)) player t) | (Piece (x, y) player t)<-pieces, (w, z)<-[(2,0),(-1,-1),(1,1)], moveCheck (x,y) (w,z) aPlayer pieces && aPlayer == player ] ++ allPossibleActions (OnitamaBoard stalemate aPlayer r pieces cardList s u)  
    | card==Rabbit   = [Action card (Piece (x,y) player t) (Piece  (if aPlayer==RedPlayer then (x+w,y+z) else (x-w,y-z)) player t) | (Piece (x, y) player t)<-pieces, (w, z)<-[(-1,1),(1,-1),(-2,0)], moveCheck (x,y) (w,z) aPlayer pieces && aPlayer == player ] ++ allPossibleActions (OnitamaBoard stalemate aPlayer r pieces cardList s u)  
    | card==Crab     = [Action card (Piece (x,y) player t) (Piece  (if aPlayer==RedPlayer then (x+w,y+z) else (x-w,y-z)) player t) | (Piece (x, y) player t)<-pieces, (w, z)<-[(-2,0),(0,1),(2,0)], moveCheck (x,y) (w,z) aPlayer pieces && aPlayer == player ] ++ allPossibleActions (OnitamaBoard stalemate aPlayer r pieces cardList s u)  
    | card==Elephant = [Action card (Piece (x,y) player t) (Piece  (if aPlayer==RedPlayer then (x+w,y+z) else (x-w,y-z)) player t) | (Piece (x, y) player t)<-pieces, (w, z)<-[(-1,0),(-1,1),(1,0),(1,1)], moveCheck (x,y) (w,z) aPlayer pieces && aPlayer == player ] ++ allPossibleActions (OnitamaBoard stalemate aPlayer r pieces cardList s u)  
    | card==Goose    = [Action card (Piece (x,y) player t) (Piece  (if aPlayer==RedPlayer then (x+w,y+z) else (x-w,y-z)) player t) | (Piece (x, y) player t)<-pieces, (w, z)<-[(-1,-1),(-1,0),(1,0),(1,1)], moveCheck (x,y) (w,z) aPlayer pieces && aPlayer == player ] ++ allPossibleActions (OnitamaBoard stalemate aPlayer r pieces cardList s u)  
    | card==Rooster  = [Action card (Piece (x,y) player t) (Piece  (if aPlayer==RedPlayer then (x+w,y+z) else (x-w,y-z)) player t) | (Piece (x, y) player t)<-pieces, (w, z)<-[(-1,1),(-1,0),(1,0),(1,-1)], moveCheck (x,y) (w,z) aPlayer pieces && aPlayer == player ] ++ allPossibleActions (OnitamaBoard stalemate aPlayer r pieces cardList s u)  
    | card==Monkey   = [Action card (Piece (x,y) player t) (Piece  (if aPlayer==RedPlayer then (x+w,y+z) else (x-w,y-z)) player t) | (Piece (x, y) player t)<-pieces, (w, z)<-[(-1,-1),(-1,1),(1,-1),(1,1)], moveCheck (x,y) (w,z) aPlayer pieces && aPlayer == player ] ++ allPossibleActions (OnitamaBoard stalemate aPlayer r pieces cardList s u)  
    | card==Mantis   = [Action card (Piece (x,y) player t) (Piece  (if aPlayer==RedPlayer then (x+w,y+z) else (x-w,y-z)) player t) | (Piece (x, y) player t)<-pieces, (w, z)<-[(-1,1),(0,-1),(1,1)], moveCheck (x,y) (w,z) aPlayer pieces && aPlayer == player ] ++ allPossibleActions (OnitamaBoard stalemate aPlayer r pieces cardList s u)  
    | card==Horse    = [Action card (Piece (x,y) player t) (Piece  (if aPlayer==RedPlayer then (x+w,y+z) else (x-w,y-z)) player t) | (Piece (x, y) player t)<-pieces, (w, z)<-[(1,0),(0,-1),(0,1)], moveCheck (x,y) (w,z) aPlayer pieces && aPlayer == player ] ++ allPossibleActions (OnitamaBoard stalemate aPlayer r pieces cardList s u)  
    | card==Ox       = [Action card (Piece (x,y) player t) (Piece  (if aPlayer==RedPlayer then (x+w,y+z) else (x-w,y-z)) player t) | (Piece (x, y) player t)<-pieces, (w, z)<-[(0,-1),(0,1),(-1,0)], moveCheck (x,y) (w,z) aPlayer pieces && aPlayer == player ] ++ allPossibleActions (OnitamaBoard stalemate aPlayer r pieces cardList s u)  
    | card==Crane    = [Action card (Piece (x,y) player t) (Piece  (if aPlayer==RedPlayer then (x+w,y+z) else (x-w,y-z)) player t) | (Piece (x, y) player t)<-pieces, (w, z)<-[(-1,-1),(1,-1),(0,1)], moveCheck (x,y) (w,z) aPlayer pieces && aPlayer == player ] ++ allPossibleActions (OnitamaBoard stalemate aPlayer r pieces cardList s u)  
    | card==Boar     = [Action card (Piece (x,y) player t) (Piece  (if aPlayer==RedPlayer then (x+w,y+z) else (x-w,y-z)) player t) | (Piece (x, y) player t)<-pieces, (w, z)<-[(1,0),(0,1),(-1,0)], moveCheck (x,y) (w,z) aPlayer pieces && aPlayer == player ] ++ allPossibleActions (OnitamaBoard stalemate aPlayer r pieces cardList s u)  
    | card==Eel      = [Action card (Piece (x,y) player t) (Piece  (if aPlayer==RedPlayer then (x+w,y+z) else (x-w,y-z)) player t) | (Piece (x, y) player t)<-pieces, (w, z)<-[(1,-1),(1,1),(-1,0)], moveCheck (x,y) (w,z) aPlayer pieces && aPlayer == player ] ++ allPossibleActions (OnitamaBoard stalemate aPlayer r pieces cardList s u)  
    | card==Cobra    = [Action card (Piece (x,y) player t) (Piece  (if aPlayer==RedPlayer then (x+w,y+z) else (x-w,y-z)) player t) | (Piece (x, y) player t)<-pieces, (w, z)<-[(1,0),(-1,1),(-1,-1)], moveCheck (x,y) (w,z) aPlayer pieces && aPlayer == player ] ++ allPossibleActions (OnitamaBoard stalemate aPlayer r pieces cardList s u)  
    | card==Giraffe  = [Action card (Piece (x,y) player t) (Piece  (if aPlayer==RedPlayer then (x+w,y+z) else (x-w,y-z)) player t) | (Piece (x, y) player t)<-pieces, (w, z)<-[(0,1),(2,-1),(-2,-1)], moveCheck (x,y) (w,z) aPlayer pieces && aPlayer == player ] ++ allPossibleActions (OnitamaBoard stalemate aPlayer r pieces cardList s u)  
    | card==Kirin    = [Action card (Piece (x,y) player t) (Piece  (if aPlayer==RedPlayer then (x+w,y+z) else (x-w,y-z)) player t) | (Piece (x, y) player t)<-pieces, (w, z)<-[(0,2),(-1,-2),(1,-2)], moveCheck (x,y) (w,z) aPlayer pieces && aPlayer == player ] ++ allPossibleActions (OnitamaBoard stalemate aPlayer r pieces cardList s u)  
    | card==Phoenix  = [Action card (Piece (x,y) player t) (Piece  (if aPlayer==RedPlayer then (x+w,y+z) else (x-w,y-z)) player t) | (Piece (x, y) player t)<-pieces, (w, z)<-[(-2,0),(2,0),(-1,-1),(1,-1)], moveCheck (x,y) (w,z) aPlayer pieces && aPlayer == player ] ++ allPossibleActions (OnitamaBoard stalemate aPlayer r pieces cardList s u)  
    | card==Turtle   = [Action card (Piece (x,y) player t) (Piece  (if aPlayer==RedPlayer then (x+w,y+z) else (x-w,y-z)) player t) | (Piece (x, y) player t)<-pieces, (w, z)<-[(-2,0),(2,0),(-1,1),(1,1)], moveCheck (x,y) (w,z) aPlayer pieces && aPlayer == player ] ++ allPossibleActions (OnitamaBoard stalemate aPlayer r pieces cardList s u)  
    | card==Fox      = [Action card (Piece (x,y) player t) (Piece  (if aPlayer==RedPlayer then (x+w,y+z) else (x-w,y-z)) player t) | (Piece (x, y) player t)<-pieces, (w, z)<-[(1,-1),(1,0),(1,1)], moveCheck (x,y) (w,z) aPlayer pieces && aPlayer == player ] ++ allPossibleActions (OnitamaBoard stalemate aPlayer r pieces cardList s u)  
    | card==Panda    = [Action card (Piece (x,y) player t) (Piece  (if aPlayer==RedPlayer then (x+w,y+z) else (x-w,y-z)) player t) | (Piece (x, y) player t)<-pieces, (w, z)<-[(0,-1),(1,-1),(-1,1)], moveCheck (x,y) (w,z) aPlayer pieces && aPlayer == player ] ++ allPossibleActions (OnitamaBoard stalemate aPlayer r pieces cardList s u)  
    | card==Seasnake = [Action card (Piece (x,y) player t) (Piece  (if aPlayer==RedPlayer then (x+w,y+z) else (x-w,y-z)) player t) | (Piece (x, y) player t)<-pieces, (w, z)<-[(0,-1),(2,0),(-1,1)], moveCheck (x,y) (w,z) aPlayer pieces && aPlayer == player ] ++ allPossibleActions (OnitamaBoard stalemate aPlayer r pieces cardList s u)  
    | card==Mouse    = [Action card (Piece (x,y) player t) (Piece  (if aPlayer==RedPlayer then (x+w,y+z) else (x-w,y-z)) player t) | (Piece (x, y) player t)<-pieces, (w, z)<-[(0,-1),(1,0),(-1,1)], moveCheck (x,y) (w,z) aPlayer pieces && aPlayer == player ] ++ allPossibleActions (OnitamaBoard stalemate aPlayer r pieces cardList s u)  
    | card==Tanuki   = [Action card (Piece (x,y) player t) (Piece  (if aPlayer==RedPlayer then (x+w,y+z) else (x-w,y-z)) player t) | (Piece (x, y) player t)<-pieces, (w, z)<-[(0,-1),(2,-1),(-1,1)], moveCheck (x,y) (w,z) aPlayer pieces && aPlayer == player ] ++ allPossibleActions (OnitamaBoard stalemate aPlayer r pieces cardList s u)  
    | card==Sable    = [Action card (Piece (x,y) player t) (Piece  (if aPlayer==RedPlayer then (x+w,y+z) else (x-w,y-z)) player t) | (Piece (x, y) player t)<-pieces, (w, z)<-[(-2,0),(-1,1),(1,-1)], moveCheck (x,y) (w,z) aPlayer pieces && aPlayer == player ] ++ allPossibleActions (OnitamaBoard stalemate aPlayer r pieces cardList s u)  
    | card==Dog      = [Action card (Piece (x,y) player t) (Piece  (if aPlayer==RedPlayer then (x+w,y+z) else (x-w,y-z)) player t) | (Piece (x, y) player t)<-pieces, (w, z)<-[(-1,-1),(-1,0),(-1,1)], moveCheck (x,y) (w,z) aPlayer pieces && aPlayer == player ] ++ allPossibleActions (OnitamaBoard stalemate aPlayer r pieces cardList s u)  
    | card==Bear     = [Action card (Piece (x,y) player t) (Piece  (if aPlayer==RedPlayer then (x+w,y+z) else (x-w,y-z)) player t) | (Piece (x, y) player t)<-pieces, (w, z)<-[(0,-1),(-1,-1),(1,1)], moveCheck (x,y) (w,z) aPlayer pieces && aPlayer == player ] ++ allPossibleActions (OnitamaBoard stalemate aPlayer r pieces cardList s u)  
    | card==Viper    = [Action card (Piece (x,y) player t) (Piece  (if aPlayer==RedPlayer then (x+w,y+z) else (x-w,y-z)) player t) | (Piece (x, y) player t)<-pieces, (w, z)<-[(0,-1),(-2,0),(1,1)], moveCheck (x,y) (w,z) aPlayer pieces && aPlayer == player ] ++ allPossibleActions (OnitamaBoard stalemate aPlayer r pieces cardList s u)  
    | card==Rat      = [Action card (Piece (x,y) player t) (Piece  (if aPlayer==RedPlayer then (x+w,y+z) else (x-w,y-z)) player t) | (Piece (x, y) player t)<-pieces, (w, z)<-[(0,-1),(-1,0),(1,1)], moveCheck (x,y) (w,z) aPlayer pieces && aPlayer == player ] ++ allPossibleActions (OnitamaBoard stalemate aPlayer r pieces cardList s u)  
    | card==Iguana   = [Action card (Piece (x,y) player t) (Piece  (if aPlayer==RedPlayer then (x+w,y+z) else (x-w,y-z)) player t) | (Piece (x, y) player t)<-pieces, (w, z)<-[(0,-1),(-2,-1),(1,1)], moveCheck (x,y) (w,z) aPlayer pieces && aPlayer == player ] ++ allPossibleActions (OnitamaBoard stalemate aPlayer r pieces cardList s u)  
    | card==Otter    = [Action card (Piece (x,y) player t) (Piece  (if aPlayer==RedPlayer then (x+w,y+z) else (x-w,y-z)) player t) | (Piece (x, y) player t)<-pieces, (w, z)<-[(-1,-1),(1,1),(2,0)], moveCheck (x,y) (w,z) aPlayer pieces && aPlayer == player ] ++ allPossibleActions (OnitamaBoard stalemate aPlayer r pieces cardList s u)  

{-
Función auxiliar de allPossibleActions.
Verifica que no haya una pieza aliada ocupando la casilla de destino, que esta última se encuentre dentro del tablero, devolviendo verdadero si se cumple.
-}
moveCheck :: (Int, Int) -> (Int, Int) -> OnitamaPlayer -> [Piece] -> Bool
moveCheck (x,y) (w,z) player pieces 
    | player == RedPlayer = (x+w)<6 && (x+w)>0 && (y+z)<6 && (y+z)>0 && (not (any ((x+w,y+z) ==) [(a,b) |(Piece (a,b) playerX t) <-pieces , playerX == player]))
    | otherwise =(x-w)<6 && (x-w)>0 && (y-z)<6 && (y-z)>0 && (not (any ((x-w,y-z) ==) [(a,b) |(Piece (a,b) playerX t) <-pieces , playerX == player]))

{- 
Aplica una acción sobre un estado de juego dado, y retorna el estado resultante.
Tiene tres casos posibles, uno génerico para los errores relacionados al jugador, uno para el tipo de acción NoMoves y otro para el tipo de acción Action.
-}
next :: OnitamaGame -> OnitamaPlayer -> OnitamaAction -> OnitamaGame
next (OnitamaBoard stalemate aPlayer inPlayer board cardsAPlayer cardsIPlayer card) player accion
    | result (OnitamaBoard stalemate aPlayer inPlayer board cardsAPlayer cardsIPlayer card) /= [] = error "El juego termino"
    | maybeToList (activePlayer (OnitamaBoard stalemate aPlayer inPlayer board cardsAPlayer cardsIPlayer card)) !! 0 /= player = error "No es tu turno"

next (OnitamaBoard stalemate aPlayer inPlayer board cardsAPlayer cardsIPlayer card) player (NoMoves cardToUse _)
    | not stalemate && not (elem (NoMoves cardToUse player) (allPossibleActions (OnitamaBoard stalemate aPlayer inPlayer board cardsAPlayer cardsIPlayer card))) = error "Carta inválida"
    | not stalemate && (elem (NoMoves cardToUse player) (allPossibleActions (OnitamaBoard stalemate aPlayer inPlayer board cardsAPlayer cardsIPlayer card))) = OnitamaBoard stalemate inPlayer aPlayer board cardsIPlayer (card:(filter (cardToUse /=) cardsAPlayer)) cardToUse

next (OnitamaBoard stalemate aPlayer inPlayer board cardsAPlayer cardsIPlayer card) player (Action cardAction pieceOut pieceIn)
    | not (elem (Action cardAction pieceOut pieceIn) (allPossibleActions (OnitamaBoard stalemate aPlayer inPlayer board cardsAPlayer cardsIPlayer card))) = error "Movimiento invalido/Accion no realizable"
    | otherwise = OnitamaBoard stalemate inPlayer aPlayer (pieceMovements board (Action cardAction pieceOut pieceIn)) cardsIPlayer (card:(filter (cardAction/=) cardsAPlayer)) cardAction

{-
Función auxiliar de next.
Mueve las piezas en el tablero, se encarga de la lógica necesaria en caso de que haya una pieza enémiga en la casilla de destino.
-}
pieceMovements :: [Piece] -> OnitamaAction -> [Piece]
pieceMovements xs (Action _ (Piece (e,f) _ _) (Piece (a,b) player t)) 
    | any ((a,b) ==) [(c,d) | Piece (c,d) playerX _ <- xs, playerX /= player] = [Piece (if (c,d) == (e,f) then (a,b) else (c,d)) playerX tx | Piece (c,d) playerX tx <- xs, (c,d) /= (a,b)] 
    | otherwise = (Piece (a,b) player t) : [Piece (c,d) playerX tx | Piece (c,d) playerX tx <- xs, (c,d) /= (e,f)]

{-
Devuelve una lista que indica cuál de los jugadores es el ganador y cuál es el perdedor, tomando como condición de victoria haber capturado 
al maestro enemigo o que el maestro propio esté en el trono enemigo. En el caso de que ninguno haya ganado, devuelve una lista vacía.
-}
result :: OnitamaGame -> [GameResult OnitamaPlayer]
result (OnitamaBoard stalemate aPlayer inPlayer board cardsAPlayer cardsIPlayer card)
    | (stalemate && allPossibleActions (OnitamaBoard stalemate RedPlayer inPlayer board cardsAPlayer cardsIPlayer card) == []) || (any ((Piece (3,1) BluePlayer Master)==) board || (length [ 1 | (Piece _ player tipo)<-board, player == RedPlayer, tipo == Master]) == 0) = [Winner BluePlayer, Loser RedPlayer]
    | (stalemate && allPossibleActions (OnitamaBoard stalemate BluePlayer inPlayer board cardsAPlayer cardsIPlayer card) == []) || (any ((Piece (3,5) RedPlayer Master)==) board || (length [ 1 | (Piece _ player tipo)<-board, player == BluePlayer, tipo == Master]) == 0) = [Winner RedPlayer, Loser BluePlayer]
    | otherwise = []

showGame :: OnitamaGame -> String
showGame (OnitamaBoard _ aPlayer _ board cardsAPlayer cardsIPlayer card) 
    | aPlayer == BluePlayer = concat (["\n"] ++ (map (\x -> show x ++ " ") cardsIPlayer) ++ filler ++ (map (\x -> show x ++ " ") cardsAPlayer) ++ ["\n"])
    | aPlayer == RedPlayer = concat (["\n"] ++ (map (\x -> show x ++ " ") cardsAPlayer) ++ filler ++ (map (\x -> show x ++ " ") cardsIPlayer) ++ ["\n"])
        where filler =  ["\n"] ++ [show (auxShowGame (x,1) board)|x<-[1..5]] ++ ["|\n"] ++ [show (auxShowGame (x,2) board) |x<-[1..5]]++ ["|\n"] ++ [show (auxShowGame (x,3) board) |x<-[1..5]] ++ ["| "] ++ [(show card)] ++ ["\n"] ++ [show (auxShowGame (x,4) board) |x<-[1..5]]++ ["|\n"] ++ [show (auxShowGame (x,5) board) |x<-[1..5]] ++ ["|\n"] 

--Devuelve la pieza que hay en un determinado lugar, si no hay ninguna pieza, devuelve una pieza Empty para indicar que es una casilla vacía.
auxShowGame :: (Int,Int) -> [Piece] -> Piece
auxShowGame _ [] = Empty
auxShowGame (a,b) ((Piece (c,d) player tipo):ps) = if (a,b)==(c,d) then (Piece (c,d) player tipo) else auxShowGame (a,b) ps

--Ejemplo de salida cuando hay acción: RedPlayer Tiger Master (1,1) (1,3)
showAction :: OnitamaAction -> String
showAction (Action card (Piece (a,b) player tipo) (Piece (c,d) _ _)) = "\n"++ show player ++" "++ show card ++" "++ show tipo ++" "++ show (a,b) ++" "++ show (c,d) 
showAction (NoMoves card player) = show player ++" cambiar carta "++ show card

--Ejemplo de entrada cuando hay acción: RedPlayer Tiger Master (1,1) (1,3)
readAction :: String -> OnitamaAction
readAction linea
    | (length pos) == 5 = Action (read (pos !! 1)) (Piece (read (pos !! 3)) (read (pos !! 0)) (read (pos !! 2))) (Piece (read (pos !! 4)) (read (pos !! 0)) (read (pos !! 2)))  
    | (length pos) == 4 = NoMoves (read (pos !! 3)) (read (pos !! 0))
    | otherwise = error "Entrada inválida" 
    where pos = (words linea)

players :: [OnitamaPlayer]
players = [RedPlayer, BluePlayer]

--Implementación de agente inteligente 
intelligentAgent :: OnitamaPlayer -> OnitamaAgent
intelligentAgent player state@(OnitamaBoard stalemate playerX playerY pieces cardsX cardsY card) = do
    let moves = fromJust (lookup player (actions state))
        winnerMove = (intelligentWinCondition state moves)
        enemyMoves = allPossibleActions (OnitamaBoard stalemate playerY playerX pieces cardsY cardsX card)
        survivalMove = intelligentMoveFilter (intelligentSurvivalCondition state enemyMoves) enemyMoves 
        filteredMoves = intelligentMoveFilter moves enemyMoves
    if null moves then do
       putStrLn "No moves!"
       return Nothing
    else if winnerMove /= [] then do
       return (Just (winnerMove !! 0))
    else if survivalMove /= [] then do
       i <- randomRIO (0, (length survivalMove) - 1)
       return (Just (survivalMove !! i))
    else if filteredMoves /= [] then do
       i <- randomRIO (0, (length filteredMoves) - 1)
       return (Just (filteredMoves !! i))
    else do
       i <- randomRIO (0, (length moves) - 1)
       return (Just (moves !! i))

{-
Condición de victoria para el agente inteligente, verifica dos condiciones:
    1. Si hay algún movimiento del jugador inteligente que pueda tomar al maestro enemigo
    2. Si hay algún movimiento del jugador inteligente que pueda colocar a su maestro en el santuario enemigo 
-}
intelligentWinCondition :: OnitamaGame -> [OnitamaAction] -> [OnitamaAction]
intelligentWinCondition (OnitamaBoard stalemate aPlayer inPlayer board cardsAPlayer cardsIPlayer card) acciones = [(Action cardAction pieceOut (Piece (x,y) player tipo))| (Action cardAction pieceOut (Piece (x,y) player tipo)) <- acciones, (x,y)==(w,z) || ((tipo==Master) && (if player==RedPlayer then (x,y)==(3,5) else (x,y)==(3,1)))]
    where (w,z) = [(a,b) | (Piece (a,b) playerX tipoX)<-board, playerX==inPlayer && tipoX==Master] !! 0

{-
Condición para garantizar la supervivencia del agente:
Verifica si es posible que una pieza enemiga tome al maestro y, de serlo, devuelve solamente acciones para mover al maestro.
-}
intelligentSurvivalCondition :: OnitamaGame -> [OnitamaAction] -> [OnitamaAction]
intelligentSurvivalCondition state@(OnitamaBoard _ playerX playerY pieces _ _ _) enemyActions = if (not (null masterPiece)) then [(Action card pieceIn pieceOut) | (Action card pieceIn pieceOut) <- (allPossibleActions state), pieceIn == (masterPiece !! 0)] else []
    where xs = [(x,y) | (Action card _ (Piece (x,y) _ _)) <- enemyActions]
          masterPiece = [(Piece (w,z) player tipo) | (Piece (w,z) player tipo) <- pieces, tipo == Master && player == playerX && (elem (w,z) xs)]

{-
Condición para filtrar los movimientos que ponen las piezas en peligro:
Verifica si es posible que un movimiento propio ponga una pieza en peligro de ser tomada, de serlo, filtra esos movimientos. 
-}
intelligentMoveFilter :: [OnitamaAction] -> [OnitamaAction] -> [OnitamaAction]
intelligentMoveFilter activeActions enemyActions = [(Action card pieceIn (Piece (x,y) player tipo)) | (Action card pieceIn (Piece (x,y) player tipo)) <- activeActions, not (elem (x,y) xs)]
    where xs = [(x,y) | (Action card _ (Piece (x,y) _ _)) <- enemyActions]

{-- Match controller -------------------------------------------------------------------------------

Código de prueba. Incluye una función para correr las partidas y dos agentes: consola y aleatorio.

-}
type OnitamaAgent = OnitamaGame -> IO (Maybe OnitamaAction)

{- La función ´runMatch´ corre la partida completa a partir del estado de juego dado, usando los dos 
agentes dados. Retorna una tupla con los puntajes (score) finales del juego.
-}
runMatch :: (OnitamaAgent, OnitamaAgent) -> OnitamaGame -> IO [GameResult OnitamaPlayer]
runMatch ags@(ag1, ag2) g = do
   putStrLn (showGame g)
   case (activePlayer g) of
      Nothing -> return $ result g
      Just p -> do
         let ag = [ag1, ag2] !! (fromJust (elemIndex p players))
         move <- ag g
         runMatch ags (Onitama.next g p (fromJust move))

shuffle :: [a] -> IO [a]
shuffle vs = do
  let len = length vs
  rs <- mapM randomRIO (take len (repeat (0.0::Double, 1.0)))
  return [vs !! i | (_, i) <- sort (zip rs [0..(len - 1)])]

runGame :: (OnitamaAgent, OnitamaAgent) -> IO [GameResult OnitamaPlayer]
runGame ags = do
  cards <- shuffle deck
  runMatch ags (beginning cards)

{- El agente de consola ´consoleAgent´ muestra el estado de juego y los movimientos disponibles por
consola, y espera una acción por entrada de texto.
-}
consoleAgent :: OnitamaPlayer -> OnitamaAgent
consoleAgent player state = do
   let moves = fromJust (lookup player (actions state))
   if null moves then do
      putStrLn "No moves!"
      getLine
      return Nothing
   else do
      putStrLn ("Select one move:" ++ concat [" "++ showAction m | m <- moves])
      line <- getLine
      let input = readAction line
      if elem input moves then return (Just input) else do 
         putStrLn "Invalid move!"
         consoleAgent player state

{- Las funciones ´runConsoleGame´ y `runConsoleMatch` ejecutan toda la partida 
usando dos agentes de consola.
-}
runConsoleGame :: IO [GameResult OnitamaPlayer]
runConsoleGame = do
   runGame (consoleAgent RedPlayer, consoleAgent BluePlayer)
runConsoleMatch :: OnitamaGame -> IO [GameResult OnitamaPlayer]
runConsoleMatch g = do
   runMatch (consoleAgent RedPlayer, consoleAgent BluePlayer) g

{- El agente aleatorio ´randomAgent´ elige una acción de las disponibles completamente al azar.
-}
randomAgent :: OnitamaPlayer -> OnitamaAgent
randomAgent player state = do
    let moves = fromJust (lookup player (actions state))
    if null moves then do
       putStrLn "No moves!"
       return Nothing
    else do
       i <- randomRIO (0, (length moves) - 1)
       return (Just (moves !! i))

{- Las funciones ´runRandomGame´ y `runRandomMatch` ejecutan toda la partida 
usando dos agentes aleatorios.
-}
runRandomGame :: IO [GameResult OnitamaPlayer]
runRandomGame = do
   runGame (randomAgent RedPlayer, randomAgent BluePlayer)
runRandomMatch :: OnitamaGame -> IO [GameResult OnitamaPlayer]
runRandomMatch g = do
   runMatch (randomAgent RedPlayer, randomAgent BluePlayer) g

-- Fin
