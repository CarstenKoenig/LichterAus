module GameBoard
    exposing
        ( GameBoard
        , Cell
        , Coordinate
        , push, toggle, clearBoard
        , solvedGameBoard
        , randomGameBoard
        , allLightsOn
        , rows
        , solveGameBoard
        )

{-| defines the main-board where you play the game
and some operations on it

# definitions
@docs GameBoard, Cell, Coordinate

# initialisation
@docs solvedGameBoard, randomGameBoard

# properties
@docs allLightsOn, rows

# operations
@docs solveGameBoard, push, toggle, clearBoard

-}

import Array
import List
import Random as Rnd exposing (Generator)
import Random.Array as RndArr
import Vector exposing (..)
import Gauss exposing (span, solve)


{-| the rectacular game-board
dimX    is the cell-count per row
dimY    is the cell-count per column 
lights  is a vector representing the hole game-board 
        in a dimX * dimY - dimensional vector-field over the booleans

to get a 2-D representation of this 1-D lights-vector you can use the
rows function bellow
-}
type alias GameBoard =
    { dimX : Int
    , dimY : Int
    , lights : Vector Bool
    }


{-| a cell on the GameBoards has it's
coordinate, light indicating if it's lit, a vector
describing the action (by adding this vector to the board
vector) pushing the cell will have and the push-action
itself
-}
type alias Cell =
    { coordinate : Coordinate
    , light : Bool
    , vectorPush : Vector Bool
    , vectorToggle : Vector Bool
    }

{-| the coordinate y=row, x=column 
on the 2-D representation of a board
-}
type alias Coordinate =
    { x : Int, y : Int }


{-| "push" a cell on a board, calculating a new board
-}
push : Cell -> GameBoard -> GameBoard
push cell = xorGameBoard cell.vectorPush
        
{-| toggles the state of a cell on a board, calculating a new board
-}
toggle : Cell -> GameBoard -> GameBoard
toggle cell = xorGameBoard cell.vectorToggle

{-| initializes a board with all lights on
-}
solvedGameBoard : Int -> Int -> GameBoard
solvedGameBoard x y =
    { dimX = x
    , dimY = y
    , lights = List.repeat (x * y) True
    }

{-| clears the board: turns off all lights
-}
clearBoard : GameBoard -> GameBoard
clearBoard board =
    { board | lights = List.repeat (board.dimX * board.dimY) True }

{-| generates a gameboard of the given size with
about n randomly pressed cells
-}
randomGameBoard : Int -> Int -> Int -> Generator GameBoard
randomGameBoard x y n =
    let
        empty =
            solvedGameBoard x y
    in
        randomCells empty n
            |> Rnd.map
                (List.foldl
                    (\z -> xorGameBoard z.vectorPush )
                    empty
                )


{-| are all lights on the board lit?

    allLightsOn (solvedGameBoard 5 5) == True
-}
allLightsOn : GameBoard -> Bool
allLightsOn board =
    List.all identity board.lights


{-| transforms the 1-D lights-vector of the board
into a 2-D representation: 
a list of rows, where each row is a list of cells

    rows (solvedGameBoard 2 2) ==
          [ [ Cell 0 0 ..., Cell 0 1 ...]
          , [ Cell 1 0 ..., Cell 1 1 ...]
-}
rows : GameBoard -> List (List Cell)
rows board =
    let
        cell y x l =
            Cell (Coordinate x y) l (neighbourhoodVector board x y) (coordVector board x y)

        genRows y ls =
            case ls of
                [] ->
                    []

                ls ->
                    let
                        row =
                            List.take board.dimX ls

                        rest =
                            List.drop board.dimX ls

                        cells =
                            List.indexedMap (cell y) row
                    in
                        cells :: genRows (y + 1) rest
    in
        genRows 0 board.lights


{-| uses a simplified gaussian elimination to solve a GameBoard

a solution is just a list of cells you have to push in order
to turn on all lights on the gameboard
-}
solveGameBoard : GameBoard -> List Cell
solveGameBoard board =
    solve .vectorPush (cellSpan board) (mapVector not board.lights)



--------------------------------------------------------------------------------
-- helper functions


randomCells : GameBoard -> Int -> Generator (List Cell)
randomCells gameBoard n =
    let
        cells =
            cellSpan gameBoard
    in
        RndArr.shuffle (Array.fromList cells)
            |> Rnd.map (\a -> Array.toList a |> List.take n)


cellSpan : GameBoard -> List Cell
cellSpan gameBoard =
    let
        elems =
            List.concat (rows gameBoard)

        proj =
            .vectorPush
    in
        span proj elems


xorGameBoard : Vector Bool -> GameBoard -> GameBoard
xorGameBoard v gameBoard =
    { gameBoard | lights = xorVector gameBoard.lights v }

        
createVector : GameBoard -> Int -> Int -> ((Int,Int) -> Bool) -> Vector Bool
createVector gameBoard x y choose =
    coordinats gameBoard
        |> List.map (\{ x, y } -> choose ( x, y ))


        

neighbourhoodVector : GameBoard -> Int -> Int -> Vector Bool
neighbourhoodVector gameBoard x y =
    let
        inNeighbourhood ( x', y' ) =
            abs (x' - x) + abs (y' - y) <= 1
    in createVector gameBoard x y inNeighbourhood


coordVector : GameBoard -> Int -> Int -> Vector Bool
coordVector gameBoard x y =
    let
        atCoord ( x', y' ) =
            x == x' && y == y'

    in createVector gameBoard x y atCoord


coordinats : GameBoard -> List Coordinate
coordinats gameBoard =
    List.concatMap (\y -> List.map (\x -> Coordinate x y) [0..gameBoard.dimX - 1]) [0..gameBoard.dimY - 1]
