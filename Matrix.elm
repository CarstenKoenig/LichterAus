module Matrix exposing (..)

{-| Matrixdarstellung als Liste von Zeilen-Vektoren
und einige Operationen darauf

# Definition
@docs Matrix

# Erzeugung
@docs fromColumns

# Eigenschaften
@docs rowCount, columnCount

# Operationen
@docs transpose, concat

-}

import Array exposing (Array)
import List
import Set exposing (Set)
import Vektor exposing (..)


{-| Matrix als Liste von Zeilenvektoren
-}
type alias Matrix a =
    List (Vektor a)


{-| erzeugt eine Matrix aus Spaltenvektoren

    fromColumns [[1,2],[3,4]] == [[1,3],[2,4]]
-}
fromColumns : List (Vektor a) -> Matrix a
fromColumns =
    transpose


{-| Wieviele Zeilen hat die Matrix

    rowCount [[1,2,3],[4,5,6]] == 2
-}
rowCount : Matrix a -> Int
rowCount =
    List.length


{-| Wieviele Spalten hat die Matrix

   rowCount [[1,2,3],[4,5,6]] == 3
-}
columnCount : Matrix a -> Int
columnCount rows =
    case rows of
        [] ->
            0

        xs :: _ ->
            List.length xs


{-| transponiert eine Matrix, d.h.
die Einträge werden an der Hauptdiagonalen "gespiegelt"

   transpose [[1,2,3],[4,5,6]] == [[1,4],[2,5],[3,6]]

-}
transpose : Matrix a -> Matrix a
transpose mat =
    let
        dim =
            columnCount mat

        empties =
            List.repeat dim []

        trans xss =
            case xss of
                [] ->
                    empties

                ys :: yss ->
                    List.map2 (::) ys (trans yss)
    in
        trans mat


{-| hängt eine Matrix rechts an eine andere an

   concat [[1,2],[3,4]] [[5],[6]] == [[1,2,5],[3,4,6]]

-}
concat : Matrix a -> Matrix a -> Matrix a
concat =
    List.map2 (++)
