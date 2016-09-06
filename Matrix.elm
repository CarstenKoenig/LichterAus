module Matrix exposing (..)

{-| represents a matrix as a list of it's rows
each row in turn is just a vector

# definition
@docs Matrix

# generation
@docs fromColumns

# properties
@docs rowCount, columnCount

# operations
@docs transpose, concat

-}

import Array exposing (Array)
import List
import Set exposing (Set)
import Vector exposing (..)


{-| a matrix is a list of row-vectors
-}
type alias Matrix a =
    List (Vector a)


{-| generates a matrix from a list of column-vectors

    fromColumns [[1,2],[3,4]] == [[1,3],[2,4]]
-}
fromColumns : List (Vector a) -> Matrix a
fromColumns =
    transpose


{-| returns the number of rows of the given matrix

    rowCount [[1,2,3],[4,5,6]] == 2
-}
rowCount : Matrix a -> Int
rowCount =
    List.length


{-| returns the number of columns of the given matrix

   rowCount [[1,2,3],[4,5,6]] == 3
-}
columnCount : Matrix a -> Int
columnCount rows =
    case rows of
        [] ->
            0

        xs :: _ ->
            List.length xs


{-| calculates the transpose of a Matrix
by reflecting the entries on the main diagonal

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


{-| appends the second matrix to the right of first

   concat [[1,2],[3,4]] [[5],[6]] == [[1,2,5],[3,4,6]]

-}
concat : Matrix a -> Matrix a -> Matrix a
concat =
    List.map2 (++)
