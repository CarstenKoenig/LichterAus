module Vektor exposing (..)

import List
import Array exposing (Array)
import Set exposing (Set)


type alias Vektor a =
    List a


span : (a -> Vektor Bool) -> List a -> List a
span proj elems =
    let
        vs =
            List.map proj elems

        mat =
            fromMatrix (fromColumns vs)

        echolon =
            intoEcholon mat

        indizes =
            spanIndizesFromEcholon echolon
    in
        List.indexedMap (\i a -> ( i, a )) elems
            |> List.filter (\( i, a ) -> Set.member i indizes)
            |> List.map snd


solve : (a -> Vektor Bool) -> List a -> Vektor Bool -> List a
solve proj span y =
    let
        cols =
            List.map proj span

        mat =
            fromMatrix (fromColumns cols `concat` fromColumns [ y ])

        echo =
            intoEcholon mat

        indizes =
            solveEcholon echo
    in
        List.indexedMap (\i a -> ( i, a )) span
            |> List.filter (\( i, a ) -> Set.member i indizes)
            |> List.map snd


mapVektor : (a -> b) -> Vektor a -> Vektor b
mapVektor f =
    List.map f


xorVektor : Vektor Bool -> Vektor Bool -> Vektor Bool
xorVektor xs ys =
    List.map2 xor xs ys


type alias Matrix a =
    List (Vektor a)


rowCount : Matrix a -> Int
rowCount =
    List.length


columnCount : Matrix a -> Int
columnCount rows =
    case rows of
        [] ->
            0

        xs :: _ ->
            List.length xs


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


concat : Matrix a -> Matrix a -> Matrix a
concat =
    List.map2 (++)


fromColumns : List (Vektor a) -> Matrix a
fromColumns cols =
    transpose cols


type alias AMatrix =
    Array (Array Bool)


fromMatrix : Matrix Bool -> AMatrix
fromMatrix mat =
    Array.fromList (List.map Array.fromList mat)


spanIndizesFromEcholon : AMatrix -> Set Int
spanIndizesFromEcholon mat =
    let
        firstNonZero row =
            Array.toIndexedList row
                |> List.filter snd
                |> List.head
                |> Maybe.map fst
    in
        Array.toList mat
            |> List.filterMap firstNonZero
            |> Set.fromList


solveEcholon : AMatrix -> Set Int
solveEcholon mat =
    let
        last =
            Maybe.withDefault [] (lastColumn mat)

        relevant =
            List.map2 (\a b -> ( a, b )) (Array.toList mat) last
                |> List.filter snd
                |> List.map fst

        firstNonZero row =
            Array.toIndexedList row
                |> List.filter snd
                |> List.head
                |> Maybe.map fst
    in
        relevant
            |> List.filterMap firstNonZero
            |> Set.fromList


lastColumn : AMatrix -> Maybe (Vektor Bool)
lastColumn mat =
    Array.toList (Array.map Array.toList mat)
        |> transpose
        |> List.reverse
        |> List.head


intoEcholon : AMatrix -> AMatrix
intoEcholon mat =
    List.foldl reduceCol mat [0..colCount mat - 1]


reduceCol : Int -> AMatrix -> AMatrix
reduceCol col mat' =
    let
        mat =
            pivotUp col mat'

        pivot =
            case Array.get col mat of
                Just row ->
                    Array.toList row

                Nothing ->
                    []

        xorPivot row =
            case Array.get col row of
                Just True ->
                    Array.fromList (List.map2 xor pivot (Array.toList row))

                _ ->
                    row
    in
        Array.indexedMap
            (\rowNr row ->
                if rowNr /= col then
                    xorPivot row
                else
                    row
            )
            mat


pivotUp : Int -> AMatrix -> AMatrix
pivotUp col mat =
    case findePivot col mat of
        Nothing ->
            mat

        Just pivotRow ->
            if pivotRow == col then
                mat
            else
                swapRows col pivotRow mat


findePivot : Int -> AMatrix -> Maybe Int
findePivot col mat =
    let
        istPivot row =
            case Array.get col row of
                Just True ->
                    True

                _ ->
                    False

        finde r rows =
            case rows of
                [] ->
                    Nothing

                row :: rest ->
                    if istPivot row then
                        Just r
                    else
                        finde (r + 1) rest
    in
        finde col (List.drop col (Array.toList mat))


swapRows : Int -> Int -> AMatrix -> AMatrix
swapRows r1 r2 mat =
    case Array.get r1 mat of
        Nothing ->
            mat

        Just row1 ->
            case Array.get r2 mat of
                Nothing ->
                    mat

                Just row2 ->
                    Array.set r2 row1 (Array.set r1 row2 mat)


xorRows : Int -> Int -> AMatrix -> AMatrix
xorRows to add mat =
    case Array.get to mat of
        Nothing ->
            mat

        Just toRow ->
            case Array.get add mat of
                Nothing ->
                    mat

                Just addRow ->
                    let
                        result =
                            List.map2 xor (Array.toList toRow) (Array.toList addRow)
                    in
                        Array.set to (Array.fromList result) mat


colCount : AMatrix -> Int
colCount mat =
    case Array.get 0 mat of
        Just row ->
            Array.length row

        Nothing ->
            0
