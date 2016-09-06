module Gauss exposing (span, solve)

{-| very simple implementation of the Gaussian elimination
(see https://en.wikipedia.org/wiki/Gaussian_elimination)
just for vectors over F² (boolean vectors)

# methods
@docs span, solve

-}

import Array exposing (Array)
import List
import Matrix exposing (..)
import Set exposing (Set)
import Vector exposing (..)


{-| given a list of some items you can extract boolean vectors from 
this will calculates a maximal sublist of "linearly indepentend" items:

    map proj (span proj items) 

will be a maximal linearly indepentend sublist of

    map proj items

-}
span : (a -> Vector Bool) -> List a -> List a
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


{-| given a list of some items you can extract boolean vectors from (using proj)
this will try to calculate a solution

    xs = solve proj span y

from a "spanning" list of items span, such that

    sum (map proj xs) = y
-}
solve : (a -> Vector Bool) -> List a -> Vector Bool -> List a
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



--------------------------------------------------------------------------------
-- simplified gaussian elimination algorithm


type alias AMatrix =
    Array (Array Bool)


colCount : AMatrix -> Int
colCount mat =
    case Array.get 0 mat of
        Just row ->
            Array.length row

        Nothing ->
            0


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


lastColumn : AMatrix -> Maybe (Vector Bool)
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
    case findPivot col mat of
        Nothing ->
            mat

        Just pivotRow ->
            if pivotRow == col then
                mat
            else
                swapRows col pivotRow mat


findPivot : Int -> AMatrix -> Maybe Int
findPivot col mat =
    let
        istPivot row =
            case Array.get col row of
                Just True ->
                    True

                _ ->
                    False

        find r rows =
            case rows of
                [] ->
                    Nothing

                row :: rest ->
                    if istPivot row then
                        Just r
                    else
                        find (r + 1) rest
    in
        find col (List.drop col (Array.toList mat))


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
