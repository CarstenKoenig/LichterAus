module Model exposing (..)

import Array
import List
import Random as Rnd exposing (Generator)
import Random.Array as RndArr
import Vektor exposing (..)


type alias Spielfeld =
    { dimX : Int
    , dimY : Int
    , lichter : Vektor Bool
    }


alleTrue : Spielfeld -> Bool
alleTrue spielfeld =
    List.all identity spielfeld.lichter


leeresSpielfeld : Int -> Int -> Spielfeld
leeresSpielfeld x y =
    { dimX = x
    , dimY = y
    , lichter = List.repeat (x * y) True
    }


randomSpielfeld : Int -> Int -> Int -> Generator Spielfeld
randomSpielfeld x y anzahl =
    let
        leer =
            leeresSpielfeld x y
    in
        randomZellen leer anzahl
            |> Rnd.map
                (List.foldl
                    (\z sp -> xorSpielfeld sp z.vektor)
                    leer
                )


randomZellen : Spielfeld -> Int -> Generator (List Zelle)
randomZellen spielfeld anzahl =
    let
        zellen =
            zellenSpan spielfeld
    in
        RndArr.shuffle (Array.fromList zellen)
            |> Rnd.map (\a -> Array.toList a |> List.take anzahl)


xorSpielfeld : Spielfeld -> Vektor Bool -> Spielfeld
xorSpielfeld spielfeld v =
    { spielfeld | lichter = xorVektor spielfeld.lichter v }


type alias Zelle =
    { x : Int
    , y : Int
    , licht : Bool
    , vektor : Vektor Bool
    }


rows : Spielfeld -> List (List Zelle)
rows spielfeld =
    let
        zelle y x l =
            Zelle x y l (rectVektor spielfeld x y)

        genRows y ls =
            case ls of
                [] ->
                    []

                ls ->
                    let
                        row =
                            List.take spielfeld.dimX ls

                        rest =
                            List.drop spielfeld.dimX ls

                        zellen =
                            List.indexedMap (zelle y) row
                    in
                        zellen :: genRows (y + 1) rest
    in
        genRows 0 spielfeld.lichter


zellenSpan : Spielfeld -> List Zelle
zellenSpan spielfeld =
    let
        elems =
            List.concat (rows spielfeld)

        proj =
            .vektor
    in
        span proj elems


type alias Koordinate =
    { x : Int, y : Int }


koordinaten : Spielfeld -> List Koordinate
koordinaten spielfeld =
    List.concatMap (\y -> List.map (\x -> Koordinate x y) [0..spielfeld.dimX - 1]) [0..spielfeld.dimY - 1]


rectVektor : Spielfeld -> Int -> Int -> Vektor Bool
rectVektor spielfeld x y =
    let
        istAktiv ( x', y' ) =
            abs (x' - x) + abs (y' - y) <= 1

        bs =
            koordinaten spielfeld
                |> List.map (\{ x, y } -> istAktiv ( x, y ))
    in
        bs
