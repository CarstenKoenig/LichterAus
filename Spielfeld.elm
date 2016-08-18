module Spielfeld
    exposing
        ( Spielfeld
        , Zelle
        , Koordinate
        , geloestesSpielfeld
        , zufaelligesSpielfeld
        , alleLichterAn
        , zeilen
        , loeseSpielfeld
        )

{-| In diesem Modul wird das Basis-Spielfeld
und die relevanten Operationen darauf definiert

# Definition
@docs Spielfeld, Zelle, Koordinate

# Initialisierungen
@docs leeresSpielfeld, zufaelligesSpielfeld

# Eigenschaften
@docs alleLichterAn, zeilen

# Operationen
@docs loeseSpielfeld

-}

import Array
import List
import Random as Rnd exposing (Generator)
import Random.Array as RndArr
import Vektor exposing (..)
import Gauss exposing (span, solve)


{-| Das Spielfeld, dargesttelt durch die
Anzahl der Lichter pro Zeile und pro Spalte
sowie alle Lichter als linearer Vektor

Die 2D-Darstellung wird durch die zeilen-Funktion
gegeben.
-}
type alias Spielfeld =
    { dimX : Int
    , dimY : Int
    , lichter : Vektor Bool
    }


{-| Eine Zelle des Spielfelds bestehend aus der
Koordinate auf dem Spielfeld, dem Zustand des Lichtes
der Zelle, einer Funktion, die das Licht auf einem
Spielfeld drückt und den Vektor der diese Operation
des Drückens beschreibt
-}
type alias Zelle =
    { koordinate : Koordinate
    , licht : Bool
    , gedrückt : Spielfeld -> Spielfeld
    , vektor : Vektor Bool
    }


{-| der x-Wert beschreibt die Zeile und der
y-Wert die Spalte auf dem Spielfeld
-}
type alias Koordinate =
    { x : Int, y : Int }


{-| Initialisiert ein Spielfeld, bei dem alle
Lichter AN sind mit der gegebenen Dimension
-}
geloestesSpielfeld : Int -> Int -> Spielfeld
geloestesSpielfeld x y =
    { dimX = x
    , dimY = y
    , lichter = List.repeat (x * y) True
    }


{-| Erzeugt ein zufälliges Spielfeld der angegebenen Dimension
in dem, ausgehend von einem gelösten Spielfeld ca. anzahl-Lichter
zufällig "gedrückt" werden
-}
zufaelligesSpielfeld : Int -> Int -> Int -> Generator Spielfeld
zufaelligesSpielfeld x y anzahl =
    let
        leer =
            geloestesSpielfeld x y
    in
        randomZellen leer anzahl
            |> Rnd.map
                (List.foldl
                    (\z -> xorSpielfeld z.vektor)
                    leer
                )


{-| prüft ob auf einem Spielfeld alle Lichter "an" sind
d.h. ob das Spielfeld gelöst ist

    alleLichterAn (geloestesSpielfeld 5 5) == True
-}
alleLichterAn : Spielfeld -> Bool
alleLichterAn spielfeld =
    List.all identity spielfeld.lichter


{-| wandelt den linearen Lichter-Vektor des
Spielfelds in eine Zeilen-Darstellung um.

D.h. das Ergebnis ist eine Zeilen-Liste mit
Listen von Zellen

    zeilen (geloestesSpielfeld 2 2) ==
          [ [ Zelle 0 0 ..., Zelle 0 1 ...]
          , [ Zelle 1 0 ..., Zelle 1 1 ...]
-}
zeilen : Spielfeld -> List (List Zelle)
zeilen spielfeld =
    let
        zelle y x l =
            let
                vektor =
                    zellenVektor spielfeld x y
            in
                Zelle (Koordinate x y) l (xorSpielfeld vektor) vektor

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


{-| benutzt den vereinfachten Algorithmus aus dem
Gauss-Modul um eine Lösung für das übergebene Spielfeld
zu finden.

Eine Lösung ist eine Liste mit Lichtern, die (in einer
beliebigen Reihenfolge) gedrückt werden muss um alle
Lichter auf dem Spielfeld "an" zu schalten
-}
loeseSpielfeld : Spielfeld -> List Zelle
loeseSpielfeld spielfeld =
    solve .vektor (zellenSpan spielfeld) (mapVektor not spielfeld.lichter)



--------------------------------------------------------------------------------
-- Hilfsfunktionen


randomZellen : Spielfeld -> Int -> Generator (List Zelle)
randomZellen spielfeld anzahl =
    let
        zellen =
            zellenSpan spielfeld
    in
        RndArr.shuffle (Array.fromList zellen)
            |> Rnd.map (\a -> Array.toList a |> List.take anzahl)


zellenSpan : Spielfeld -> List Zelle
zellenSpan spielfeld =
    let
        elems =
            List.concat (zeilen spielfeld)

        proj =
            .vektor
    in
        span proj elems


xorSpielfeld : Vektor Bool -> Spielfeld -> Spielfeld
xorSpielfeld v spielfeld =
    { spielfeld | lichter = xorVektor spielfeld.lichter v }


zellenVektor : Spielfeld -> Int -> Int -> Vektor Bool
zellenVektor spielfeld x y =
    let
        istAktiv ( x', y' ) =
            abs (x' - x) + abs (y' - y) <= 1

        bs =
            koordinaten spielfeld
                |> List.map (\{ x, y } -> istAktiv ( x, y ))
    in
        bs


koordinaten : Spielfeld -> List Koordinate
koordinaten spielfeld =
    List.concatMap (\y -> List.map (\x -> Koordinate x y) [0..spielfeld.dimX - 1]) [0..spielfeld.dimY - 1]
