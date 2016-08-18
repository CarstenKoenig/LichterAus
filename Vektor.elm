module Vektor exposing (..)

{-| Vektordarstellung als Liste der Komponenten

# Definition
@docs Vektor

# Operationen
@docs mapVektor, xorVektor

-}

import List
import Array exposing (Array)
import Set exposing (Set)


{-| ein Vektor ist einfach die Liste seiner Komponenten
-}
type alias Vektor a =
    List a


{-| Ändert den Vektor durch komponentenweise Anwendung von f

    mapVektor not [True,False] == [False,True]
-}
mapVektor : (a -> b) -> Vektor a -> Vektor b
mapVektor f =
    List.map f


{-| Verknüpft zwei Bool-Vektoren komponentenweise durch xor

    xorVektor [True, False, False] [True, True, False] == [True, True False]

entspricht der Vektor-Addition für Bool-Vektoren
-}
xorVektor : Vektor Bool -> Vektor Bool -> Vektor Bool
xorVektor xs ys =
    List.map2 xor xs ys
