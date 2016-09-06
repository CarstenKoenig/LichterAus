module Vector exposing (..)

{-| represents vectors as a list of it's components

# definition
@docs Vektor

# operations
@docs mapVektor, xorVektor

-}

import List
import Array exposing (Array)
import Set exposing (Set)


{-| a vector is just a list of it's component values
-}
type alias Vector a =
    List a


{-| maps a vector by applying f to each of it's components

    mapVector not [True,False] == [False,True]
-}
mapVector : (a -> b) -> Vector a -> Vector b
mapVector f =
    List.map f


{-| xors two vectors componentwise

    xorVector [True, False, False] [True, True, False] == [True, True False]

this is addition for boolean-vectors
-}
xorVector : Vector Bool -> Vector Bool -> Vector Bool
xorVector xs ys =
    List.map2 xor xs ys
