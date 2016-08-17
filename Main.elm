module Main exposing (..)

import Html exposing (Html, Attribute)
import Html.App exposing (beginnerProgram)
import Html.Events as Events
import Html.Attributes as Attr
import Model as M
import Vektor exposing (..)


type alias Model =
    { spielfeld : M.Spielfeld }


main =
    beginnerProgram
        { model = Model (M.leeresSpielfeld 2 2)
        , view = view
        , update = update
        }


view model =
    Html.div []
        [ viewRows (M.rows model.spielfeld) ]


viewRows : List (List M.Zelle) -> Html Msg
viewRows rows =
    Html.table []
        (List.map viewRow rows)


viewRow : List M.Zelle -> Html Msg
viewRow zellen =
    Html.tr []
        (List.map viewZelle zellen)


viewZelle : M.Zelle -> Html Msg
viewZelle zelle =
    Html.button
        [ zelleStyle zelle
        , Events.onClick (XorButton zelle.vektor)
        ]
        [ Html.text <|
            if zelle.licht then
                "X"
            else
                "O"
        ]


zelleStyle : M.Zelle -> Attribute Msg
zelleStyle zelle =
    Attr.style
        [ ( "backgroundColor"
          , if zelle.licht then
                "green"
            else
                "red"
          )
        , ( "height", "40px" )
        , ( "width", "40px" )
        ]


type Msg
    = XorButton (Vektor Bool)


update msg model =
    case msg of
        XorButton v ->
            { model | spielfeld = M.xorSpielfeld model.spielfeld v }
