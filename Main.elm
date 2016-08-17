module Main exposing (..)

import Html exposing (Html, Attribute)
import Html.App exposing (program)
import Html.Events as Events
import Html.Attributes as Attr
import Model as M
import Vektor exposing (..)
import Random as Rnd
import Platform.Sub as Subs
import Platform.Cmd as Cmds


type alias Model =
    { spielfeld : M.Spielfeld }


main =
    program
        { init = initialModel
        , update = update
        , subscriptions = \_ -> Subs.none
        , view = view
        }


initialModel =
    ( Model (M.leeresSpielfeld 8 5), Rnd.generate ResetSpielfeld (M.randomSpielfeld 8 5 10) )


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
        []


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
    | ResetSpielfeld M.Spielfeld


update msg model =
    case msg of
        ResetSpielfeld sf ->
            ( { model | spielfeld = sf }
            , Cmds.none
            )

        XorButton v ->
            ( { model | spielfeld = M.xorSpielfeld model.spielfeld v }
            , Cmds.none
            )
