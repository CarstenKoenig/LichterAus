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
    { spielfeld : M.Spielfeld
    , gewonnen : Bool
    }


main =
    program
        { init = initialModel
        , update = update
        , subscriptions = \_ -> Subs.none
        , view = view
        }


initialModel =
    ( Model (M.leeresSpielfeld 8 5) False, Rnd.generate ResetSpielfeld (M.randomSpielfeld 8 5 10) )


view model =
    Html.div []
        [ Html.h2 []
            [ Html.text
                (if model.gewonnen then
                    "GEWONNEN"
                 else
                    ""
                )
            ]
        , viewRows model (M.rows model.spielfeld)
        ]


viewRows : Model -> List (List M.Zelle) -> Html Msg
viewRows model rows =
    Html.table []
        (List.map (viewRow model) rows)


viewRow : Model -> List M.Zelle -> Html Msg
viewRow model zellen =
    Html.tr []
        (List.map (viewZelle model) zellen)


viewZelle : Model -> M.Zelle -> Html Msg
viewZelle model zelle =
    Html.button
        [ zelleStyle zelle
        , Events.onClick
            (if model.gewonnen then
                NoOp
             else
                XorButton zelle.vektor
            )
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
    | NoOp


update msg model =
    case msg of
        NoOp ->
            ( model, Cmds.none )

        ResetSpielfeld sf ->
            ( { model | spielfeld = sf, gewonnen = M.alleTrue sf }
            , Cmds.none
            )

        XorButton v ->
            let
                sf =
                    M.xorSpielfeld model.spielfeld v
            in
                ( { model | spielfeld = sf, gewonnen = M.alleTrue sf }
                , Cmds.none
                )
