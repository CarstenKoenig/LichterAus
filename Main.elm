module Main exposing (..)

import Html exposing (Html, Attribute)
import Html.App exposing (program)
import Html.Attributes as Attr
import Html.Events as Events
import Platform.Cmd as Cmds
import Platform.Sub as Subs
import Random as Rnd
import Spielfeld exposing (..)
import Vektor exposing (..)


type alias Model =
    { spielfeld : Spielfeld
    , gewonnen : Bool
    , loesung : List Koordinate
    }


main =
    program
        { init = initialModel
        , update = update
        , subscriptions = \_ -> Subs.none
        , view = view
        }


initialModel =
    ( Model (geloestesSpielfeld 9 9) False []
    , generiereNeuesSpiel
    )


type Msg
    = Push Zelle
    | ResetSpielfeld Spielfeld
    | NeuesSpiel
    | SucheLoesung
    | NoOp


update msg model =
    case msg of
        NoOp ->
            ( model, Cmds.none )

        NeuesSpiel ->
            ( model, generiereNeuesSpiel )

        SucheLoesung ->
            ( { model
                | loesung =
                    loeseSpielfeld model.spielfeld
                        |> List.map (\z -> z.koordinate)
              }
            , Cmds.none
            )

        ResetSpielfeld sf ->
            ( { model | spielfeld = sf, gewonnen = alleLichterAn sf }
            , Cmds.none
            )

        Push zelle ->
            let
                sf =
                    zelle.gedrückt model.spielfeld

                loesung =
                    model.loesung |> List.filter (\k -> k /= zelle.koordinate)
            in
                ( { model
                    | spielfeld = sf
                    , gewonnen = alleLichterAn sf
                    , loesung = loesung
                  }
                , Cmds.none
                )


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
        , viewRows model (zeilen model.spielfeld)
        , Html.div []
            [ Html.button [ Events.onClick SucheLoesung ] [ Html.text "Löse" ]
            , Html.button [ Events.onClick NeuesSpiel ] [ Html.text "neu" ]
            ]
        ]



--------------------------------------------------------------------------------
-- Hilfsfunktionen


generiereNeuesSpiel =
    Rnd.generate ResetSpielfeld (zufaelligesSpielfeld 5 5 10)


viewRows : Model -> List (List Zelle) -> Html Msg
viewRows model rows =
    Html.table []
        (List.map (viewRow model) rows)


viewRow : Model -> List Zelle -> Html Msg
viewRow model zellen =
    Html.tr []
        (List.map (viewZelle model) zellen)


viewZelle : Model -> Zelle -> Html Msg
viewZelle model zelle =
    Html.button
        [ zelleStyle model zelle
        , Events.onClick
            (if model.gewonnen then
                NoOp
             else
                Push zelle
            )
        ]
        []


zelleStyle : Model -> Zelle -> Attribute Msg
zelleStyle model zelle =
    Attr.style
        [ ( "backgroundColor", zelleColor model zelle )
        , ( "height", "45px" )
        , ( "width", "45px" )
        ]


zelleColor : Model -> Zelle -> String
zelleColor model zelle =
    if model.loesung |> List.member zelle.koordinate then
        "blue"
    else if zelle.licht then
        "green"
    else
        "red"
