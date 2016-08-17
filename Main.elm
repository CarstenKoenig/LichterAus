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
    , loesung : List M.Koordinate
    }


main =
    program
        { init = initialModel
        , update = update
        , subscriptions = \_ -> Subs.none
        , view = view
        }


generiereNeuesSpiel =
    Rnd.generate ResetSpielfeld (M.randomSpielfeld 9 9 30)


initialModel =
    ( Model (M.leeresSpielfeld 9 9) False []
    , generiereNeuesSpiel
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
        , viewRows model (M.rows model.spielfeld)
        , Html.div []
            [ Html.button [ Events.onClick SucheLoesung ] [ Html.text "Löse" ]
            , Html.button [ Events.onClick NeuesSpiel ] [ Html.text "neu" ]
            ]
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
        [ zelleStyle model zelle
        , Events.onClick
            (if model.gewonnen then
                NoOp
             else
                Push zelle
            )
        ]
        []


zelleStyle : Model -> M.Zelle -> Attribute Msg
zelleStyle model zelle =
    Attr.style
        [ ( "backgroundColor", zelleColor model zelle )
        , ( "height", "45px" )
        , ( "width", "45px" )
        ]


zelleColor : Model -> M.Zelle -> String
zelleColor model zelle =
    if model.loesung |> List.member zelle.koordinate then
        "blue"
    else if zelle.licht then
        "green"
    else
        "red"


type Msg
    = Push M.Zelle
    | ResetSpielfeld M.Spielfeld
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
                    solve .vektor (M.zellenSpan model.spielfeld) (mapVektor not model.spielfeld.lichter)
                        |> List.map (\z -> z.koordinate)
              }
            , Cmds.none
            )

        ResetSpielfeld sf ->
            ( { model | spielfeld = sf, gewonnen = M.alleTrue sf }
            , Cmds.none
            )

        Push zelle ->
            let
                sf =
                    M.xorSpielfeld model.spielfeld zelle.vektor

                loesung =
                    model.loesung |> List.filter (\k -> k /= zelle.koordinate)
            in
                ( { model
                    | spielfeld = sf
                    , gewonnen = M.alleTrue sf
                    , loesung = loesung
                  }
                , Cmds.none
                )
