module Main exposing (..)

import Html exposing (Html, Attribute)
import Html.App exposing (program)
import Html.Attributes as Attr
import Html.Events as Events
import Platform.Cmd as Cmds
import Platform.Sub as Subs
import Random as Rnd
import GameBoard exposing (..)
import Vector exposing (..)


type alias Model =
    { board : GameBoard
    , isWon : Bool
    , solution : List Coordinate
    }


main =
    program
        { init = initialModel
        , update = update
        , subscriptions = \_ -> Subs.none
        , view = view
        }


initialModel =
    ( Model (solvedGameBoard 9 9) False []
    , generateNewGame
    )


type Msg
    = Push Cell
    | ResetGameBoard GameBoard
    | NewGame
    | FindSolution
    | NoOp


update msg model =
    case msg of
        NoOp ->
            ( model, Cmds.none )

        NewGame ->
            ( model, generateNewGame )

        FindSolution ->
            ( { model
                | solution =
                    solveGameBoard model.board
                        |> List.map (\z -> z.coordinate)
              }
            , Cmds.none
            )

        ResetGameBoard sf ->
            ( { model | board = sf, isWon = allLightsOn sf }
            , Cmds.none
            )

        Push cell ->
            let
                sf =
                    push cell model.board

                solution =
                    model.solution |> List.filter (\k -> k /= cell.coordinate)
            in
                ( { model
                    | board = sf
                    , isWon = allLightsOn sf
                    , solution = solution
                  }
                , Cmds.none
                )


view model =
    Html.div []
        [ Html.h2 []
            [ Html.text
                (if model.isWon then
                    "YOU WON"
                 else
                    ""
                )
            ]
        , viewRows model (rows model.board)
        , Html.div []
            [ Html.button [ Events.onClick FindSolution ] [ Html.text "solve" ]
            , Html.button [ Events.onClick NewGame ] [ Html.text "new" ]
            ]
        ]



--------------------------------------------------------------------------------
-- helpers


generateNewGame =
    Rnd.generate ResetGameBoard (randomGameBoard 9 9 25)


viewRows : Model -> List (List Cell) -> Html Msg
viewRows model rows =
    Html.table []
        (List.map (viewRow model) rows)


viewRow : Model -> List Cell -> Html Msg
viewRow model celln =
    Html.tr []
        (List.map (viewCell model) celln)


viewCell : Model -> Cell -> Html Msg
viewCell model cell =
    Html.button
        [ cellStyle model cell
        , Events.onClick
            (if model.isWon then
                NoOp
             else
                Push cell
            )
        ]
        []


cellStyle : Model -> Cell -> Attribute Msg
cellStyle model cell =
    Attr.style
        [ ( "backgroundColor", cellColor model cell )
        , ( "height", "45px" )
        , ( "width", "45px" )
        ]


cellColor : Model -> Cell -> String
cellColor model cell =
    if model.solution |> List.member cell.coordinate then
        "blue"
    else if cell.light then
        "green"
    else
        "red"
