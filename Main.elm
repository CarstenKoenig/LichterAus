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
    , isEditing : Bool
    , isSolvable : Bool
    }

dimension : Int
dimension = 9

main =
    program
        { init = initialModel
        , update = update
        , subscriptions = \_ -> Subs.none
        , view = view
        }


initialModel =
    ( Model (solvedGameBoard 0 0) False [] False True
    , generateNewGame
    )


type Msg
    = Push Cell
    | Toggle Cell
    | ResetGameBoard GameBoard
    | NewGame
    | FindSolution
    | ToggleEdit
    | ClearBoard
    | NoOp


update msg model =
    case msg of
        NoOp ->
            ( model, Cmds.none )

        ToggleEdit ->
            ( { model
                  | isEditing = not model.isEditing
                  , solution = []
                  , isSolvable = not (List.isEmpty <| solveGameBoard model.board)
              }, Cmds.none )

        ClearBoard ->
            ( { model
                  | board = clearBoard model.board
                  , isWon = False
                  , solution = []
              }, Cmds.none )

        NewGame ->
            ( { model | solution = [] }, generateNewGame )

        FindSolution ->
            let solution =
                    solveGameBoard model.board
                        |> List.map (\z -> z.coordinate)
            in 
                ( { model
                      | solution = solution
                  }, Cmds.none )

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

        Toggle cell ->
            let
                sf =
                    toggle cell model.board

            in
                ( { model
                    | board = sf
                    , isWon = allLightsOn sf
                  }
                , Cmds.none
                )


view model =
    Html.div []
        [ Html.h2 []
            [ Html.text
                (if model.isWon && not model.isEditing then
                    "YOU WON"
                 else
                    ""
                )
            , Html.text
                (if not model.isSolvable && not model.isEditing then
                     "UNSOLVABLE"
                 else
                     ""
                )
            ]
        , viewRows model (rows model.board)
        , Html.div []
            (if model.isEditing then
                 [ Html.button [ Events.onClick ToggleEdit ] [ Html.text "done" ]
                 , Html.button [ Events.onClick ClearBoard ] [ Html.text "clear" ] ]
            else 
                [ Html.button [ Events.onClick FindSolution ] [ Html.text "solve" ]
                , Html.button [ Events.onClick NewGame ] [ Html.text "new" ]
                , Html.button [ Events.onClick ToggleEdit ] [ Html.text "edit" ]
                ])
        ]



--------------------------------------------------------------------------------
-- helpers


generateNewGame =
    Rnd.generate ResetGameBoard (randomGameBoard dimension dimension 25)


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
            (if model.isWon && not model.isEditing then
                NoOp
             else if model.isEditing then
                Toggle cell
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
