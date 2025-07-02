module View exposing (view)

import Html exposing (Html, button, div, h1, text)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import Model exposing (Model, GameState(..))
import Msg exposing (Msg(..))
import Svg exposing (Svg, g, rect, svg, text_)
import Svg.Attributes exposing (width, height,viewBox)
import Utils.Tile exposing (render)
import Utils.Grid as Grid
import Utils.Tile exposing (Tile)
import Svg.Attributes exposing (transform)
import Svg.Attributes exposing (fill)
import Svg.Attributes exposing (stroke)
import Svg.Attributes exposing (x)
import Svg.Attributes exposing (y)
import Svg.Attributes exposing (textAnchor)


view : Model -> Html Msg
view model =
    div [ class "game-container" ]
        [ h1 [] [ text "Sum-Swap" ]
        , div [ class "game-controls" ]
            [ viewGameControls model.gameState
            , div [ class "score" ] [ text ("Score: " ++ String.fromInt model.score) ]
            ]
        , div [ class "game-grid" ]
            [ svg
                [ width "400"
                , height "400"
                , viewBox "0 0 400 400"
                ]
                (renderGame model)
            ]
        ]


viewGameControls : GameState -> Html Msg
viewGameControls gameState =
    case gameState of
        Playing ->
            button [ onClick Pause ] [ text "Pause" ]

        Paused ->
            div []
                [ button [ onClick Resume ] [ text "Resume" ]
                , button [ onClick Restart ] [ text "Restart" ]
                ]

        GameOver ->
            div []
                [ div [ class "game-over" ] [ text "Game Over!" ]
                , button [ onClick Restart ] [ text "Play Again" ]
                ]


renderGame : Model -> List (Svg Msg)
renderGame model =
    let
        cellSize =
            400 // model.gridSize

        gridCells =
            Grid.render model.grid cellSize model.selectedTile

        fallingTiles =
            List.map (render cellSize) model.currentTiles
    in
    gridCells ++ fallingTiles

