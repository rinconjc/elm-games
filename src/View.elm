module View exposing (view)

import Html.Styled exposing (Html, button, div, h1, text)
import Html.Styled.Attributes exposing (class)
import Html.Styled.Events exposing (onClick)
import Model exposing (GameState(..), Model)
import Msg exposing (Msg(..))
import Svg.Styled exposing (Svg, svg)
import Svg.Styled.Attributes exposing (css, height, viewBox, width)
import Utils.Grid as Grid
import Utils.Styles
import Utils.Tile exposing (render)


view : Model -> Html Msg
view model =
    div [ css Utils.Styles.container ]
        [ h1 [ class "title" ] [ text "Sum-Swap" ]
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
            Grid.render model.grid cellSize model.drag

        fallingTiles =
            List.map (render cellSize) model.currentTiles
    in
    gridCells ++ fallingTiles
