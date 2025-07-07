module View exposing (view)

import Html.Styled exposing (Html, div, h1, h3, node, p, text)
import Html.Styled.Attributes exposing (class, name, property)
import Html.Styled.Events exposing (onClick)
import Json.Encode as Encode
import Model exposing (GameState(..), Model)
import Msg exposing (Msg(..))
import Svg.Styled exposing (Svg, svg)
import Svg.Styled.Attributes exposing (css, height, viewBox, width)
import Utils.Grid as Grid
import Utils.Styles exposing (sButton, sH2)
import Utils.Tile exposing (render)


view : Model -> Html Msg
view model =
    div [ css Utils.Styles.container ]
        [ node "meta"
            [ name "viewport"
            , property "content" (Encode.string "width=device-width, initial-scale=1.0")
            ]
            []
        , h1 [] [ text "Sum Swap Game!" ]
        , p [] [ text "Swap tiles to make up 3 adjacent matching numbers (adding or substrating the first 2 gives the third) " ]
        , div [ class "game-controls" ]
            [ viewGameControls model.gameState
            , h3 [ class "score" ] [ text ("Score: " ++ String.fromInt model.score) ]
            ]
        , div []
            [ svg
                [ width "300"
                , height "300"
                , viewBox "0 0 300 300"
                ]
                (renderGame model)
            ]
        ]


viewGameControls : GameState -> Html Msg
viewGameControls gameState =
    case gameState of
        Playing ->
            sButton [ onClick Pause ] [ text "Pause" ]

        Paused ->
            div []
                [ sButton [ onClick Resume ] [ text "Resume" ]
                , sButton [ onClick Restart ] [ text "Restart" ]
                ]

        GameOver ->
            div []
                [ sH2 [] [ text "Game Over!" ]
                , sButton [ onClick Restart ] [ text "Play Again" ]
                ]


renderGame : Model -> List (Svg Msg)
renderGame model =
    let
        cellSize =
            300 // model.gridSize

        gridCells =
            Grid.render model.grid cellSize model.drag

        fallingTiles =
            List.map (render cellSize) model.currentTiles
    in
    gridCells ++ fallingTiles
