module View exposing (view)

import Css exposing (none, touchAction)
import Html.Styled exposing (Html, div, h1, h3, node, p, text)
import Html.Styled.Attributes exposing (name, placeholder, property)
import Html.Styled.Events exposing (onClick, onInput)
import Json.Decode as Decode exposing (field, float)
import Json.Encode as Encode
import Model exposing (GameState(..), Model, Player)
import Msg exposing (Msg(..))
import Svg.Styled exposing (Svg, svg)
import Svg.Styled.Attributes exposing (css, height, viewBox, width)
import Svg.Styled.Events as SE
import Utils.Grid as Grid
import Utils.Styles exposing (sButton, sH2, sInput)
import Utils.Tile exposing (render)


view : Model -> Html Msg
view model =
    let
        offsetXY =
            offsetDecode (gridSize // model.gridSize)
    in
    div [ css Utils.Styles.container ]
        [ node "meta"
            [ name "viewport"
            , property "content" (Encode.string "width=device-width, initial-scale=1.0")
            ]
            []
        , h1 [] [ text "Sum Swap Game!" ]
        , p [] [ text "Swap tiles to make up 3 adjacent matching numbers (adding or substrating the first 2 gives the third) " ]
        , div []
            [ playerView model
            , viewGameControls model.gameState
            , h3 [] [ text ("Score: " ++ String.fromInt model.score) ]
            ]
        , div []
            [ svg
                [ width (String.fromInt gridSize)
                , height (String.fromInt gridSize)
                , viewBox "0 0 300 300"
                , SE.on "pointerdown" (Decode.map DragStart offsetXY)
                , SE.on "pointermove" (Decode.map DragOver offsetXY)
                , SE.on "pointerup" (Decode.succeed Drop)
                , SE.preventDefaultOn "click" (Decode.map (\pos -> ( Click pos, True )) offsetXY)
                , css [ touchAction none ]
                ]
                (renderGame model)
            ]
        ]


gridSize =
    300


offsetDecode : Int -> Decode.Decoder ( Int, Int )
offsetDecode cellSize =
    let
        cellSizef =
            toFloat cellSize
    in
    Decode.map2 (\x y -> ( x, y ))
        (field "offsetX" float |> Decode.map (\x -> floor (x / cellSizef)))
        (field "offsetY" float |> Decode.map (\y -> floor (y / cellSizef)))


playerView : Model -> Html Msg
playerView model =
    let
        register =
            div []
                [ sH2 [] [ text "Register to Play:" ]
                , sInput [ placeholder "Your name", onInput Registered ] []
                ]
    in
    case ( model.gameState, model.player ) of
        ( NotStarted, _ ) ->
            register

        ( _, Nothing ) ->
            register

        ( _, Just p ) ->
            div []
                [ h3 [] [ text ("Player:" ++ p.name) ]
                , h3 [] [ text ("Best Score:" ++ String.fromInt p.bestScore) ]
                ]


viewGameControls : GameState -> Html Msg
viewGameControls gameState =
    case gameState of
        NotStarted ->
            sButton [ onClick Restart ] [ text "Play" ]

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
            gridSize // model.gridSize

        gridCells =
            Grid.render model.grid cellSize (cellSelected model)

        fallingTiles =
            List.map (render cellSize) model.currentTiles
    in
    gridCells ++ fallingTiles


cellSelected : Model -> ( Int, Int ) -> Bool
cellSelected model pos =
    case ( model.drag, model.selected ) of
        ( Just { cell, currentPos }, _ ) ->
            pos == cell || pos == currentPos

        ( _, Just selection ) ->
            selection == pos

        _ ->
            False
