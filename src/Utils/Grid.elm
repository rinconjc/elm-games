module Utils.Grid exposing (Grid, empty, getCell, setCell, isCellOccupied, addTiles, swapTiles, render, isFull)

import Array exposing (Array)
import Svg exposing (Svg, g, rect, text_)
import Svg.Attributes exposing (..)
import Utils.Tile exposing (Tile)
import Svg exposing (text)


type alias Grid =
    Array (Array (Maybe Int))


empty : Int -> Grid
empty size =
    Array.repeat size (Array.repeat size Nothing)


getCell : Grid -> Int -> Int -> Maybe Int
getCell grid x y =
    Array.get y grid
        |> Maybe.andThen (Array.get x)
        |> Maybe.andThen identity


setCell :  Int -> Int -> Maybe Int -> Grid -> Grid
setCell x y value grid =
    case Array.get y grid of
        Just row ->
            Array.set y (Array.set x value row) grid

        Nothing ->
            grid


isCellOccupied : Grid -> Int -> Int -> Bool
isCellOccupied grid x y =
    case getCell grid x y of
        Just _ ->
            True

        Nothing ->
            False


addTiles : Grid -> List { a | x : Float, y : Float, value : Int } -> Grid
addTiles grid tiles =
    List.foldl (\tile g -> setCell (floor tile.x) (ceiling tile.y) (Just tile.value) g) grid tiles


swapTiles : Grid -> ( Int, Int ) -> ( Int, Int ) -> Grid
swapTiles grid ( x1, y1 ) ( x2, y2 ) =
    let
        val1 =
            getCell grid x1 y1

        val2 =
            getCell grid x2 y2
    in
        grid
            |> setCell x1 y1 val2
            |> setCell x2 y2 val1


isFull : Grid -> Bool
isFull grid =
        case Array.get 0 grid of
            Just topRow ->
                topRow |> Array.filter ((==) Nothing)
                       |> Array.isEmpty
            _ -> False


render : Grid -> Int -> Maybe ( Int, Int ) -> List (Svg msg)
render grid cellSize selectedTile =
    let
        size =
            Array.length grid
    in
    List.concatMap
        (\y_ ->
            List.map
                (\x_ ->
                    let
                        cellValue =
                            getCell grid x_ y_

                        ( fillColor, textColor ) =
                            case selectedTile of
                                Just ( selectedX, selectedY ) ->
                                    if x_ == selectedX && y_ == selectedY then
                                        ( "#ff0", "#000" )
                                    else
                                        ( "#fff", "#000" )

                                Nothing ->
                                    ( "#fff", "#000" )
                    in
                    g []
                        [ rect
                            [ x (String.fromInt (x_ * cellSize))
                            , y (String.fromInt (y_ * cellSize))
                            , width (String.fromInt cellSize)
                            , height (String.fromInt cellSize)
                            , fill fillColor
                            , stroke "#000"
                            , strokeWidth "1"
                            ]
                            []
                        , case cellValue of
                            Just value ->
                                text_
                                    [ x (String.fromInt (x_ * cellSize + cellSize // 2))
                                    , y (String.fromInt (y_ * cellSize + cellSize // 2))
                                    , textAnchor "middle"
                                    , dominantBaseline "middle"
                                    , fill textColor
                                    , fontSize (String.fromInt (cellSize // 2))
                                    ]
                                    [ Svg.text (String.fromInt value) ]

                            Nothing ->
                                Svg.text ""
                        ]
                )
                (List.range 0 (size - 1))
        )
        (List.range 0 (size - 1))

