module Utils.Grid exposing (Drag, Grid, addTiles, empty, getCell, isCellOccupied, isFull, release, render, setCell, swapTiles)

import Array exposing (Array)
import Json.Decode as Decode
import Msg exposing (Msg(..))
import Svg.Styled exposing (Svg, g, rect, text, text_)
import Svg.Styled.Attributes exposing (..)
import Svg.Styled.Events as SE
import Utils.Styles
import Utils.Tile exposing (Tile, create)


type alias Drag =
    { cell : ( Int, Int )
    , currentPos : ( Int, Int )
    }


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


setCell : Int -> Int -> Maybe Int -> Grid -> Grid
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
            topRow
                |> Array.filter ((==) Nothing)
                |> Array.isEmpty

        _ ->
            False


render : Grid -> Int -> Maybe Drag -> List (Svg Msg)
render grid cellSize drag =
    let
        size =
            Array.length grid
    in
    List.concatMap
        (\y_ ->
            List.map (\x_ -> renderCell x_ y_ cellSize grid drag)
                (List.range 0 (size - 1))
        )
        (List.range 0 (size - 1))


renderCell : Int -> Int -> Int -> Grid -> Maybe Drag -> Svg Msg
renderCell x_ y_ cellSize grid drag =
    let
        cellValue =
            getCell grid x_ y_

        ( cellStyle, textStyle ) =
            case drag of
                Just { cell, currentPos } ->
                    if cell == ( x_, y_ ) || ( x_, y_ ) == currentPos then
                        ( Utils.Styles.cellActive, Utils.Styles.cellText )

                    else
                        ( Utils.Styles.cell, Utils.Styles.cellText )

                Nothing ->
                    ( Utils.Styles.cell, Utils.Styles.cellText )

        cellAttrs =
            [ x (String.fromInt (x_ * cellSize))
            , y (String.fromInt (y_ * cellSize))
            , width (String.fromInt cellSize)
            , height (String.fromInt cellSize)

            -- , stroke "#aaa"
            -- , css cellStyle
            , strokeWidth "1"
            ]
    in
    case cellValue of
        Just value ->
            -- let style = if
            g
                [ SE.on "mousedown" (Decode.succeed (DragStart x_ y_))
                , SE.on "touchstart" (Decode.succeed (DragStart x_ y_))
                , SE.on "mouseup" (Decode.succeed Drop)
                , SE.on "touchend" (Decode.succeed Drop)

                -- , SE.on "mouseleave" (Decode.succeed DragEnd)
                , SE.on "mouseenter" (Decode.succeed (DragOver x_ y_))
                , SE.on "touchmove" (Decode.succeed (DragOver x_ y_))
                , css cellStyle
                ]
                [ rect cellAttrs []
                , text_
                    [ x (String.fromInt (x_ * cellSize + cellSize // 2))
                    , y (String.fromInt (y_ * cellSize + cellSize // 2))
                    , textAnchor "middle"
                    , dominantBaseline "middle"
                    , css textStyle

                    -- , fill textColor
                    , fontSize (String.fromInt (cellSize // 2))
                    ]
                    [ text (String.fromInt value) ]
                ]

        Nothing ->
            g
                [ SE.on "mouseenter" (Decode.succeed (DragOver x_ y_))
                , SE.on "touchmove" (Decode.succeed (DragOver x_ y_))
                , SE.on "mouseup" (Decode.succeed Drop)
                , SE.on "touchend" (Decode.succeed Drop)
                , css Utils.Styles.emptyCell
                ]
                [ rect cellAttrs [ text "" ] ]


release : Grid -> ( Int, Int ) -> ( Int, Int ) -> ( Int, Int ) -> ( Grid, List Tile )
release grid ( x, y ) ( x1, y1 ) ( x2, y2 ) =
    -- horizontal
    if y == y2 then
        ( grid, [] )
            |> release_ ( x, y )
            |> release_ ( x1, y1 )
            |> release_ ( x2, y2 )
        -- vertical

    else
        release_ ( x, y ) ( grid, [] )


release_ : ( Int, Int ) -> ( Grid, List Tile ) -> ( Grid, List Tile )
release_ ( x, y ) ( grid, tiles ) =
    if y == 0 then
        ( grid, tiles )

    else
        case getCell grid x (y - 1) of
            Just v ->
                release_ ( x, y - 1 ) ( grid |> setCell x (y - 1) Nothing, create v (toFloat x) (toFloat (y - 1)) :: tiles )

            Nothing ->
                ( grid, tiles )
