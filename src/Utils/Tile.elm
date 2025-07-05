module Utils.Tile exposing (Tile, create, render)

import Svg.Styled exposing (Svg, g, rect, text, text_)
import Svg.Styled.Attributes exposing (..)
import Utils.Styles


type alias Tile =
    { value : Int
    , x : Float
    , y : Float
    }


create : Int -> Float -> Float -> Tile
create value x y =
    { value = value
    , x = x
    , y = y
    }


render : Int -> Tile -> Svg msg
render cellSize tile =
    let
        cellSizef =
            toFloat cellSize
    in
    g
        [-- transform ("translate(" ++ String.fromFloat (tile.x * cellSizef) ++
         --             "," ++ String.fromFloat (tile.y * cellSizef) ++ ")")
        ]
        [ rect
            [ x (String.fromFloat (tile.x * cellSizef))
            , y (String.fromFloat (tile.y * cellSizef))
            , width (String.fromInt cellSize)
            , height (String.fromInt cellSize)
            , css Utils.Styles.cell
            , strokeWidth "1"
            ]
            []
        , text_
            [ x (String.fromFloat (tile.x * cellSizef + cellSizef / 2))
            , y (String.fromFloat (tile.y * cellSizef + cellSizef / 2))
            , textAnchor "middle"
            , dominantBaseline "middle"
            , css Utils.Styles.cellText
            , fontSize (String.fromInt (cellSize // 2))
            ]
            [ text (String.fromInt tile.value) ]
        ]
