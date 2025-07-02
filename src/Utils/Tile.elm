module Utils.Tile exposing (Tile, create, render)

import Svg exposing (Svg, g, rect , text_)
import Svg.Attributes exposing (..)


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
    let cellSizef = toFloat cellSize
    in
 -- transform ("translate(" ++ String.fromFloat (tile.x * cellSizef) ++
 --                 "," ++ String.fromFloat (tile.y * cellSizef) ++ ")")
    g [
      ]
        [ rect
            [ x (String.fromFloat (tile.x * cellSizef))
            , y (String.fromFloat (tile.y * cellSizef))
            , width (String.fromInt cellSize)
            , height (String.fromInt cellSize)
            , fill "#fff"
            , stroke "#000"
            , strokeWidth "1"
            ]
            []
        , text_
            [ x (String.fromFloat (tile.x * cellSizef + cellSizef / 2))
            , y (String.fromFloat (tile.y * cellSizef + cellSizef / 2))
            , textAnchor "middle"
            , dominantBaseline "middle"
            , fill "#000"
            , fontSize (String.fromInt (cellSize // 2))
            ]
            [ Svg.text (String.fromInt tile.value) ]
        ]
