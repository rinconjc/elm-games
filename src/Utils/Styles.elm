module Utils.Styles exposing (..)

import Css exposing (..)


container : List Style
container =
    [ position fixed
    , top (pct 50)
    , left (pct 50)
    , transform (translate2 (pct -50) (pct -50))
    ]


cell : List Style
cell =
    [ property "user-select" "none"
    , cursor grab
    , fill (hex "ffb94e")
    , property "stroke" "#aaa"
    , property "strokeWidth" "1px"
    ]


cellActive : List Style
cellActive =
    [ property "user-select" "none"
    , cursor grab
    , fill (hex "f39c12")
    , property "stroke" "#aaa"
    ]


cellText : List Style
cellText =
    [ fill (hex "fff"), property "stroke" "#fff" ]


emptyCell : List Style
emptyCell =
    [ cursor notAllowed
    , fill (hex "eee")
    , property "stroke" "#aaa"
    , property "strokeWidth" "1px"
    ]


tile : List Style
tile =
    [ property "user-select" "none" ]
