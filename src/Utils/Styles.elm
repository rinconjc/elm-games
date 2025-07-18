module Utils.Styles exposing (..)

import Css exposing (..)
import Html.Styled exposing (Attribute, Html, button, h2, input, styled)


container : List Style
container =
    [ position fixed
    , top (pct 50)
    , left (pct 50)
    , transform (translate2 (pct -50) (pct -50))
    , fontFamily monospace
    , textAlign center
    , lineHeight (em 2.0)
    ]


sButton : List (Attribute msg) -> List (Html msg) -> Html msg
sButton =
    styled button
        [ backgroundColor (hex "4caf50")
        , border (px 0)
        , color (hex "fff")
        , padding2 (px 15) (px 32)
        , fontSize (px 16)
        , borderRadius (px 5)
        , margin2 (px 4) (px 2)
        , cursor pointer
        ]


sInput : List (Attribute msg) -> List (Html msg) -> Html msg
sInput =
    styled input
        [ fontSize (em 1.3), padding (px 5) ]


sH2 : List (Attribute msg) -> List (Html msg) -> Html msg
sH2 =
    styled h2 []


cell : List Style
cell =
    [ cursor grab
    , fill (hex "ffb94e")
    , property "stroke" "#aaa"
    , property "strokeWidth" "1px"
    ]


cellActive : List Style
cellActive =
    [ cursor grab
    , fill (hex "f39c12")
    , property "stroke" "#aaa"
    ]


cellText : List Style
cellText =
    [ fill (hex "fff")
    , property "stroke" "#fff"
    , property "user-select" "none"
    ]


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
