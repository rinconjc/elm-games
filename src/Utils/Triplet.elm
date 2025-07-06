module Utils.Triplet exposing (findMatch)

import Array exposing (Array)
import Utils.Grid as Grid exposing (getCell)


findMatch : Grid.Grid -> Maybe (List ( Int, Int ))
findMatch grid =
    let
        numRows =
            Array.length grid
    in
    List.range 0 (numRows - 1)
        |> List.reverse
        |> List.concatMap (\y_ -> List.range 0 (numRows - 1) |> List.map (\x_ -> ( x_, y_ )))
        |> List.filterMap (\pos -> findMatchAt grid pos)
        |> List.head
        |> Maybe.map identity


findMatchAt : Grid.Grid -> ( Int, Int ) -> Maybe (List ( Int, Int ))
findMatchAt grid ( x, y ) =
    let
        size =
            Array.length grid
    in
    if x < size - 2 && isMatch grid ( x, y ) ( x + 1, y ) ( x + 2, y ) then
        Just [ ( x, y ), ( x + 1, y ), ( x + 2, y ) ]

    else if y > 1 && isMatch grid ( x, y - 2 ) ( x, y - 1 ) ( x, y ) then
        Just [ ( x, y - 2 ), ( x, y - 1 ), ( x, y ) ]

    else
        Nothing


isMatch : Grid.Grid -> ( Int, Int ) -> ( Int, Int ) -> ( Int, Int ) -> Bool
isMatch grid ( x, y ) ( x1, y1 ) ( x2, y2 ) =
    case ( getCell grid x y, getCell grid x1 y1, getCell grid x2 y2 ) of
        ( Just a, Just b, Just c ) ->
            a + b == c || a - b == c

        _ ->
            False
