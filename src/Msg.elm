module Msg exposing (Msg(..))

import Time


type Msg
    = NewTiles ( List Int, List Int )
    | Tick Time.Posix
    | SelectTile Int Int
    | RemoveTriplets (List ( Int, Int ))
    | Pause
    | Resume
    | Restart
    | DragStart Int Int
    | DragEnd
    | DragOver Int Int
    | Drop
