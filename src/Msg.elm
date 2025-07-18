module Msg exposing (Msg(..))

import Time


type Msg
    = NewTiles ( List Int, List Int )
    | Tick Time.Posix
    | Pause
    | Resume
    | Restart
    | DragStart ( Int, Int )
    | DragEnd
    | DragOver ( Int, Int )
    | Drop
    | Click ( Int, Int )
    | Registered String
