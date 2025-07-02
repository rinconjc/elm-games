module Subscriptions exposing (subscriptions)

import Msg exposing (Msg(..))
import Model exposing (Model)
import Time


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 16 Tick
