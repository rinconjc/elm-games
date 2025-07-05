module Main exposing (main)

import Browser
import Html.Styled exposing (toUnstyled)
import Model exposing (Model, initialModel)
import Msg exposing (Msg)
import Subscriptions exposing (subscriptions)
import Update exposing (update)
import View exposing (view)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view >> toUnstyled
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel, Cmd.none )
