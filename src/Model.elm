module Model exposing (GameState(..), Model, initialModel)

import Array exposing (Array)
import Utils.Grid as Grid exposing (Drag)
import Utils.Tile exposing (Tile)


type alias Model =
    { gridSize : Int
    , grid : Grid.Grid
    , currentTiles : List Tile
    , score : Int
    , gameState : GameState
    , selected : Maybe ( Int, Int )
    , drag : Maybe Drag
    }


type GameState
    = Playing
    | GameOver
    | Paused


initialModel : Model
initialModel =
    { gridSize = 5
    , grid = Grid.empty 5
    , currentTiles = []
    , score = 0
    , gameState = Playing
    , selected = Nothing
    , drag = Nothing
    }
