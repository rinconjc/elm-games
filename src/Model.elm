module Model exposing (Model, initialModel, GameState(..))

import Array exposing (Array)
import Utils.Tile exposing (Tile)
import Utils.Grid as Grid


type alias Model =
    { gridSize : Int
    , grid : Grid.Grid
    , currentTiles : List Tile
    , score : Int
    , gameState : GameState
    , selectedTile : Maybe ( Int, Int )
    }


type GameState
    = Playing
    | GameOver
    | Paused


initialModel : Model
initialModel =
    { gridSize =5
    , grid = Grid.empty 5
    , currentTiles = []
    , score = 0
    , gameState = Playing
    , selectedTile = Nothing
    }
