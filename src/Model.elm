module Model exposing (GameState(..), Model, Player, initialModel, restartGame)

import Utils.Grid as Grid exposing (Drag)
import Utils.Tile exposing (Tile)


type alias Model =
    { player : Maybe Player
    , gridSize : Int
    , grid : Grid.Grid
    , currentTiles : List Tile
    , score : Int
    , gameState : GameState
    , selected : Maybe ( Int, Int )
    , drag : Maybe Drag
    }


type GameState
    = NotStarted
    | Playing
    | GameOver
    | Paused


type alias Player =
    { name : String
    , bestScore : Int
    }


initialModel : Model
initialModel =
    { player = Nothing
    , gridSize = 5
    , grid = Grid.empty 5
    , currentTiles = []
    , score = 0
    , gameState = NotStarted
    , selected = Nothing
    , drag = Nothing
    }


restartGame : Model -> Model
restartGame model =
    { initialModel | player = model.player, gameState = Playing }
