module Update exposing (update)

import Array exposing (Array)
import Msg exposing (Msg(..))
import Model exposing (Model, GameState(..))
import Random
import Utils.Tile as Tile
import Time
import Utils.Grid as Grid
import Utils.Triplet as Triplet
import Utils.Tile exposing (Tile)
import String exposing (toInt)
import Utils.Grid exposing (isFull)

fallInc = 0.01

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewTiles values ->
            spawnNewTiles model values

        Tick _ ->
            if model.gameState == Playing then
                moveTilesDown model
            else
                ( model, Cmd.none )

        SelectTile x y ->
            handleTileSelection model x y

        SwapTiles pos1 pos2 ->
            handleSwap model pos1 pos2

        RemoveTriplets positions ->
            removeMatchedTiles model positions

        Pause ->
            ( { model | gameState = Paused }, Cmd.none )

        Resume ->
            ( { model | gameState = Playing }, Cmd.none )

        Restart ->
            ( Model.initialModel, Cmd.none )


spawnNewTiles : Model -> ( List Int, List Int ) -> ( Model, Cmd Msg )
spawnNewTiles model ( values, columns ) =
    let
        newTiles =
            List.map2
                (\value col ->
                    Tile.create value (toFloat col) 0  -- Spawn at top of random column
                )
                values
                columns
    in
    ( { model | currentTiles = newTiles }
    , Cmd.none
    )


moveTilesDown : Model -> ( Model, Cmd Msg )
moveTilesDown model =
    case model.currentTiles of
        [] ->
           if isFull model.grid then
               ({model | gameState = GameOver}, Cmd.none)
           else
            -- No tiles? Spawn new ones!
               ( model, generateNewTilesCmd model.gridSize )
        tiles ->
            let
                movedTiles =
                    List.map (\t -> { t | y = t.y + fallInc }) tiles  -- Half-step for smoothness
                (landedTiles, fallingTiles) = landingTiles movedTiles model
            in
                handleLanding {model | currentTiles = fallingTiles} landedTiles

generateNewTilesCmd : Int -> Cmd Msg
generateNewTilesCmd gridSize =
    Random.generate NewTiles <|
        Random.pair
            (Random.list 3 (Random.int 1 9))  -- Random values
            (Random.list 3 (Random.int 0 (gridSize - 1)))  -- Random columns


landingTiles : List Tile -> Model -> (List Tile, List Tile)
landingTiles tiles model =
    tiles
        |> List.partition (\t ->
                            t.y >= toFloat (model.gridSize - 1) ||
                            Grid.isCellOccupied model.grid (floor t.x) ((floor t.y) + 1) )

handleLanding : Model -> List Tile -> ( Model, Cmd Msg )
handleLanding model landedTiles =
    let
        snappedTiles = List.map (\t -> { t | y = toFloat (floor t.y) }) landedTiles
        updatedGrid = Grid.addTiles model.grid snappedTiles
    in
        ( { model | grid = updatedGrid} , Cmd.none)


handleTileSelection : Model -> Int -> Int -> ( Model, Cmd Msg )
handleTileSelection model x y =
    case model.selectedTile of
        Nothing ->
            ( { model | selectedTile = Just ( x, y ) }, Cmd.none )

        Just ( selectedX, selectedY ) ->
            if (abs (x - selectedX) + abs (y - selectedY)) == 1 then
                ( { model | selectedTile = Nothing }, Cmd.none )
            else
                ( { model | selectedTile = Just ( x, y ) }, Cmd.none )


handleSwap : Model -> ( Int, Int ) -> ( Int, Int ) -> ( Model, Cmd Msg )
handleSwap model ( x1, y1 ) ( x2, y2 ) =
    let
        updatedGrid =
            Grid.swapTiles model.grid ( x1, y1 ) ( x2, y2 )

        matchedPositions =
            Triplet.findMatches updatedGrid
    in
    if List.isEmpty matchedPositions then
        ( { model | grid = Grid.swapTiles updatedGrid ( x1, y1 ) ( x2, y2 ) }, Cmd.none )
    else
        ( { model
            | grid = updatedGrid
            , score = model.score + List.length matchedPositions * 10
          }
        , Cmd.none
        )


removeMatchedTiles : Model -> List ( Int, Int ) -> ( Model, Cmd Msg )
removeMatchedTiles model positions =
    let
        clearedGrid =
            List.foldl (\( x, y ) g -> Grid.setCell x y Nothing g) model.grid positions
    in
    ( { model | grid = clearedGrid }, Cmd.none )
