module Update exposing (update)

import Model exposing (GameState(..), Model)
import Msg exposing (Msg(..))
import Platform.Cmd as Cmd
import Random
import Utils.Grid as Grid exposing (getCell, isFull, setCell)
import Utils.Tile as Tile exposing (Tile)
import Utils.Triplet exposing (findMatch)


fallInc =
    0.01


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
            if model.gameState == Playing then
                handleSwap model pos1 pos2

            else
                ( model, Cmd.none )

        RemoveTriplets positions ->
            removeMatchedTiles model positions

        Pause ->
            ( { model | gameState = Paused }, Cmd.none )

        Resume ->
            ( { model | gameState = Playing }, Cmd.none )

        Restart ->
            ( Model.initialModel, Cmd.none )

        DragStart x y ->
            if model.gameState == Playing then
                ( { model | drag = Just { cell = ( x, y ), currentPos = ( x, y ) } }, Cmd.none )

            else
                ( model, Cmd.none )

        DragOver x y ->
            if model.gameState == Playing then
                case model.drag of
                    Just drag ->
                        if isAdjacent drag.cell ( x, y ) then
                            ( { model | drag = Just { drag | currentPos = ( x, y ) } }, Cmd.none )

                        else
                            ( { model | drag = Just { drag | currentPos = drag.cell } }, Cmd.none )

                    _ ->
                        ( model, Cmd.none )

            else
                ( model, Cmd.none )

        DragEnd ->
            ( { model | drag = Nothing }, Cmd.none )

        Drop ->
            if model.gameState == Playing then
                case model.drag of
                    Just drag ->
                        handleSwap { model | drag = Nothing } drag.cell drag.currentPos

                    _ ->
                        ( model, Cmd.none )

            else
                ( model, Cmd.none )


spawnNewTiles : Model -> ( List Int, List Int ) -> ( Model, Cmd Msg )
spawnNewTiles model ( values, columns ) =
    let
        newTiles =
            List.map2
                (\value col ->
                    Tile.create value (toFloat col) 0
                 -- Spawn at top of random column
                )
                values
                columns
                |> List.filter (\t -> getCell model.grid (floor t.x) 0 == Nothing)
    in
    ( { model | currentTiles = model.currentTiles ++ newTiles }
    , Cmd.none
    )


moveTilesDown : Model -> ( Model, Cmd Msg )
moveTilesDown model =
    case model.currentTiles of
        [] ->
            if isFull model.grid then
                ( { model | gameState = GameOver }, Cmd.none )

            else
                -- No tiles? Spawn new ones!
                ( model, generateNewTilesCmd model.gridSize )

        tiles ->
            let
                movedTiles =
                    List.map (\t -> { t | y = t.y + fallInc }) tiles

                -- Half-step for smoothness
                ( landedTiles, fallingTiles ) =
                    landingTiles movedTiles model
            in
            handleLanding { model | currentTiles = fallingTiles } landedTiles


generateNewTilesCmd : Int -> Cmd Msg
generateNewTilesCmd gridSize =
    Random.generate NewTiles <|
        Random.pair
            (Random.list 3 (Random.int 1 9))
            -- Random values
            (Random.list 3 (Random.int 0 (gridSize - 1)))



-- Random columns


landingTiles : List Tile -> Model -> ( List Tile, List Tile )
landingTiles tiles model =
    tiles
        |> List.partition
            (\t ->
                t.y
                    >= toFloat (model.gridSize - 1)
                    || Grid.isCellOccupied model.grid (floor t.x) (floor t.y + 1)
            )


handleLanding : Model -> List Tile -> ( Model, Cmd Msg )
handleLanding model landedTiles =
    let
        snappedTiles =
            List.map (\t -> { t | y = toFloat (floor t.y) }) landedTiles

        updatedGrid =
            Grid.addTiles model.grid snappedTiles
    in
    ( handleMatches { model | grid = updatedGrid }, Cmd.none )


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
    if isAdjacent ( x1, y1 ) ( x2, y2 ) then
        case ( getCell model.grid x1 y1, getCell model.grid x2 y2 ) of
            ( Just v1, Just v2 ) ->
                let
                    updatedGrid =
                        model.grid
                            |> setCell x1 y1 (Just v2)
                            |> setCell x2 y2 (Just v1)
                in
                ( handleMatches { model | grid = updatedGrid }, Cmd.none )

            _ ->
                ( model, Cmd.none )

    else
        ( model, Cmd.none )


handleMatches : Model -> Model
handleMatches model =
    case findMatch model.grid of
        Just [ ( x, y ), ( x1, y1 ), ( x2, y2 ) ] ->
            let
                score =
                    getCell model.grid x2 y2 |> Maybe.withDefault 0

                grid_ =
                    model.grid |> setCell x y Nothing |> setCell x1 y1 Nothing |> setCell x2 y2 Nothing

                ( grid2, tiles_ ) =
                    Grid.release grid_ ( x, y ) ( x1, y1 ) ( x2, y2 )
            in
            { model | grid = grid2, score = model.score + score, currentTiles = model.currentTiles ++ tiles_ }

        _ ->
            model


isAdjacent : ( Int, Int ) -> ( Int, Int ) -> Bool
isAdjacent ( x1, y1 ) ( x2, y2 ) =
    abs (x1 - x2) == 1 && y1 == y2 || abs (y1 - y2) == 1 && x1 == x2


removeMatchedTiles : Model -> List ( Int, Int ) -> ( Model, Cmd Msg )
removeMatchedTiles model positions =
    let
        clearedGrid =
            List.foldl (\( x, y ) g -> Grid.setCell x y Nothing g) model.grid positions
    in
    ( { model | grid = clearedGrid }, Cmd.none )
