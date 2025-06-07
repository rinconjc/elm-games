module Main exposing (..)
import Playground exposing (..)
import Random exposing (Generator, int, float, step)
import Set exposing (toList)

main = game view update initialModel

view computer model =
    case model.fallingTile of
        Just tile ->
            viewEmptyGrid ++ List.map viewTile model.tiles ++ [viewTile tile]
        Nothing ->
            viewEmptyGrid ++ List.map viewTile model.tiles

update computer model =
    model
        -- |> updateSeed computer
        |> updateNewTile
  

gridSize = 5


viewEmptyGrid: List Shape
viewEmptyGrid =
    List.range 0 (gridSize * gridSize - 1)
        |> List.map emptyTile

emptyTile: Int -> Shape
emptyTile i =
    let row = i // gridSize
        col = modBy gridSize i
    in
    square grey 40
        |> move (41.0 * (toFloat col)) (41.0 * (toFloat row))


viewTile: Tile -> Shape
viewTile tile =
    group [square blue 40,
          words white (String.fromInt tile.value)]
        |> move (toFloat (41 * tile.col)) (tile.top)

-- viewTiles : List Tile -> List Shape
-- viewTiles tiles =
--     List.range 0 gridSize
--         |> List.indexedMap (\r ->
--                            List.indexedMap (\c -> viewTile r c ))
    -- tilej
    --     |> List.filterMap (\t -> t.index)
    --        |>
        -- |> List.indexedMap (\r row ->
        --     List.indexedMap (\c value -> cell value r c) row
        -- )
        -- |> List.concat

cell : Int -> Int -> Int -> Shape
cell value row col =
    let box =
            square grey 40 |> move ((toFloat col) * 41) ((toFloat row) * 41)
    in
    if value== 0 then
        box
    else
        group [square blue 40,
              words white (String.fromInt value) ]
            |> move ((toFloat col) * 41) ((toFloat row) * 41)


-- Generate a random integer between min and max (inclusive)
randomDigitGen = Random.int 1 9
randomColGen = Random.int 0 4

-- Model
--
type alias Tile =
    {top: Float
    ,col: Int
    ,value: Int
    ,falling: Bool}

type alias Grid =
    List (List Int)

emptyGrid : Grid
emptyGrid =
    List.repeat gridSize (List.repeat gridSize 0)

type alias FallingTile = {value:Int, col:Int, row:Int, offset:Float}

type alias Model =
    { tiles: List (Maybe Tile)
    , fallingTiles: List Tile
    , seed : Random.Seed}
    
initialModel: Model
initialModel =
        {tiles = List.repeat (gridSize*gridSize) Nothing
        , fallingTiles = []
        , seed = Random.initialSeed 100
        }

updateSeed : Computer -> Model -> Model
updateSeed computer model =
    case model.fallingTiles of
        [] ->
            let seed = Random.initialSeed computer.time.now
                (_, seed1) = Random.step (Random.int 0 1000) seed
            in
            {model | seed = seed1 }
        _ -> model


updateNewTile : Model -> Model
updateNewTile model =
    case model.fallingTiles of
        [] ->
            let (newCol, seed1) = Random.step randomColGen model.seed
                (randVal, seed2) = Random.step randomDigitGen seed1
                top = (gridSize * 41)
            in
                if isOccupied newCol (gridSize - 1) model.tiles then
                  {model| seed = seed2}
                else
                  { model | fallingTiles =
                         {value= randVal, col = newCol, top = top, falling=True} :: model.fallingTiles
                        , seed = seed2 }
        tiles ->
            let updateTiles fallings m =
                case fallings of
                    tile::rest ->
                        let top = tile.top - 0.40
                            rowBelow = floor ((tile.top - 3.0)/41.0)
                        in
                        if top <0 || isOccupied tile.col rowBelow m.tiles then
                            updateTiles rest {m | tiles =
                                updateTileAt (gridSize * (rowBelow+1)+tile.col) (Just {tile|falling=False,
                                    , top = toFloat (rowBelow+1)*41}) m.tiles}
                        else
                            updateTiles rest {model| fallingTiles = {tile | top = top} :: m.fallingTiles}
                    [] -> m
            in
                updateTiles tiles {model | fallingTiles = []}

isOccupied : Int -> Int -> List (Maybe Tile) -> Bool
isOccupied col row tiles =
    let index =  row * gridSize + col
    in
        tiles |> List.any (\t -> t.index == Just index)


updateTileAt: Int -> Maybe Tile -> List (Maybe Tile) -> List (Maybe Tile)
updateTileAt index tile tiles =
    let
        (before, _ :: after)=
            List.splitAt index tiles
    in
        before ++ (tile :: after)

-- top = row*41 + offset; top/41: row, offset

firstEmptyRow : Int -> Grid -> Maybe Int
firstEmptyRow col grid =
    List.indexedMap
        (\r row ->
        if List.head (List.drop col row) == Just 0 then
            Just r
        else
            Nothing)
        grid
            |> List.filterMap identity
               |> List.head


-- findAndSwapTriplets : Grid -> Grid
-- findAndSwapTriplets grid =
--     let
--         -- Helper to find all matching triplets in a row
--         findInRow : Int -> List Int -> Maybe (Int, Int)  -- (row, col) of starts of triplets
--         findInRow rowIndex row =
--             let
--                 findHelper col remaining =
--                     case remaining of
--                         a :: b :: c :: _ ->
--                             if a + b == c then
--                                 Just (rowIndex, col)
--                             else
--                                 findHelper (col + 1) (b :: c :: List.drop 2 remaining)
--                         _ ->
--                             Nothing
--             in
--             findHelper 0 row

--         -- Find all triplet positions in the grid (horizontal only)
--         tripletPositions : Maybe (Int, Int)
--         tripletPositions =
--             grid
--                 |> List.indexedMap findInRow
--                 |> List.filterMap identity
--                 |> List.head

--         -- Swap a cell with the one above it (or remove if at top)
--         swapWithAbove : (Int, Int) -> Grid -> Grid
--         swapWithAbove (row, col) g =
--             if row == gridSize then
--                 -- At top row, remove the element
--                 List.indexedMap
--                     (\r rowList ->
--                         if r == row then
--                             List.take col rowList ++ List.drop (col + 1) rowList
--                         else
--                             rowList
--                     )
--                     g
--             else
--                 -- Swap with cell above
--                 List.indexedMap
--                     (\r rowList ->
--                         if r == row - 1 then
--                             -- Row above: take the cell from below
--                             let
--                                 currentCell = Maybe.withDefault 0 (List.head (List.drop col (List.drop row grid |> List.head |> Maybe.withDefault [])))
--                             in
--                             List.take col rowList ++ [currentCell] ++ List.drop (col + 1) rowList
--                         else if r == row then
--                             -- Current row: take the cell from above
--                             let
--                                 aboveCell = Maybe.withDefault 0 (List.head (List.drop col (List.drop (row - 1) grid |> List.head |> Maybe.withDefault [])))
--                             in
--                             List.take col rowList ++ [aboveCell] ++ List.drop (col + 1) rowList
--                         else
--                             rowList
--                     )
--                     g

--         -- Process all found triplets
--         processAllTriplets : List (List Int) -> List (Int, Int) -> List (List Int)
--         processAllTriplets g positions =
--             case positions of
--                 [] ->
--                     g
--                 pos :: rest ->
--                     processAllTriplets (swapWithAbove pos g) rest
--     in
--     processAllTriplets grid tripletPositions
