module Main exposing (..)
import Playground exposing (..)
import Random exposing (Generator, int, float, step)

main = game view update initialModel

view computer model =
    viewEmptyGrid
    -- case model.fallingTile of
    --     Just tile ->  (viewTiles model.grid) ++ [viewFallingTile tile]
    --     Nothing -> viewTiles model.grid

update computer model =
    model
        -- |> updateSeed computer
        -- |> updateFallingTile
  

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


viewFallingTile : FallingTile -> Shape
viewFallingTile t =
        group [square blue 40
              , words white (String.fromInt t.value)]
        |> move ((toFloat t.col) * 41) (t.offset)

-- Generate a random integer between min and max (inclusive)
randomDigitGen = Random.int 1 9
randomColGen = Random.int 0 4

-- Model
--
type alias Tile =
    {pos: (Int, Int)
    ,index: Maybe Int
    ,value: Int
    ,target: (Int, Int)
    ,falling: Bool}

type alias Grid =
    List (List Int)

emptyGrid : Grid
emptyGrid =
    List.repeat gridSize (List.repeat gridSize 0)

type alias FallingTile = {value:Int, col:Int, row:Int, offset:Float}

type alias Model =
    { tiles: List Tile
    , fallingTile: Maybe Tile
    , seed : Random.Seed}
    
initialModel: Model
initialModel =
        {tiles = []
        , fallingTile = Nothing
        , seed = Random.initialSeed 1
        }

updateSeed : Computer -> Model -> Model
updateSeed computer model =
    case model.fallingTile of
        Nothing ->
            let seed = Random.initialSeed computer.time.now
                (_, seed1) = Random.step (Random.int 0 1000) seed
            in
            {model | seed = seed1 }
        Just _ -> model

-- Function to update a specific element in the grid
updateGrid : Int -> Int -> Int -> Grid -> Grid
updateGrid rowIndex colIndex newValue grid =
    grid
        |> List.indexedMap
            (\rIndex row ->
                if rIndex == rowIndex then
                    List.indexedMap
                        (\cIndex value ->
                            if cIndex == colIndex then
                                newValue  -- Update the specific element
                            else
                                value  -- Keep the original value
                        ) row
                else
                    row  -- Keep the original row
            )

-- updateFallingTile : Model -> Model
-- updateFallingTile model =
--     case model.fallingTile of
--         Nothing ->
--             let (randomCol, seed1) = Random.step randomColGen model.seed
--                 (randVal, seed2) = Random.step randomDigitGen seed1
--                 row = firstEmptyRow randomCol model.grid
--             in
--                 case row of
--                     Just r ->
--                         { model | fallingTile = Just {value= randVal, col = randomCol, offset = (gridSize * 41), row=r}
--                         , seed = seed2 }
--                     Nothing ->
--                         {model | seed=seed2}
--         Just tile ->
--             if tile.offset <= 41*(toFloat tile.row) then
--                 {model | fallingTile = Nothing, grid = updateGrid tile.row tile.col tile.value model.grid}
--             else
--                 {model| fallingTile = Just {tile | offset = tile.offset-0.40} }


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
