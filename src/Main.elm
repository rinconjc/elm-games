module Main exposing (..)
import Playground exposing (..)
import Random exposing (Generator, int, float, step)

main = game view update initialModel

view computer model =
    case model.fallingTile of
        Just tile ->  (tiles model.grid) ++ [viewFallingTile tile]
        Nothing -> tiles model.grid

update computer model =
    model |> updateFallingTile
  

gridSize = 5

tiles : Grid -> List Shape
tiles grid =
    grid
        |> List.indexedMap (\r row ->
            List.indexedMap (\c value -> cell value r c) row
        )
        |> List.concat

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
type alias Grid =
    List (List Int)

emptyGrid : Grid
emptyGrid =
    List.repeat gridSize (List.repeat gridSize 0)

type alias FallingTile = {value:Int, col:Int, row:Int, offset:Float}

type alias Model =
    { grid: Grid
    , fallingTile: Maybe FallingTile
    , time : Float
    , seed : Random.Seed}
    
initialModel: Model
initialModel =
        {grid = emptyGrid
        , fallingTile = Nothing
        , time = 0
        , seed = Random.initialSeed 12345
        }



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

updateFallingTile : Model -> Model
updateFallingTile model =
    case model.fallingTile of
        Nothing ->
            let (randomCol, seed1) = Random.step randomColGen model.seed
                (randVal, seed2) = Random.step randomDigitGen seed1
                row = firstEmptyRow randomCol model.grid
            in
                case row of
                    Just r ->
                        { model | fallingTile = Just {value= randVal, col = randomCol, offset = (gridSize * 41), row=r}
                        , seed = seed2 }
                    Nothing ->
                        {model | seed=seed2}
        Just tile ->
            if tile.offset <= 41*(toFloat tile.row) then
                {model | fallingTile = Nothing, grid = updateGrid tile.row tile.col tile.value model.grid}
            else
                {model| fallingTile = Just {tile | offset = tile.offset-0.40} }


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
