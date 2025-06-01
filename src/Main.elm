module Main exposing (..)
import Playground exposing (..)
import Random exposing (Generator, int, float, step)

main = game view update initialModel

view computer model =
    tiles model.grid

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
        group [box,
              words black (String.fromInt value) |> move ((toFloat col) * 41) ((toFloat row) * 41)]

-- Generate a random integer between min and max (inclusive)
randomDigitGen = Random.int 1 9

-- Model
--
type alias Grid =
    List (List Int)

emptyGrid : Grid
emptyGrid =
    List.repeat gridSize (List.repeat gridSize 0)


type alias Model =
    { grid: Grid
          ,fallingTile: Maybe (Int, Float)
    , time : Float}
    
initialModel: Model
initialModel =
    {grid = emptyGrid
         , fallingTile = Nothing
         , time = 0
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

updateFallingTile : Model -> (Model, Cmd Msg)
updateFallingTile model =
    case model.fallingTile of
        Nothing ->
            let randomCol =
                    Random.generate (always GenerateRandomCol) randomDigitGen
            in
              ({ model | fallingTile = Just (randomCol, 0.0) },  )

        Just (row, offset) ->
            -- Here you can update the falling tile logic as needed
            model

-- Msg
type Msg
    = GenerateRandomCol Int
