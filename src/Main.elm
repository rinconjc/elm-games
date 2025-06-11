module Main exposing (..)
import Playground exposing (..)
import Random exposing (Generator, int, float, step)
import Set exposing (toList)
import String exposing (toList)
import Set exposing (remove)

main = game view update initialModel

view : Computer -> Model -> List Shape
view computer model =
    viewEmptyGrid
    ++ (model.tiles
       |> List.filterMap identity
       |> List.map viewTile)
    ++ List.map viewTile model.fallingTiles
    ++ (viewSelectedTile model)

update: Computer -> Model -> Model
update computer model =
    model
        -- |> updateSeed computer
        |> updateSwap computer
        |> updateFallingTiles
        |> removeEquations

gridSize = 5
tileSideIn = 40
tileSideOut = 42


type alias TileTheme = {
        color: Color,
        textColor: Color
    }

defaultTheme : TileTheme
defaultTheme = {color=blue, textColor=white}
selectedTheme: TileTheme
selectedTheme = {color=green, textColor=white}

viewEmptyGrid: List Shape
viewEmptyGrid =
    List.range 0 (gridSize * gridSize - 1)
        |> List.map emptyTile

emptyTile: Int -> Shape
emptyTile i =
    let row = i // gridSize
        col = modBy gridSize i
    in
    square grey tileSideIn
        |> move (tileSideOut * (toFloat col)) (tileSideOut * (toFloat row))

viewTile : Tile -> Shape
viewTile tile = viewAnyTile defaultTheme tile

viewAnyTile: TileTheme -> Tile -> Shape
viewAnyTile theme tile =
    group [square theme.color tileSideIn,
          words theme.textColor (String.fromInt tile.value)]
        |> move (toFloat (tileSideOut * tile.col)) (tile.top)

viewSelectedTile: Model -> List Shape
viewSelectedTile model =
    case model.selectedTile of
        Just index -> case model.tiles
                   |> List.drop index
                   |> List.head
                   |> Maybe.andThen identity
                      of
                          Just tile -> [viewAnyTile selectedTheme tile]
                          _ -> []
        _ -> []

-- Generate a random integer between min and max (inclusive)
randomDigitGen = Random.int 1 9
randomColGen = Random.int 0 4

-- Model
--
type alias Tile =
    {top: Float
    ,col: Int
    ,value: Int}

type alias Grid =
    List (List Int)

emptyGrid : Grid
emptyGrid =
    List.repeat gridSize (List.repeat gridSize 0)

type alias FallingTile = {value:Int, col:Int, row:Int, offset:Float}

type alias Model =
    { tiles: List (Maybe Tile)
    , fallingTiles: List Tile
    , seed : Random.Seed
    , selectedTile: Maybe Int}
    
initialModel: Model
initialModel =
        {tiles = List.repeat (gridSize*gridSize) Nothing
        , fallingTiles = []
        , seed = Random.initialSeed 17
        , selectedTile = Nothing}

updateSeed : Computer -> Model -> Model
updateSeed computer model =
    case model.fallingTiles of
        [] ->
            let seed = Random.initialSeed computer.time.now
                (_, seed1) = Random.step (Random.int 0 1000) seed
            in
            {model | seed = seed1 }
        _ -> model


updateFallingTiles : Model -> Model
updateFallingTiles model =
    case model.fallingTiles of
        [] ->
            let (newCol, seed1) = Random.step randomColGen model.seed
                (randVal, seed2) = Random.step randomDigitGen seed1
                top = (gridSize * tileSideOut)
            in
                if isOccupied newCol (gridSize - 1) model.tiles then
                  {model| seed = seed2}
                else
                  { model | fallingTiles =
                         {value= randVal, col = newCol, top = top} :: model.fallingTiles
                        , seed = seed2 }
        tiles ->
            let updateTiles : List Tile -> Model -> Model
                updateTiles fallings m =
                    case fallings of
                        tile::rest ->
                            let top = tile.top - 0.80
                                rowBelow = floor ((tile.top - 3.0)/tileSideOut)
                            in
                            if top <0 || isOccupied tile.col rowBelow m.tiles then
                                updateTiles rest {m | tiles =
                                    updateTileAt (gridSize * (rowBelow+1)+tile.col) (Just {tile| top = toFloat (rowBelow+1)*tileSideOut}) m.tiles}
                            else
                                updateTiles rest {m| fallingTiles = {tile | top = top} :: m.fallingTiles}
                        [] -> m
            in
                updateTiles tiles {model | fallingTiles = []}

isOccupied : Int -> Int -> List (Maybe Tile) -> Bool
isOccupied col row tiles =
    let index =  row * gridSize + col
    in
        tiles
            |> List.drop index
            |> List.head
            |> Maybe.andThen identity
            |> isJust

isJust : Maybe a -> Bool
isJust maybe =
    case maybe of
        Just _ -> True
        _ -> False

updateTileAt: Int -> Maybe Tile -> List (Maybe Tile) -> List (Maybe Tile)
updateTileAt index tile tiles =
    let
        before = List.take index tiles
        rest = List.drop index tiles
    in
        case rest of
            (_ ::after) -> before ++ (tile :: after)
            [] -> before ++ [tile]


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

isValidEquation : Int -> Int -> Int -> Bool
isValidEquation a b c =
    (a + b == c) ||
    (a - b == c ) ||
    (a * b == c) ||
    (a // b == c)

horzTripletStarts : Int -> List a -> List Int
horzTripletStarts n grid =
    let
        gridLength = List.length grid
        isValidStart i =
            let
                rowStart = i // n * n
                rowEnd = rowStart + n - 1
            in
            i + 2 <= rowEnd && i + 2 < gridLength
    in
    List.range 0 (gridLength - 3)
        |> List.filter isValidStart


vertTripletStarts: Int -> List a -> List Int
vertTripletStarts n grid =
    []


findFirstTriplet : (a -> a -> a -> Bool) -> Int -> List a -> Maybe ( List a, Int )
findFirstTriplet predicate n grid =
    let
        starts = horzTripletStarts n grid
        checkStart i =
            let
                triplet =
                    List.drop i grid
                        |> List.take 3
            in
            case triplet of
                [ a, b, c ] ->
                    if predicate a b c then
                        Just ( triplet, i )
                    else
                        Nothing
                _ ->
                    Nothing
    in
    List.filterMap checkStart starts
        |> List.head

removeEquations: Model -> Model
removeEquations model =
    let predicate: Maybe Tile -> Maybe Tile -> Maybe Tile -> Bool
        predicate t1 t2 t3 =
            case (t1,t2,t3) of
                (Just a, Just b, Just c) -> isValidEquation a.value b.value c.value
                _ -> False
    in
    case findFirstTriplet predicate gridSize model.tiles of
        Just (_, index) ->
            let removeTriplet :Bool-> Int -> Model -> Model
                removeTriplet falls pos m =
                    let before = List.take pos m.tiles
                        triplet = List.drop pos m.tiles |> List.take 3 |> List.filterMap identity
                        after = List.drop (pos+3) m.tiles
                        newmodel = {m | tiles = before ++ [Nothing, Nothing, Nothing] ++ after }
                    in
                        if falls then
                            {newmodel | fallingTiles = newmodel.fallingTiles ++ triplet}
                        else
                            newmodel
            in
                positionsAbove gridSize index
                    |> List.foldl (\i m -> removeTriplet True i m) (removeTriplet False index model)
        Nothing -> model

positionsAbove: Int -> Int -> List Int
positionsAbove n i =
    let rowsAbove = (n*n - i - 1) // n
    in
        List.range 1 rowsAbove
            |> List.map (\j -> j*n + i)

updateSwap: Computer -> Model -> Model
updateSwap comp model =
    if comp.mouse.click then
        let
            index = (indexOf comp.mouse.x comp.mouse.y)
            _ = Debug.log "mouse, index:" (comp.mouse, index)
        in
        case model.selectedTile of
            Just selected ->
                if isAdjacent index selected then
                    {model | tiles = swap index selected model.tiles
                    , selectedTile = Nothing}
                else
                    {model | selectedTile = Nothing}
            Nothing -> {model | selectedTile = Just index }
    else
        model

indexOf: Float -> Float -> Int
indexOf x y =
    let halfSize = tileSideIn/2
        col = floor ((x + halfSize) / tileSideOut)
        row = floor ((y + halfSize) / tileSideOut)
    in row * gridSize + col

isAdjacent : Int->Int->Bool
isAdjacent i j =
    abs (i - j) == 1 && (i // gridSize) == (j // gridSize) --same row
        || abs (i - j) == gridSize && (modBy gridSize j) == (modBy gridSize i) -- same column

swap : Int -> Int -> List (Maybe Tile) -> List (Maybe Tile)
swap i j tiles =
   let
       tileAt : Int -> Maybe Tile
       tileAt x = tiles |> List.drop x |> List.head |> Maybe.andThen identity
       itile = tileAt i
       jtile = tileAt j
       vertical = modBy gridSize i == modBy gridSize j
   in
       case (itile, jtile, vertical) of
           (Just it, Just jt, _) ->
               tiles
                   |> updateTileAt i (Just {it|value = jt.value})
                   |> updateTileAt j (Just {jt|value = it.value})

           (Just it, Nothing, False)  ->
                tiles
                    |> updateTileAt i Nothing
                    |> updateTileAt j (Just {it|col = modBy gridSize j})
           (_, Just jt, False)  ->
                tiles
                    |> updateTileAt i (Just {jt|col = modBy gridSize i})
                    |> updateTileAt j Nothing
           _ -> tiles
