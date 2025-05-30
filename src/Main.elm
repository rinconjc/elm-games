module Main exposing (..)
import Playground exposing (..)

main = view_tiles
  -- game view update (0,0)

view computer (x,y) =
  [ square blue 400
      |> move x y
  ]

update computer (x,y) =
  ( x + toX computer.keyboard
  , y + toY computer.keyboard
  )


main_animation =
  animation view_animation

view_animation time =
  [ square blue 40
      |> moveX (zigzag -100 100 2 time)
  ]


view_tiles =
    picture (tiledGrid (emptyGrid 0))
        -- [square blue 40
        -- ,square blue 40 |> moveX 41]



-- Model
--
type alias Grid =
    List (List Int)

type alias Model =
    { grid: Grid
    , fallingOffset: Float
    , fallingCol : Int}
    


emptyGrid:Int -> Grid
emptyGrid value =
    List.repeat 5 (List.repeat 5 value)

-- tiledGrid: Grid -> List Shape
tiledGrid grid =
    List.concatMap
        (\r -> List.map(\c -> square grey 40 |> move ((toFloat c) * 41) ((toFloat r) * 41)) (List.range 0 4))
            (List.range 0 4)
