import Dict
import Keyboard
import Char

side = 30
n = 15
gridSize = side * n

-- model

data Square = Black | White Char
type Grid = Dict.Dict (Int, Int) Square
type Cursor = (Int, Int)
type State = { grid: Grid, cursor: Cursor }

createState : Grid -> Cursor -> State
createState grid cursor = { grid = grid, cursor = cursor }

start = [((1, 1), Black), ((1, 3), White 'A')]

defaultGrid = Dict.fromList start

-- keyboard

lastKey : Signal Char
lastKey = Char.toUpper <~ (Char.fromCode <~ Keyboard.lastPressed)

-- coordinates

toScreen : (Int, Int) -> (Int, Int)
toScreen (x, y) = (side * (x - 1), side * (y - 1))

fromScreen : (Int, Int) -> (Int, Int)
fromScreen (x, y) = (1 + div x side, 1 + div y side)

-- square

sqColor s = case s of
              Black -> black
              White _ -> white

sqLetter s = case s of
               Black -> '#'
               White c -> c

-- grid

getSq grid x y =
  case (Dict.get (x, y) grid) of
    Just s -> s
    Nothing -> White ' '

border : Element -> Element
border e =
  let iside = side - 2
      inner = container iside iside middle e |> color white
  in container side side middle inner

drawSquare : Square -> Element
drawSquare s = case s of
  Black -> container side side middle (plainText "#")
  White c -> let contents = plainText (String.fromList [c])
             in border contents 

drawCursor : Element
drawCursor = container side side middle (plainText " ") |> color green |> opacity 0.5

sq : Grid -> Int -> Int -> Element
sq grid x y = drawSquare <| getSq grid x y

renderCursor : Cursor -> Element
renderCursor cursor =
  let (px, py) = toScreen cursor
  in container gridSize gridSize (topLeftAt (absolute px) (absolute py)) drawCursor

drawGrid : Grid -> Element
drawGrid grid =
  flow right <| map (\x -> flow down <| map (sq grid x) [1 .. n]) [1 .. n]

renderGrid : Grid -> Element
renderGrid grid = container gridSize gridSize topLeft (drawGrid grid) |> color black

renderAll : State -> Element
renderAll state = layers [renderGrid state.grid, renderCursor state.cursor]

addKey : Char -> Grid
addKey key = Dict.insert (5, 5) (White key) defaultGrid

main =
  let grid0 = lift addKey lastKey
      cursor0 = constant (1, 3)
      state = lift2 createState grid0 cursor0
      rendered = lift renderAll state
  in lift (container (gridSize + 60) (gridSize + 60) middle) rendered
