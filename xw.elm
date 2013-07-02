import Dict
import Keyboard
import Char

side = 30
n = 15
x0 = side * n

-- model

data Square = Black | White Char
type Grid = Dict (Int, Int) Square
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
toScreen (x, y) = (side * x - x0, side * (n - y))
fromScreen (x, y) = (div (x + x0) side, n - div y side)
centre = toScreen (n `div` 2 + 1, n `div` 2 + 1)

-- square

sqColor s = case s of
              Black -> black
              White _ -> white

sqLetter s = case s of
               Black -> '#'
               White c -> c

-- grid

getSq grid x y =
  case (Dict.lookup (x, y) grid) of
    Just s -> s
    Nothing -> White ' '

border : Element -> Element
border e =
  let iside = side - 1
      inner = container iside iside middle e |> color white
  in container side side middle inner |> color black

drawSquare : Square -> Element
drawSquare s = case s of
  Black -> container side side middle (plainText "#") |> color black
  White c -> border (plainText [c])

drawCursor : Element
drawCursor = container side side middle (plainText " ") |> color green |> opacity 0.5

sq : Grid -> Int -> Int -> Element
sq grid x y = drawSquare <| getSq grid x y

renderCursor : Cursor -> Form
renderCursor cursor =
  let (px, py) = toScreen cursor
  in  drawCursor |> toForm |> move (px, py)

drawGrid : Grid -> Element
drawGrid grid =
  flow right <| map (\x -> flow down <| map (sq grid x) [1 .. n]) [1 .. n]

renderGrid : Grid -> Form
renderGrid grid = drawGrid grid |> toForm |> move centre

renderAll : State -> [Form]
renderAll state = [renderGrid state.grid, renderCursor state.cursor]

addKey : Char -> Grid
addKey key = Dict.insert (5, 5) (White key) defaultGrid

main =
  let grid0 = lift addKey lastKey
      cursor0 = constant (1, 3)
      state = lift2 createState grid0 cursor0
      rendered = lift renderAll state
  in
  lift (collage 1200 1200) rendered
