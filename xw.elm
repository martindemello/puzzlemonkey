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

sqr : Color -> Char -> Element
sqr c l = collage side side [
             square side |> filled c
           , square side |> outlined (solid black)
           , plainText [l] |> toForm
          ]

renderCell : Square -> Element
renderCell square = sqr (sqColor square) (sqLetter square)

crsr = opacity 0.5 <| collage side side [ square side |> filled green ]

sq grid x y =
  let (px, py) = toScreen (x, y)
      cell = getSq grid x y
  in renderCell cell |> toForm |> move (px, py)

renderCursor state =
  let (px, py) = toScreen state.cursor
  in  crsr |> toForm |> move (px, py)

renderGrid state =
  concatMap (\x -> map (sq state.grid x) [1 .. n]) [1 .. n]

renderAll : State -> [Form]
renderAll state = concat [renderGrid state, [renderCursor state]]

addKey : Char -> Grid
addKey key = Dict.insert (5, 5) (White key) defaultGrid

main =
  let grid0 = lift addKey lastKey
      cursor0 = constant (2, 2)
      state = lift2 createState grid0 cursor0
      rendered = lift renderAll state
  in
  lift (collage 1200 1200) rendered
