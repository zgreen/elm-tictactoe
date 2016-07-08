import Basics exposing (..)
import List exposing (append, repeat, indexedMap)
import Html exposing (h1, h2, div, button, input, text)
import Html.Attributes exposing (..)
import Html.App as App
import Html.Events exposing (onClick)

main =
  App.beginnerProgram
    { model = model
    , view = view
    , update = update
    }

-- Model

type alias Plays =
  { x: List
  , o: List
  }
type alias Winner =
  { x: Bool
  , o: Bool
  }
type alias Model =
  { isXMove: Bool
  , gameIsOver: Bool
  , moveMade: List
  , plays: Plays
  , winner: Winner
  , totalSpaces: Int
  , startScreenVisible: Bool
  }

model =
  { isXMove = True
  , gameIsOver = False
  , plays = { x = [], o = [] }
  , winner = { x = False, o = False }
  , totalSpaces = 9
  , startScreenVisible = True
  }

-- Update

type Msg
  = WantsToPlay
  | MakeMove Int (Int, Int)
  | NewGame

update msg model =
  case msg of
    WantsToPlay ->
      { model | startScreenVisible = False }
    MakeMove index (row, col) ->
      if (determineWinner model.plays.x model.winner.x) then
        { model | gameIsOver = True }
      else if List.length (List.append model.plays.x model.plays.o) <= model.totalSpaces then
        let
          plays = makePlay model.plays model.isXMove (row, col)
        in
          { model |
            isXMove = not model.isXMove
            , plays = plays
            , gameIsOver = (determineWinner plays.x model.winner.x)
        }
      else
        { model | gameIsOver = True }
    NewGame ->
      { model |
        isXMove = True
        , gameIsOver = False
        , plays = { x = [], o = [] }
        , winner = { x = False, o = False }
        , totalSpaces = 9
        , startScreenVisible = True
      }

-- View

makePlay plays isXMove spacePlayed =
  if isXMove == True then
    { plays | x = List.append plays.x [spacePlayed] }
  else
    { plays | o = List.append plays.o [spacePlayed] }

makeSquare index string =
  [ input [ value string
  , style [ ("width", "calc(100%/3)"), ("box-sizing", "border-box"), ("cursor", "pointer"), ("height", "calc(100vh/3)"), ("text-align", "center"), ("font-size", "3em") ]
  , onClick (MakeMove index (space index)) ] []
  ]

space index =
  (index // 3, (index % 3))

spaces list index plays =
  if index > 7 then
    List.append list (makeSquare index (move (space index) plays))
  else
    spaces (List.append list (makeSquare index (move (space index) plays))) (index + 1) plays

move space plays =
  if List.member space plays.x then
    "x"
  else if List.member space plays.o then
    "o"
  else
    ""

getPlayerMoves playerMoves checkXAxis =
  if checkXAxis == True then
    List.unzip playerMoves
      |> fst
  else
    List.unzip playerMoves
      |> snd

determineWinner playerMoves winner =
  if List.length (getPlayerMoves playerMoves True) > 2 then
    if List.maximum (getPlayerMoves playerMoves True) == List.minimum (getPlayerMoves playerMoves True) then
      True
    else if List.maximum (getPlayerMoves playerMoves False) == List.minimum (getPlayerMoves playerMoves False) then
      True
    else if List.sum (getPlayerMoves playerMoves True) == 3
      && List.sum (getPlayerMoves playerMoves False) == 3 then
      True
    else
      False
  else
    False

view model =
  div [ style [("font-family", "sans-serif")] ]
    [ div [ style [("background-color", "black"), ("color", "white"), ("position", "absolute"), ("height", "100vh"), ("width", "100vw"), ("display", if not model.startScreenVisible then "none" else "block")] ]
      [ h1 [] [ text "Tic Tac Toe" ]
      , h2 [] [ text "Written in Elm." ]
      , button [ onClick (WantsToPlay)] [ text "Play." ] ]
    , button [ onClick (MakeMove -1 (9,9)) ] [ text "Make Move" ]
    , button [ onClick NewGame ] [ text "New Game" ]
    , div [] [ text (toString model.isXMove) ]
    , div [] [ text ("game is over: " ++ (toString model.gameIsOver)) ]
    , div [] [ text (toString model.plays) ]
    , div [] [ text (toString model.winner) ]
    , input
      [ placeholder (toString model.isXMove)
      , onClick (MakeMove -1 (9,9)) ] []
    , div [ style [("display", "flex"), ("flex-wrap", "wrap")] ]
      (spaces [] 0 model.plays)
    ]
