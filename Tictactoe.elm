import Basics exposing (..)
import List exposing (append, repeat, indexedMap)
import Html exposing (h1, h2, div, button, input, text, a, p)
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
  , showDebugInfo: Bool
  }

model =
  { isXMove = True
  , gameIsOver = False
  , plays = { x = [], o = [] }
  , winner = { x = False, o = False }
  , totalSpaces = 9
  , startScreenVisible = True
  , showDebugInfo = False
  }

-- Update

type Msg
  = MakeMove Int (Int, Int)
  | NewGame
  | ToggleDebugInfo  Bool

update msg model =
  case msg of
    MakeMove index (row, col) ->
      if List.member (row, col) (List.append model.plays.x model.plays.o) then
        { model | winner = { x = False, o = False } }
      else if fst (determineWinner model.plays) then
        { model |
          gameIsOver = True
          , winner = { x = True, o = False }
          , startScreenVisible = True }
      else if snd (determineWinner model.plays) then
        { model |
          gameIsOver = True
          , winner = { x = False, o = True }
          , startScreenVisible = True }
      else if List.length (List.append model.plays.x model.plays.o) <= model.totalSpaces then
        let
          plays = makePlay model.plays model.isXMove (row, col)
        in
          { model |
            isXMove = not model.isXMove
            , plays = plays
            , winner = { x = fst (determineWinner plays), o = snd (determineWinner plays) }
            , gameIsOver = gameIsOver plays
            , startScreenVisible = gameIsOver plays }
      else
        { model |
          gameIsOver = True
          , startScreenVisible = True }
    NewGame ->
      { model |
        isXMove = True
        , gameIsOver = False
        , plays = { x = [], o = [] }
        , winner = { x = False, o = False }
        , totalSpaces = 9
        , startScreenVisible = False
      }
    ToggleDebugInfo bool ->
      { model | showDebugInfo = bool }

-- View

makePlay plays isXMove spacePlayed =
  if not (List.member spacePlayed (List.append plays.x plays.o)) then
    if isXMove == True then
      { plays | x = List.append plays.x [spacePlayed] }
    else
      { plays | o = List.append plays.o [spacePlayed] }
  else
    plays

makeSquare index string =
  [ input [ value string
  , style [ ("width", "calc(100%/3)")
    , ("box-sizing", "border-box")
    , ("cursor", "pointer")
    , ("height", "calc(100vh/3)")
    , ("text-align", "center")
    , ("font-size", "3em") ]
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

getLastMove playerMoves =
  List.sum (List.take 1 (List.reverse playerMoves))

getCurrentPlayerMoves moves =
  if List.length moves.x > List.length moves.o then
    moves.x
  else
    moves.o

getStraightPlays moves checkXAxis =
  if checkXAxis then
    List.unzip moves
      |> fst
  else
    List.unzip moves
      |> snd

checkStraight list last =
  List.length (List.filter (\n -> n == last) list) == 3

straightWin playerMoves =
  checkStraight (getStraightPlays playerMoves True) (getLastMove (getStraightPlays playerMoves True)) ||
    checkStraight (getStraightPlays playerMoves False) (getLastMove (getStraightPlays playerMoves False))

checkDiagonal axes =
  fst axes == snd axes || (2, 0) == axes || (0, 2) == axes

checkZeroZeroDiagonal axes =
  fst axes == snd axes

diagonalWin playerMoves checkXAxis =
  List.sum (getUnzippedDiagonal playerMoves checkXAxis) == 3 &&
    List.length (getUnzippedDiagonal playerMoves checkXAxis) == 3

getUnzippedDiagonal list checkXAxis =
  if (List.member (0, 0) list) || (List.member (2,2) list) then
    if checkXAxis then
      fst (List.unzip (List.filter checkZeroZeroDiagonal list))
    else
      snd (List.unzip (List.filter checkZeroZeroDiagonal list))
  else
    if checkXAxis then
      fst (List.unzip (List.filter checkDiagonal list))
    else
      snd (List.unzip (List.filter checkDiagonal list))

didWin playerMoves =
  if (straightWin playerMoves) then
    True
  else if (diagonalWin playerMoves True) || (diagonalWin playerMoves False) then
    True
  else
    False

determineWinner playerMoves =
  if List.length playerMoves.x > 2 then
    if List.length playerMoves.x > List.length playerMoves.o then
      if didWin playerMoves.x then
        (True, False)
      else
        (False, False)
    else if List.length playerMoves.x == List.length playerMoves.o then
      if didWin playerMoves.o then
        (False, True)
      else
        (False, False)
    else
      (False, False)
  else
    (False, False)

gameIsOver playerMoves =
  fst (determineWinner playerMoves) ||
    snd (determineWinner playerMoves) ||
    List.length (List.append playerMoves.x playerMoves.o) == 9

displayWinnerMessage plays =
  if fst (determineWinner plays) then
    "X Wins!"
  else if snd (determineWinner plays) then
    "O Wins!"
  else
    "No winner."

playMessage gameIsOver =
  if gameIsOver then
    "Play again?"
  else
    "Play."

startScreenStyles isVisible =
  [ ("flex-direction", "column")
  , ("justify-content", "center")
  , ("align-items", "center")
  , ("background-color", "black")
  , ("color", "white")
  , ("position", "absolute")
  , ("height", "100vh")
  , ("width", "100vw")
  , ("display", "flex")
  , ("pointer-events", if not isVisible then "none" else "auto")
  , ("opacity", if not isVisible then "0" else "0.8")
  , ("transition", "opacity 0.3s ease")
  ]

view model =
  div [ style [("font-family", "sans-serif")] ]
    [ div [ style (startScreenStyles model.startScreenVisible) ]
      [ h1 [] [ text "Tic Tac Toe" ]
      , h2 [ style [ ("display", if model.gameIsOver then "block" else "none")
        , ("color", "gold")] ] [ text (displayWinnerMessage model.plays) ]
      , button [ style [
        ("background-color", "transparent")
        , ("font", "700 3rem/1 sans-serif")
        , ("padding", "0.5em 1em")
        , ("border", "3px solid white")
        , ("color", "white")
        , ("cursor", "pointer") ]
        , onClick NewGame ] [ text (playMessage model.gameIsOver) ]
      , h2 [] [ text "Written in ", a [ href "http://elm-lang.org/", style [("color", "white")] ] [ text "Elm." ] ]
      , p [] [ a [ href "https://github.com/zgreen/elm-tictactoe", style [("color", "white")] ] [ text "View on Github" ] ]
      , p [] [ a [ href "https://twitter.com/zgreen_", style [("color", "white")] ] [ text "By @zgreen_" ] ] ]
    , div [ style [("display", if model.showDebugInfo then "block" else "none"), ("font-family", "monospace")] ] [
      button [ onClick NewGame ] [ text "New Game" ]
      , div [] [ text ("is X move: " ++ toString model.isXMove) ]
      , div [] [ text ("game is over: " ++ (toString model.gameIsOver)) ]
      , div [] [ text (toString model.plays) ]
      , div [] [ text (toString model.winner) ] ]
    , button [ style [("position", "absolute"), ("top", "0"), ("right", "0")]
      , (onClick (ToggleDebugInfo (not model.showDebugInfo))) ] [ text "Show debug info?" ]
    , div [ style [("display", "flex"), ("flex-wrap", "wrap")] ]
      (spaces [] 0 model.plays)
    ]
