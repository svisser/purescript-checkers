module Checkers.Main where

import Control.Monad.Eff
import Control.Monad.ST
import Control.MonadPlus
import Data.Array
import Data.DOM.Simple.Document
import Data.DOM.Simple.Element
import Data.DOM.Simple.Events
import Data.DOM.Simple.Types
import Data.DOM.Simple.Unsafe.Events
import Data.DOM.Simple.Window
import Data.Int
import Data.Maybe
import Data.Maybe.Unsafe
import Data.Tuple
import DOM
import Graphics.Canvas
import Math (abs, max, pi)
import Prelude

import Checkers.Constants
import Checkers.Types

createPiece :: Player -> Piece
createPiece player =
  { player: player,
    color:  if player == playerOne then colorPlayerOne else colorPlayerTwo,
    king:   false }

createGrid :: Pixel -> Int -> Int -> Int -> Grid
createGrid offset width height layers = { width: width, height: height, squares: squares }
  where
    squares = do
      x <- 0 .. (width - 1)
      y <- 0 .. (height - 1)
      let rx = (toNumber x) * renderSize
          ry = (toNumber y) * renderSize
          isPieceSquare = ((even x) && (odd y)) || ((even y) && (odd x))
          color = if isPieceSquare then colorSquareOne else colorSquareTwo
          piece = if isPieceSquare && (y < layers)
                  then Just (createPiece playerOne)
                  else if isPieceSquare && (y >= height - layers)
                       then Just (createPiece playerTwo)
                       else Nothing
      return { offset:     offset,
               coordinate: (Tuple x y),
               render:     (Tuple rx ry),
               color:      color,
               piece:      piece }

createState :: Pixel -> Int -> Int -> Int -> State
createState offset width height layers =
  { grid:                  createGrid offset width height layers,
    currentPlayer:         playerOne,
    selectedCoordinate:    Nothing,
    highlightedCoordinate: Nothing }

setPiece :: Maybe Piece -> Square -> Square
setPiece piece square = square { piece = piece }

isValid :: Grid -> Coordinate -> Boolean
isValid grid (Tuple x y) = x >= 0 && x < grid.width && y >= 0 && y < grid.height

hasPlayer :: Player -> Piece -> Boolean
hasPlayer player piece = piece.player == player

hasCoordinate :: Coordinate -> Square -> Boolean
hasCoordinate coordinate square = coordinate == square.coordinate

getSquare :: Array Square -> Coordinate -> Maybe Square
getSquare squares coordinate =
  fromMaybe Nothing $ (squares !!) <$> (findIndex (hasCoordinate coordinate) squares)

hasPlayerPiece :: Array Square -> Player -> Coordinate -> Boolean
hasPlayerPiece squares player coordinate =
  case getSquare squares coordinate of
    Nothing -> false
    Just square -> fromMaybe false $ (hasPlayer player) <$> square.piece

hasPiece :: Array Square -> Coordinate -> Boolean
hasPiece squares coordinate =
  hasPlayerPiece squares playerOne coordinate ||
  hasPlayerPiece squares playerTwo coordinate

getDiagonalSquares :: Coordinate -> Array Coordinate
getDiagonalSquares (Tuple x y) = do
  i <- -1 : (singleton 1)
  j <- -1 : (singleton 1)
  offset <- 1 : (singleton 2)
  return (Tuple (x + i * offset) (y + j * offset))

isOnSquare :: Pixel -> Square -> Boolean
isOnSquare (Tuple x y) square =
  let px = square.render + square.offset
      px' = px + (Tuple renderSize renderSize)
  in fst px < x && x < fst px' && snd px < y && y < snd px'

isOnActiveSquare :: Grid -> Player -> Pixel -> Square -> Boolean
isOnActiveSquare grid player pixel square =
  let moves = getMoves grid player square.coordinate
  in isOnSquare pixel square && not (null moves)

getActiveCoordinate :: Grid -> Player -> Pixel -> Maybe Coordinate
getActiveCoordinate grid player pixel =
  case findIndex (isOnActiveSquare grid player pixel) grid.squares of
    Just index -> _.coordinate <$> grid.squares !! index
    Nothing    -> Nothing

findCoordinate :: Grid -> Player -> Pixel -> Maybe Coordinate
findCoordinate grid player pixel =
  case findIndex (isOnSquare pixel) grid.squares of
    Just index -> _.coordinate <$> grid.squares !! index
    Nothing    -> Nothing

getHighlightCoordinate :: State -> Pixel -> Maybe Coordinate
getHighlightCoordinate state pixel =
  case findCoordinate state.grid state.currentPlayer pixel of
    Nothing -> Nothing
    Just to -> do
      case state.selectedCoordinate of
        Nothing -> getActiveCoordinate state.grid state.currentPlayer pixel
        Just from ->
          if isValidMove state.grid state.currentPlayer from to
          then Just to
          else getActiveCoordinate state.grid state.currentPlayer pixel

isRegularMove :: Player -> Coordinate -> Coordinate -> Boolean
isRegularMove (Player 1) from to =
  let d1 = from + Tuple   1    1
      d2 = from + Tuple (-1)   1
  in d1 == to || d2 == to
isRegularMove _ from to =
  let d3 = from + Tuple   1  (-1)
      d4 = from + Tuple (-1) (-1)
  in d3 == to || d4 == to

isJumpMove :: Grid -> Player -> Coordinate -> Coordinate -> Boolean
isJumpMove grid (Player 1) from to =
  let d1 = from + Tuple   1    1
      d2 = from + Tuple (-1)   1
      d5 = from + Tuple   2    2
      d6 = from + Tuple (-2)   2
      f = hasPlayerPiece grid.squares playerTwo
  in d5 == to && f d1 || d6 == to && f d2
isJumpMove grid _ from to =
  let d3 = from + Tuple   1  (-1)
      d4 = from + Tuple (-1) (-1)
      d7 = from + Tuple   2  (-2)
      d8 = from + Tuple (-2) (-2)
      f = hasPlayerPiece grid.squares playerOne
  in d7 == to && f d3 || d8 == to && f d4

isValidMove :: Grid -> Player -> Coordinate -> Coordinate -> Boolean
isValidMove grid player from to =
  isValid grid to && not hasPiece grid.squares to &&
  (isRegularMove player from to || isJumpMove grid player from to)

getMoves :: Grid -> Player -> Coordinate -> Array Coordinate
getMoves grid player coordinate = do
  guard $ hasPlayerPiece grid.squares player coordinate
  potential <- getDiagonalSquares coordinate
  guard $ isValidMove grid player coordinate potential
  return potential

alterPiece :: Maybe Piece -> Coordinate -> Grid -> Grid
alterPiece piece coordinate grid = grid { squares = newSquares }
  where
    index = fromJust (findIndex (hasCoordinate coordinate) grid.squares)
    newSquares = fromJust (modifyAt index (setPiece piece) grid.squares)

removePiece :: Coordinate -> Grid -> Grid
removePiece = alterPiece Nothing

movePiece :: Coordinate -> Coordinate -> Grid -> Grid
movePiece from to grid =
  let fromIndex = fromJust (findIndex (hasCoordinate from) grid.squares)
      originalSquare = fromJust (grid.squares !! fromIndex)

      getMiddle :: Coordinate -> Coordinate -> Maybe Coordinate
      getMiddle (Tuple x1 y1) (Tuple x2 y2) =
        if abs (toNumber (y1 - y2)) == 2.0
        then
          let n1 = floor (max (toNumber x1) (toNumber x2) - 1.0)
              n2 = floor (max (toNumber y1) (toNumber y2) - 1.0)
          in Just (Tuple n1 n2)
        else Nothing
  in
    case getMiddle from to of
      Nothing -> removePiece from (alterPiece originalSquare.piece to grid)
      Just middle -> removePiece middle (removePiece from (alterPiece originalSquare.piece to grid))

otherPlayer :: Player -> Player
otherPlayer (Player 1) = Player 2
otherPlayer (Player 2) = Player 1
otherPlayer _          = Player 1

renderSquare :: forall e.
                  Context2D ->
                  Unit ->
                  Square ->
                  Eff (canvas :: Canvas, dom :: DOM | e) Unit
renderSquare ctx _ square = do
  setFillStyle square.color ctx
  fillRect ctx { x: fst square.render,
                 y: snd square.render,
                 w: renderSize,
                 h: renderSize }
  return unit

renderPiece :: forall e.
                 State ->
                 Context2D ->
                 Unit ->
                 Square ->
                 Eff (canvas :: Canvas, dom :: DOM | e) Unit
renderPiece state ctx _ square = do
  case square.piece of
    Just piece -> do
      setFillStyle piece.color ctx
      let arcPiece = { x: fst square.render + 0.5 * renderSize,
                       y: snd square.render + 0.5 * renderSize,
                       r: (renderSize / 2.0) * 0.8,
                       start: 0.0,
                       end: 2.0 * pi }
      fillPath ctx $ arc ctx arcPiece
      return unit
    _ -> return unit

renderBorder :: forall e. Context2D -> Grid -> Eff (canvas :: Canvas | e) Unit
renderBorder ctx grid = do
  let border = { x: 0.5,
                 y: 0.5,
                 w: (toNumber grid.width) * renderSize - 0.5,
                 h: (toNumber grid.height) * renderSize - 0.5 }
  setStrokeStyle colorBorder ctx
  strokeRect ctx border
  return unit

renderHighlight :: forall e.
                     Context2D ->
                     Array Square ->
                     Maybe Coordinate ->
                     Eff (canvas :: Canvas, dom :: DOM | e) Unit
renderHighlight ctx squares selection = do
  case selection of
    Nothing -> return unit
    Just coordinate -> do
      let square = fromJust (getSquare squares coordinate)
          arcPiece = { x:     fst square.render + 0.5 * renderSize,
                       y:     snd square.render + 0.5 * renderSize,
                       r:     (renderSize / 2.0) * 0.8,
                       start: 0.0,
                       end:   2.0 * pi }
      setLineWidth highlightWidth ctx
      strokePath ctx $ arc ctx arcPiece
      return unit

render :: forall s e.
            Context2D ->
            STRef s State ->
            Eff (st :: ST s, canvas :: Canvas, dom :: DOM | e) Unit
render ctx st = do
  state <- readSTRef st
  withContext ctx $ do
    foldM (renderSquare ctx) unit state.grid.squares
  withContext ctx $ do
    foldM (renderPiece state ctx) unit state.grid.squares
  withContext ctx $ do
    renderBorder ctx state.grid
  withContext ctx $ do
    renderHighlight ctx state.grid.squares state.selectedCoordinate
  withContext ctx $ do
    renderHighlight ctx state.grid.squares state.highlightedCoordinate
  return unit

renderPage :: forall s e.
                STRef s State ->
                Eff (st :: ST s, canvas :: Canvas, dom :: DOM | e) Unit
renderPage st = do
  element <- getCanvasElementById "canvas"
  case element of
    Just canvas -> do
      setCanvasDimensions renderDimension canvas
      ctx <- getContext2D canvas
      render ctx st
      return unit
    _ -> return unit
  return unit

moveListener :: forall s e.
                  STRef s State ->
                  DOMEvent ->
                  Eff (st :: ST s, canvas :: Canvas, dom :: DOM | e) Unit
moveListener st e = do
  ux <- unsafeEventNumberProp "clientX" e
  uy <- unsafeEventNumberProp "clientY" e
  state <- readSTRef st
  let pixel = (Tuple (toNumber ux) (toNumber uy))
      highlightedCoordinate = getHighlightCoordinate state pixel
  writeSTRef st $ state { highlightedCoordinate = highlightedCoordinate }
  renderPage st
  return unit

clickListener :: forall s e.
                   STRef s State ->
                   DOMEvent ->
                   Eff (st :: ST s, canvas :: Canvas, dom :: DOM | e) Unit
clickListener st e = do
  element <- getCanvasElementById "canvas"
  case element of
    Just canvas -> do
      state <- readSTRef st
      ux <- unsafeEventNumberProp "clientX" e
      uy <- unsafeEventNumberProp "clientY" e
      let pixel = Tuple (toNumber ux) (toNumber uy)
          toCoordinate = getHighlightCoordinate state pixel
      case toCoordinate of
        Nothing -> return unit
        Just to -> do
          case state.selectedCoordinate of
            Nothing -> do
              writeSTRef st $ state { selectedCoordinate = Just to }
              renderPage st
              return unit
            Just from -> do
              case isValidMove state.grid state.currentPlayer from to of
                false -> do
                  writeSTRef st $ state { selectedCoordinate = if to == from
                                                               then Nothing
                                                               else Just to }
                  renderPage st
                  return unit
                true -> do
                  writeSTRef st $ state { highlightedCoordinate = Nothing,
                                          selectedCoordinate = Nothing,
                                          grid = movePiece from to state.grid,
                                          currentPlayer = otherPlayer state.currentPlayer }
                  renderPage st
                  return unit
    _ -> return unit
  return unit

main :: forall s e. Eff (st :: ST s, canvas :: Canvas, dom :: DOM | e) Unit
main = do
  doc <- document globalWindow
  mcanvas <- getElementById "canvas" doc
  case mcanvas of
    Just canvas -> do
      x <- offsetLeft canvas
      y <- offsetTop canvas
      let offset = (Tuple (toNumber x) (toNumber y))
      st <- newSTRef (createState offset defaultWidth defaultHeight layerCount)
      addMouseEventListener MouseMoveEvent (moveListener st) canvas
      addMouseEventListener MouseClickEvent (clickListener st) canvas
      renderPage st
      return unit
    _ -> return unit
