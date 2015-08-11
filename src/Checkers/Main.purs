module Checkers.Main where

import Control.Monad.Eff
import Control.Monad.Eff.Console
import Control.Monad.ST
import Data.Array
import Data.DOM.Simple.Document
import Data.DOM.Simple.Element
import Data.DOM.Simple.Events
import Data.DOM.Simple.Types
import Data.DOM.Simple.Unsafe.Events (unsafeEventNumberProp)
import Data.DOM.Simple.Window
import Data.Int (even, odd, toNumber)
import Data.Maybe
import Data.Maybe.Unsafe
import Data.Tuple
import DOM
import Graphics.Canvas
import Math (pi)
import Prelude

import Checkers.Constants
import Checkers.Types

createGrid :: Tuple Number Number -> Int -> Int -> Int -> Grid
createGrid (Tuple ox oy) width height layers = { width: width, height: height, squares: squares }
  where
    squares = do
      x <- 0 .. (width - 1)
      y <- 0 .. (height - 1)
      let rx = (toNumber x) * renderSize
          ry = (toNumber y) * renderSize
          color = if ((even x) && (odd y)) || ((even y) && (odd x))
                  then colorSquareOne
                  else colorSquareTwo
          hasPlayerOne = (y < layers) && (((even y) && (odd x)) || ((even x) && (odd y)))
          hasPlayerTwo = (y >= height - layers) && (((even y) && (odd x)) || ((even x) && (odd y)))
          piece = if hasPlayerOne
                  then Just { color: colorPlayerOne, king: false }
                  else
                    if hasPlayerTwo
                    then Just { color: colorPlayerTwo, king: false }
                    else Nothing
      return { ox: ox, oy: oy, x: x, y: y, rx: rx, ry: ry, color: color, piece: piece }

createState :: Tuple Number Number -> Int -> Int -> Int -> State
createState offset width height layers = { grid: (createGrid offset width height layers), currentPlayer: 1 }

findPiece :: Grid -> Coordinate -> Maybe Int
findPiece grid (Tuple x y) = findIndex (\e -> e.x == x && e.y == y) grid.squares

setPiece :: Maybe Piece -> Square -> Square
setPiece piece square = square { piece = piece }

getDiagonalSquares :: Coordinate -> Array Coordinate
getDiagonalSquares (Tuple x y) = do
  i <- -1 : (singleton 1)
  j <- -1 : (singleton 1)
  return (Tuple (x + i) (y + j))

isOnSquare :: Square -> Number -> Number -> Boolean
isOnSquare square x y =
  let vx = square.rx + square.ox
      vy = square.ry + square.oy
  in vx < x && x < vx + renderSize && vy < y && y < vy + renderSize

movePiece :: Coordinate -> Coordinate -> Grid -> Grid
movePiece from to grid = grid { squares = newSquares }
    where
      fromIndex = fromJust (findPiece grid from)
      toIndex = fromJust (findPiece grid to)
      originalSquare = fromJust (grid.squares !! fromIndex)
      afterMove = fromJust (modifyAt toIndex (setPiece originalSquare.piece) grid.squares)
      newSquares = fromJust (modifyAt fromIndex (setPiece Nothing) afterMove)

renderSquare :: forall e.
                  Context2D ->
                  Maybe DOMEvent ->
                  Unit ->
                  Square ->
                  Eff (canvas :: Canvas, console :: CONSOLE, dom :: DOM | e) Unit
renderSquare ctx event _ square = do
  withContext ctx $ do
    setFillStyle square.color ctx
    fillRect ctx { x: square.rx, y: square.ry, w: renderSize, h: renderSize }
  case square.piece of
    Just piece -> do
      withContext ctx $ do
        setFillStyle piece.color ctx
        let arcPiece = { x: square.rx + 0.5 * renderSize,
                         y: square.ry + 0.5 * renderSize,
                         r: (renderSize / 2.0) * 0.8,
                         start: 0.0,
                         end: 2.0 * pi }
        fillPath ctx $ arc ctx arcPiece
        case event of
          Nothing -> return unit
          Just e -> do
            ux <- unsafeEventNumberProp "clientX" e
            uy <- unsafeEventNumberProp "clientY" e
            case isOnSquare square (toNumber ux) (toNumber uy) of
              true -> do
                setLineWidth highlightWidth ctx
                strokePath ctx $ arc ctx arcPiece
                return unit
              false -> return unit
            return unit
        return unit
    _ -> return unit

render :: forall s e.
            Context2D ->
            STRef s State ->
            Maybe DOMEvent ->
            Eff (st :: ST s, canvas :: Canvas, console :: CONSOLE, dom :: DOM | e) Unit
render ctx st event = do
  state <- readSTRef st
  withContext ctx $ do
    foldM (renderSquare ctx event) unit state.grid.squares
  withContext ctx $ do
    setStrokeStyle "black" ctx
    strokeRect ctx { x: 0.5,
                     y: 0.5,
                     w: (toNumber state.grid.width) * renderSize - 0.5,
                     h: (toNumber state.grid.height) * renderSize - 0.5 }
  return unit

renderPage :: forall s e.
                STRef s State ->
                Maybe DOMEvent ->
                Eff (st :: ST s, canvas :: Canvas, console :: CONSOLE, dom :: DOM | e) Unit
renderPage st event = do
  element <- getCanvasElementById "canvas"
  case element of
    Just canvas -> do
      setCanvasDimensions renderDimension canvas
      ctx <- getContext2D canvas
      render ctx st event
      return unit
    _ -> return unit
  return unit

main :: forall s e. Eff (st :: ST s, canvas :: Canvas, console :: CONSOLE, dom :: DOM | e) Unit
main = do
  doc <- document globalWindow
  mcanvas <- getElementById "canvas" doc
  case mcanvas of
    Just canvas -> do
      x <- offsetLeft canvas
      y <- offsetTop canvas
      let offset = (Tuple (toNumber x) (toNumber y))
      st <- newSTRef (createState offset defaultWidth defaultHeight layerCount)
      addMouseEventListener MouseMoveEvent (\e -> renderPage st (Just e)) canvas
      renderPage st Nothing
      return unit
    _ -> return unit
