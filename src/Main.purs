module Main (main) where

import Control.Monad.Eff
import Control.Monad.Eff.Console
import Control.Monad.ST
import Data.Array
import Data.DOM.Simple.Document
import Data.DOM.Simple.Element
import Data.DOM.Simple.Events
import Data.DOM.Simple.Types
import Data.DOM.Simple.Window
import Data.Int (even, odd, toNumber)
import Data.Maybe
import Data.Maybe.Unsafe
import Data.Tuple
import DOM
import Graphics.Canvas
import Math (pi)
import Prelude

type Coordinate = Tuple Int Int

type Player = Int

type Piece = { color :: String }

type Square = { x :: Int, y :: Int, rx :: Number, ry :: Number, color :: String, piece :: Maybe Piece }

type Grid = { width :: Int, height :: Int, squares :: Array Square }

type State = { grid :: Grid, currentPlayer :: Player }

renderSize = 60.0
colorSquareOne = "#d18b47"
colorSquareTwo = "#ffce9e"
colorPlayerOne = "red"
colorPlayerTwo = "white"

createGrid :: Int -> Int -> Int -> Grid
createGrid width height layers = { width: width, height: height, squares: squares }
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
                then Just { color: colorPlayerOne }
                else
                  if hasPlayerTwo
                  then Just { color: colorPlayerTwo }
                  else Nothing
    return { x: x, y: y, rx: rx, ry: ry, color: color, piece: piece }

createState :: Int -> Int -> Int -> State
createState width height layers = { grid: (createGrid width height layers), currentPlayer: 1 }

findPiece :: Coordinate -> Grid -> Maybe Int
findPiece (Tuple x y) grid = findIndex (\e -> e.x == x && e.y == y) grid.squares

setPiece :: Maybe Piece -> Square -> Square
setPiece piece square = square { piece = piece }

movePiece :: Coordinate -> Coordinate -> Grid -> Grid
movePiece from to grid = grid { squares = newSquares }
    where
      fromIndex = fromJust (findPiece from grid)
      toIndex = fromJust (findPiece to grid)
      originalSquare = fromJust (grid.squares !! fromIndex)
      afterMove = fromJust (modifyAt toIndex (setPiece originalSquare.piece) grid.squares)
      newSquares = fromJust (modifyAt fromIndex (setPiece Nothing) afterMove)

renderSquare :: forall e. Context2D -> Maybe DOMEvent -> Unit -> Square -> Eff (canvas :: Canvas, console :: CONSOLE, dom :: DOM | e) Unit
renderSquare ctx event _ square = do
  withContext ctx $ do
    setFillStyle square.color ctx
    fillRect ctx { x: square.rx, y: square.ry, w: renderSize, h: renderSize }
  case square.piece of
    Just piece -> do
      withContext ctx $ do
        setFillStyle piece.color ctx
        fillPath ctx $ arc ctx { x: square.rx + 0.5 * renderSize,
                                 y: square.ry + 0.5 * renderSize,
                                 r: (renderSize / 2.0) * 0.8,
                                 start: 0.0,
                                 end: 2.0 * pi }
        case event of
          Nothing -> return unit
          Just e -> do
            screen_x <- screenX e
            screen_y <- screenY e
            let square_x = (toNumber screen_x) / renderSize
                square_y = (toNumber screen_y) / renderSize
            return unit
        return unit
    _ -> return unit

render :: forall s e. Context2D -> STRef s State -> Maybe DOMEvent -> Eff (st :: ST s, canvas :: Canvas, console :: CONSOLE, dom :: DOM | e) Unit
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

onMouseMoveListener :: forall s e. STRef s State -> DOMEvent -> Eff (st :: ST s, canvas :: Canvas, console :: CONSOLE, dom :: DOM | e) Unit
onMouseMoveListener st e = renderPage st (Just e)

renderPage st event = do
  element <- getCanvasElementById "canvas"
  case element of
    Just canvas -> do
      setCanvasDimensions { "width": 480.0, "height": 480.0 } canvas
      ctx <- getContext2D canvas
      render ctx st event
      return unit
    _ -> return unit
  return unit

main = do
  st <- newSTRef (createState 8 8 3)
  doc <- document globalWindow
  mcanvas <- getElementById "canvas" doc
  case mcanvas of
    Just canvas -> do
      addMouseEventListener MouseMoveEvent (onMouseMoveListener st) canvas
      renderPage st Nothing
      return unit
    _ -> return unit
