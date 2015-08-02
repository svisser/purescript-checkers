module Main where

import Control.Monad.Eff
import Control.Monad.ST
import Data.Array
import Data.Int (even, odd, toNumber)
import Data.Maybe
import Data.Maybe.Unsafe
import Data.Tuple
import Graphics.Canvas
import Math
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

findPiece :: Coordinate -> Grid -> Maybe Int
findPiece (Tuple x y) grid = findIndex (\e -> e.x == x && e.y == y) grid.squares

setPiece :: Maybe Piece -> Square -> Square
setPiece piece square = {
  x: square.x,
  y: square.y,
  rx: square.rx,
  ry: square.ry,
  color: square.color,
  piece: piece }

movePiece :: Coordinate -> Coordinate -> Grid -> Grid
movePiece from to grid = { width: grid.width, height: grid.height, squares: newSquares }
    where
      fromIndex = fromJust (findPiece from grid)
      toIndex = fromJust (findPiece to grid)
      originalSquare = fromJust (grid.squares !! fromIndex)
      afterMove = fromJust (modifyAt toIndex (setPiece originalSquare.piece) grid.squares)
      newSquares = fromJust (modifyAt fromIndex (setPiece Nothing) afterMove)

renderSquare :: forall e. Context2D -> Unit -> Square -> Eff (canvas :: Canvas | e) Unit
renderSquare ctx _ square = do
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
        return unit
    _ -> return unit

render :: forall s e. Context2D -> STRef s State -> Eff (st :: ST s, canvas :: Canvas | e) Unit
render ctx st = do
  state <- readSTRef st
  withContext ctx $ do
    foldM (renderSquare ctx) unit state.grid.squares
  withContext ctx $ do
    setStrokeStyle "black" ctx
    strokeRect ctx { x: 0.5,
                     y: 0.5,
                     w: (toNumber state.grid.width) * renderSize,
                     h: (toNumber state.grid.height) * renderSize }
  return unit

main :: forall s e. (Eff (st :: ST s, canvas :: Canvas | e) Unit)
main = do
  st <- newSTRef {
    grid: (createGrid 8 8 3),
    currentPlayer: 1
  }
  element <- getCanvasElementById "canvas"
  case element of
    Just canvas -> do
      ctx <- getContext2D canvas
      render ctx st
      return unit
    _ -> return unit
