module Main where

import Control.Monad.Eff
import Data.Array
import Data.Int (even, odd, toNumber)
import Data.Maybe
import Graphics.Canvas
import Math
import Prelude

type Player = String

type Piece = { color :: String }

type Square = { x :: Int, y :: Int, rx :: Number, ry :: Number, color :: String, piece :: Maybe Piece }

type Grid = Array Square

renderSize = 60.0
colorOne = "red"
colorTwo = "white"

createGrid :: Int -> Int -> Int -> Grid
createGrid width height layers = do
  x <- 0 .. (width - 1)
  y <- 0 .. (height - 1)
  let rx = (toNumber x) * renderSize
      ry = (toNumber y) * renderSize
      color = if disj (conj (even x) (odd y)) (conj (even y) (odd x))
              then "#d18b47"
              else "#ffce9e"
      hasPlayerOne = conj (y < layers) (disj (conj (even y) (odd x)) (conj (even x) (odd y)))
      hasPlayerTwo = conj (y >= height - layers) (disj (conj (even y) (odd x)) (conj (even x) (odd y)))
      piece = if hasPlayerOne
              then Just { color: colorOne }
              else if hasPlayerTwo
              then Just { color: colorTwo }
              else Nothing
  return { x: x, y: y, rx: rx, ry: ry, color: color, piece: piece }

renderSquare :: forall e. Context2D -> Unit -> Square -> Eff (canvas :: Canvas | e) Unit
renderSquare ctx _ square = do
  save ctx
  setFillStyle square.color ctx
  fillRect ctx { x: square.rx, y: square.ry, w: renderSize, h: renderSize }
  restore ctx
  case square.piece of
    Just piece -> do
      save ctx
      setFillStyle piece.color ctx
      fillPath ctx $ arc ctx { x: square.rx + 0.5 * renderSize,
                               y: square.ry + 0.5 * renderSize,
                               r: (renderSize / 2.0) * 0.8,
                               start: 0.0,
                               end: 2.0 * pi }
      restore ctx
      return unit
    _ -> return unit

render :: forall e. Context2D -> Grid -> Eff (canvas :: Canvas | e) Unit
render ctx grid = do
  save ctx
  foldM (renderSquare ctx) unit grid
  restore ctx
  return unit

main :: forall e. Eff (canvas :: Canvas | e) Unit
main = do
  element <- getCanvasElementById "canvas"
  case element of
    Just canvas -> do
      ctx <- getContext2D canvas
      render ctx (createGrid 8 8 3)
      return unit
    _ -> return unit
