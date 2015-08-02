module Main where

import Control.Monad.Eff
import Data.Array
import Data.Int
import Data.Maybe
import Graphics.Canvas
import Prelude

type Player = String

type Square = { x :: Int, y :: Int, rx :: Number, ry :: Number, color :: String }

type Grid = Array Square

renderSize :: Number
renderSize = 60.0

createGrid :: Int -> Int -> Grid
createGrid width height = do
  x <- 0 .. (width - 1)
  y <- 0 .. (height - 1)
  let rx = (toNumber x) * renderSize
      ry = (toNumber y) * renderSize
      color = if disj (conj (even x) (odd y)) (conj (even y) (odd x))
              then "#d18b47"
              else "#ffce9e"
  return { x: x, y: y, rx: rx, ry: ry, color: color }

renderSquare :: forall e. Context2D -> Unit -> Square -> Eff (canvas :: Canvas | e) Unit
renderSquare ctx _ square = do
  save ctx
  setFillStyle square.color ctx
  fillRect ctx { x: square.rx, y: square.ry, w: renderSize, h: renderSize }
  restore ctx
  return unit

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
      render ctx (createGrid 8 8)
      return unit
    _ -> return unit
