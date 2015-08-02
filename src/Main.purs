module Main where

import Control.Monad.Eff
import Data.Array
import Data.Int
import Data.Maybe
import Graphics.Canvas
import Prelude

type Player = String

type Square = { x :: Int, y :: Int }

type Grid = Array Square

renderSize :: Number
renderSize = 50.0

createGrid :: Int -> Int -> Grid
createGrid width height = do
  x <- 0 .. (width - 1)
  y <- 0 .. (height - 1)
  return { x: x, y: y }

renderRectangle :: Square -> Rectangle
renderRectangle square = { x: (toNumber square.x) * renderSize
                         , y: (toNumber square.y) * renderSize
                         , w: renderSize
                         , h: renderSize }

renderSquare :: forall e. Context2D -> Unit -> Rectangle -> Eff (canvas :: Canvas | e) Unit
renderSquare ctx _ rectangle = do
  save ctx
  setFillStyle "#ff0000" ctx
  fillRect ctx rectangle
  restore ctx
  return unit

render :: forall e. Context2D -> Array Rectangle -> Eff (canvas :: Canvas | e) Unit
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
      render ctx (map renderRectangle (createGrid 8 8))
      return unit
    _ -> return unit
