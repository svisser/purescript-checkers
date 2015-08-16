module Checkers.Constants where

import Data.Int (toNumber)
import Prelude

import Checkers.Types

defaultWidth    = 8
defaultHeight   = 8
layerCount      = 3

playerOne       = Player 1
playerTwo       = Player 2

highlightWidth  = 2.0
renderSize      = 60.0
renderWidth     = (toNumber defaultWidth) * renderSize
renderHeight    = (toNumber defaultHeight) * renderSize
renderDimension = { width: renderWidth, height: renderHeight }

colorBorder     = "black"
colorSquareOne  = "#d18b47"
colorSquareTwo  = "#ffce9e"
colorPlayerOne  = "red"
colorPlayerTwo  = "white"
