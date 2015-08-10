module Checkers.Constants where

import Data.Int (toNumber)
import Prelude

defaultWidth    = 8
defaultHeight   = 8
layerCount      = 3
renderSize      = 60.0
renderWidth     = (toNumber defaultWidth) * renderSize
renderHeight    = (toNumber defaultHeight) * renderSize
renderDimension = { width: renderWidth, height: renderHeight }

colorSquareOne  = "#d18b47"
colorSquareTwo  = "#ffce9e"
colorPlayerOne  = "red"
colorPlayerTwo  = "white"
