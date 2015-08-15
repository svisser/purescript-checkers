module Checkers.Types where

import Data.Maybe
import Data.Tuple

type Coordinate = Tuple Int Int

type Player = Int

type Piece = {
    player :: Player,
    color  :: String,
    king   :: Boolean
}

type Square = {
    ox      :: Number,
    oy      :: Number,
    x       :: Int,
    y       :: Int,
    rx      :: Number,
    ry      :: Number,
    color   :: String,
    piece   :: Maybe Piece
}

type Grid = {
    width   :: Int,
    height  :: Int,
    squares :: Array Square
}

type State = {
    grid               :: Grid,
    currentPlayer      :: Player,
    selectedCoordinate :: Maybe Coordinate
}
