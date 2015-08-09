module Checkers.Types where

import Data.Maybe
import Data.Tuple

type Coordinate = Tuple Int Int

type Player = Int

type Piece = {
    color :: String,
    king  :: Boolean
}

type Square = {
    x     :: Int,
    y     :: Int,
    rx    :: Number,
    ry    :: Number,
    color :: String,
    piece :: Maybe Piece
}

type Grid = {
    width   :: Int,
    height  :: Int,
    squares :: Array Square
}

type State = {
    grid          :: Grid,
    currentPlayer :: Player
}
