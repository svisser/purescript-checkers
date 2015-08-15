module Checkers.Types where

import Data.Maybe
import Data.Tuple

type Pixel = Tuple Number Number

type Coordinate = Tuple Int Int

type Player = Int

type Piece = {
    player :: Player,
    color  :: String,
    king   :: Boolean
}

type Square = {
    offset     :: Pixel,
    coordinate :: Coordinate,
    render     :: Pixel,
    color      :: String,
    piece      :: Maybe Piece
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
