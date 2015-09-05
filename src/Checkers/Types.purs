module Checkers.Types where

import Data.Maybe
import Data.Tuple
import Prelude

type Pixel = Tuple Number Number

type Coordinate = Tuple Int Int

newtype Player = Player Int

instance eqPlayer :: Eq Player where
    eq (Player a) (Player b) = a == b

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
    grid                  :: Grid,
    currentPlayer         :: Player,
    selectedCoordinate    :: Maybe Coordinate,
    highlightedCoordinate :: Maybe Coordinate
}
