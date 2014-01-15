module Breakout.Types (
    HasPos,
    HasSize
    ) where

import Data.Vect.Float ( Vec2, _1, _2 )

class HasPos a where
    pos :: a -> Vec2

    posX :: a -> Float
    posX = _1 . pos

    posY :: a -> Float
    posY = _2 . pos

class HasSize a where
    size :: a -> Vec2

    sizeX :: a -> Float
    sizeX = _1 . size

    sizeY :: a -> Float
    sizeY = _2 . size

