module Brick
    (
    Brick(..),
    posX, posY,
    sizeX, sizeY,
    ) where

import Data.Vect.Float ( Vec2(..), _1, _2 )

data Brick = Brick { pos :: Vec2, size :: Vec2 }

posX :: Brick -> Float
posX b = _1 . pos $ b
posY :: Brick -> Float
posY b = _2 . pos $ b

sizeX :: Brick -> Float
sizeX b = _1 . size $ b
sizeY :: Brick -> Float
sizeY b = _2 . size $ b
