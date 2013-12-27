module Brick
    (
    Brick(..),
    posX, posY,
    sizeX, sizeY,
    top, bottom,
    left, right,
    ) where

import Data.Vect.Float ( Vec2(..), _1, _2 )

import qualified Plane as Pl

data Brick = Brick { pos :: Vec2, size :: Vec2 }

posX :: Brick -> Float
posX b = _1 . pos $ b
posY :: Brick -> Float
posY b = _2 . pos $ b

sizeX :: Brick -> Float
sizeX b = _1 . size $ b
sizeY :: Brick -> Float
sizeY b = _2 . size $ b

-- | Return plane for the top of the paddle
top :: Brick -> Pl.Plane
top brick = Pl.Plane (pos brick) $ Vec2 0 (-1)

-- | Return plane for the bottom of the paddle
bottom :: Brick -> Pl.Plane
bottom brick = Pl.Plane (pos brick + (Vec2 0 $ sizeY brick)) $ Vec2 0 1

-- | Return plane for the left side of the paddle
left :: Brick -> Pl.Plane
left brick = Pl.Plane (pos brick) $ Vec2 (-1) 0

-- | Return plane for the right side of the paddle
right :: Brick -> Pl.Plane
right brick = Pl.Plane (pos brick + (Vec2 (sizeX brick) 0)) $ Vec2 1 0

