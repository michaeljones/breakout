module Breakout.Brick
    (
    Brick(..),
    DeadBrick(..),
    BrickState(..),
    deathSequence,
    posX, posY,
    sizeX, sizeY
    ) where

import Data.Vect.Float ( Vec2(..), _1, _2 )

import qualified Breakout.Plane as Pl

data Brick = Brick { pos :: Vec2, size :: Vec2 } deriving (Eq, Show)

data DeadBrick = DeadBrick {
    brick :: Brick,
    fraction :: !Float
}

data BrickState = BrickState {
    current :: [Brick],
    dying :: [DeadBrick]
}

posX :: Brick -> Float
posX b = _1 . pos $ b
posY :: Brick -> Float
posY b = _2 . pos $ b

sizeX :: Brick -> Float
sizeX b = _1 . size $ b
sizeY :: Brick -> Float
sizeY b = _2 . size $ b

deathSequence :: BrickState -> BrickState
deathSequence brickState = brickState { dying=dying'' }
  where
    dying' = dying brickState
    dying'' = map animate $ filter cull dying' 
    cull db = fraction db > 0

animate :: DeadBrick -> DeadBrick
animate db = db { brick=brick' { pos=pos' + Vec2 0 3 }, fraction=fraction' - 0.05 }
  where
    brick' = brick db
    pos' = pos brick'
    fraction' = fraction db


