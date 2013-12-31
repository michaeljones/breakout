module Breakout.Plane
    (
    Plane(..),
    fromTuples
    ) where

import Data.Vect.Float ( Vec2(..) )

data Plane = Plane { pos :: Vec2, normal :: Vec2 }

fromTuples :: (Float,Float) -> (Float,Float) -> Plane
fromTuples (px, py) (nx, ny) = Plane { pos=Vec2 px py, normal=Vec2 nx ny }



