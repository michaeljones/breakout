module Breakout.Collision (
    Collision(..),
    MovingRect(..),
    Rect(..),
    Ray(..),
    Side(..),
    collide,
    intersect
    ) where

import Data.Vect.Float ( Vec2(..), _1, _2, scalarMul )
import Data.Vect.Float.Instances () -- For Num Vec2
import Data.List ( sortBy, minimumBy )

import qualified Breakout.Brick as Br

{- Rectangle with layout:

    b--------c
    |        |
    a--------d
   -}
data Rect = Rect {
    a :: Vec2,
    b :: Vec2,
    c :: Vec2,
    d :: Vec2
}

data Side = Side {
    sideStart :: Vec2,
    sideVector :: Vec2,
    sideNormal :: Vec2
}

data Ray = Ray {
    rayStart :: Vec2,
    rayVector :: Vec2
}

sides :: Rect -> [Side]
sides rect = [
    Side { sideStart=a rect, sideVector=(b rect - a rect), sideNormal=(Vec2 (-1) 0) },
    Side { sideStart=b rect, sideVector=(c rect - b rect), sideNormal=(Vec2 0 (-1)) },
    Side { sideStart=c rect, sideVector=(d rect - c rect), sideNormal=(Vec2 1 0) },
    Side { sideStart=d rect, sideVector=(a rect - d rect), sideNormal=(Vec2 0 1) }
    ]

data MovingRect = MovingRect {
    rect :: Rect,
    vel :: Vec2
}

data Collision = Collision {
    time :: Float,
    pos :: Vec2,
    normal :: Vec2,
    brick :: Br.Brick
} deriving (Eq, Show)

orderCollisions :: Collision -> Collision -> Ordering
orderCollisions c1 c2 = time c1 `compare` time c2

type CollisionData = ([Collision], [Br.Brick])

collide :: MovingRect -> [Br.Brick] -> CollisionData
collide rectBall bricks =
    case sorted of
        [] -> ([], bricks)
        c:cs -> ([c], foldr append bricks' cs)
  where
    brickPairs = map (\b -> (fromBrick b, b)) bricks
    (collisions, bricks') = foldr (collideSingle rectBall) ([], []) brickPairs
    sorted = sortBy orderCollisions collisions
    append c bs = brick c:bs

collideSingle :: MovingRect -> (MovingRect, Br.Brick) -> CollisionData -> CollisionData
collideSingle rectBall (rectBrick, brick') (cols, current) =
    case collideRects rectBall rectBrick of
        Nothing -> (cols, brick':current)
        (Just c) -> (c { brick=brick' }:cols, current)

{- Detect potential collision between two moving rectangles -}
collideRects :: MovingRect -> MovingRect -> Maybe Collision
collideRects mrA mrB =
    case intersections of
        [] -> Nothing
        _ -> Just $ minimumBy orderCollisions intersections
  where
    dir = (vel mrB - vel mrA)  -- relative velocity
    ray = Ray { rayStart=Vec2 0 0, rayVector=dir }
    diff = rectRectDifference (rect mrA) (rect mrB)
    sides' = sides diff
    intersections = foldr (intersect ray) [] sides'


{- Line-line (ray-to-side) intersection test. Approach from:

     http://stackoverflow.com/questions/563198
   -}
intersect :: Ray -> Side -> [Collision] -> [Collision]
intersect ray side col =
    if ( t > 0.0 && t < 1.0 ) && ( u > 0.0 && u < 1.0 )
    then Collision { time=t, pos=(p + (t `scalarMul` r)), normal=sideNormal side, brick=dummyBrick } : col
    else col
  where
    p = rayStart ray
    r = rayVector ray
    q = sideStart side
    s = sideVector side
    t = ((q - p) `cross` s) / (r `cross` s)
    u = ((q - p) `cross` r) / (r `cross` s)
    cross a b = ( _1 a * _2 b ) - ( _2 a * _1 b )
    dummyBrick = Br.Brick { Br.pos=Vec2 0 0, Br.size=Vec2 1 1 }

{- Calculates the Minkowski Difference, A-B, of two rectangles -}
rectRectDifference :: Rect -> Rect -> Rect
rectRectDifference rA rB = Rect {
        a = (a rA - c rB),
        b = (b rA - d rB),
        c = (c rA - a rB),
        d = (d rA - b rB)
    }

fromBrick :: Br.Brick -> MovingRect
fromBrick brick =
    MovingRect {
        rect = Rect {
            a = (Br.pos brick + Vec2 0 (Br.sizeY brick)),
            b = (Br.pos brick),
            c = (Br.pos brick + Vec2 (Br.sizeX brick) 0),
            d = (Br.pos brick + Br.size brick)
        },
        vel = Vec2 0 0 
    }

