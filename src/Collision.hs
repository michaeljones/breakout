module Collision (
    Collision(..),
    MovingRect(..),
    Rect(..),
    Line(..),
    collide,
    intersect
    ) where

import Data.Vect.Float ( Vec2(..), _1, _2 )
import Data.Vect.Float.Instances () -- For Num Vec2
import Data.List ( sortBy, minimumBy )

import qualified Brick as Br

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

data Line = Line {
    start :: Vec2,
    vector :: Vec2
}

sides :: Rect -> [Line]
sides rect = [
    Line { start=a rect, vector=(b rect - a rect) },
    Line { start=b rect, vector=(c rect - b rect) },
    Line { start=c rect, vector=(d rect - c rect) },
    Line { start=d rect, vector=(a rect - d rect) }
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
    velocityLine = Line { start=Vec2 0 0, vector=dir }
    diff = rectRectDifference (rect mrA) (rect mrB)
    sides' = sides diff
    intersections = foldr (intersect velocityLine) [] sides'


{- Line-line intersection test. Approach from:

     http://stackoverflow.com/questions/563198
   -}
intersect :: Line -> Line -> [Collision] -> [Collision]
intersect vel side col =
    if ( t > 0.0 && t < 1.0 ) && ( u > 0.0 && u < 1.0 )
    then Collision { time=t, pos=Vec2 0 0, normal=Vec2 1 0, brick=dummyBrick } : col
    else col
  where
    p = start vel
    r = vector vel
    q = start side
    s = vector side
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

