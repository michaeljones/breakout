module Ball
    (
    Ball,
    posX,
    posY,
    sizeX,
    sizeY,
    move,
    collide,
    bat,
    bounce
    ) where

import Data.List ( partition )
import Data.Default ( Default, def )
import Data.Vect.Float ( Vec2(..), dotprod, scalarMul )
import Data.Vect.Float.Instances () -- For Num Vec2

import qualified Plane as Pl
import qualified Paddle as Pd
import qualified Brick as Br

data Ball = Ball { pos :: Vec2, vel :: Vec2 }

instance Default Ball where
    def = Ball { pos = Vec2 50 350, vel = Vec2 3 4 }

size :: Num a => (a, a)
size = (5, 5)
sizeX :: Num a => a
sizeX = fst size
sizeY :: Num a => a
sizeY = snd size

posX :: Ball -> Float
posX Ball { pos=Vec2 x _ } = x

posY :: Ball -> Float
posY Ball { pos=Vec2 _ y } = y

move :: Ball -> Ball
move ball@Ball { pos=p, vel=v } = ball { pos=p + v }

collide :: Pl.Plane -> Ball -> Ball
collide plane ball =
    if ball `penetrated` plane
    then reflect ball plane
    else ball

{- Ball has gone through the plane if the dot product of the normal and the
   vector from the plane to the ball is less than zero -}
penetrated :: Ball -> Pl.Plane -> Bool
penetrated ball plane = ( relBallPos `dotprod` Pl.normal plane ) < 0
  where
    relBallPos = pos ball - Pl.pos plane

reflect :: Ball -> Pl.Plane -> Ball
reflect ball plane = ball { pos=finalpos, vel=finalvel }
   where
     bvel = vel ball
     normal = Pl.normal plane
     normalFraction = bvel `dotprod` normal
     finalvel = bvel - ((2 * normalFraction) `scalarMul` normal)
     bpos = pos ball
     toPlane = Pl.pos plane - bpos
     toPlaneFraction = toPlane `dotprod` normal
     finalpos = bpos + ((1.01 * toPlaneFraction) `scalarMul` normal)

bat :: Pd.Paddle -> Ball -> Ball
bat paddle ball =
    if ball `hit` paddle
    then reflect ball $ Pd.top paddle
    else ball

hit :: Ball -> Pd.Paddle -> Bool
hit ball paddle = 
    ( ball `penetrated` Pd.top paddle )
    && ( ball `penetrated` Pd.left paddle )
    && ( ball `penetrated` Pd.bottom paddle )
    && ( ball `penetrated` Pd.right paddle )

bounce :: [Br.Brick] -> Ball -> (Ball, [Br.Brick])
bounce bricks ball = 
    case partition (ball `hitBrick`) bricks of
        ([],bs) -> (ball, bs)
        ((br:_),bs) -> (reflect ball $ Br.bottom br, bs)

hitBrick :: Ball -> Br.Brick -> Bool
hitBrick ball brick =
    ( ball `penetrated` Br.top brick )
    && ( ball `penetrated` Br.left brick )
    && ( ball `penetrated` Br.bottom brick )
    && ( ball `penetrated` Br.right brick )

