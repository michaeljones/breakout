module Breakout.Ball (
    Ball(..),
    Mode(..),
    posX,
    posY,
    sizeX,
    sizeY,
    move,
    collide,
    bat,
    bounce,
    out
    ) where

import Data.List ( partition )
import Data.Default ( Default, def )
import Data.Vect.Float ( Vec2(..), dotprod, scalarMul, _1, len, normalize )
import Data.Vect.Float.Instances () -- For Num Vec2

import qualified Breakout.Plane as Pl
import qualified Breakout.Paddle as Pd
import qualified Breakout.Brick as Br
import qualified Breakout.Collision as Co
import           Breakout.Types ( HasPos, HasSize )

data Mode = Bound
          | Free
          deriving Eq

data Ball = Ball { _pos :: Vec2, vel :: Vec2, mode :: Mode }

instance Default Ball where
    def = Ball { _pos = Vec2 50 350, vel = Vec2 0 (-4), mode=Bound }

instance HasPos Ball where
    pos = _pos

instance HasSize Ball where
    size = Vec2 5 5

{- Handle free & bound motion of the Ball -}
move :: Pd.Paddle -> Ball -> Ball
move _ ball@Ball { pos=p, vel=v, mode=m } | m == Free = ball { pos=p + v }
move paddle ball =
    ball { pos=(Pd.pos paddle + (Vec2 (0.5 * Pd.sizeX) (-10))) }

{- For collision detection between planes and the ball. Particularly useful for
   the screen edges -}
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

{- Reflect the ball in the plane -}
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
    then adjustVelocity (paddlePosition) (paddlePosition + paddleWidth) $ reflect ball top
    else ball
  where
    paddlePosition = Pd.pos paddle
    paddleWidth = Vec2 Pd.sizeX 0
    top = Pd.top paddle

adjustVelocity :: Vec2 -> Vec2 -> Ball -> Ball
adjustVelocity left right ball = ball { vel=vel'' }
  where
    fraction = (_1 bpos - midpoint) / halfwidth
    midpoint = (_1 right + _1 left) / 2
    halfwidth = (_1 right - _1 left) / 2
    bpos = pos ball
    bvel = vel ball
    bmag = len bvel
    vel' = bvel + ( ( bmag * fraction * 0.6 ) `scalarMul` (Vec2 1 (-1)) )
    vel'' = bmag `scalarMul` ( normalize vel' )

hit :: Ball -> Pd.Paddle -> Bool
hit ball paddle = 
    ( ball `penetrated` Pd.top paddle )
    && ( ball `penetrated` Pd.left paddle )
    && ( ball `penetrated` Pd.bottom paddle )
    && ( ball `penetrated` Pd.right paddle )


{- Interaction between the bricks and the ball -}
bounce :: Br.BrickState -> Ball -> (Ball, Br.BrickState)
bounce brickState ball =
    case Co.collide (ballToMovingRect ball) bricks of
        ([],_) -> (ball, brickState)
        (col:_, bricks') -> (ball', brickState')
          where
            ball' = bounce' ball col
            brickState' = brickState {
                Br.current=bricks',
                Br.dying=Br.DeadBrick { Br.brick=(Co.brick col), Br.fraction=1.0 } : dying
                }
  where
    bricks = Br.current brickState
    dying = Br.dying brickState
    bounce' ball col = ball { pos=(pos ball + (Co.time col `scalarMul` vel ball)), vel=vel' }
      where
        normal = Co.normal col
        normalFraction = (vel ball) `dotprod` normal
        vel' = (vel ball) - ((2 * normalFraction) `scalarMul` normal)

speedUp :: Float -> Ball -> Ball
speedUp frac ball@Ball { vel=vel' } = ball { vel=frac `scalarMul` vel' }

out :: Pl.Plane -> Ball -> Ball
out plane ball = 
    if ( relBallPos `dotprod` Pl.normal plane ) < 0
    then ball { mode = Bound }
    else ball
  where
    relBallPos = pos ball - Pl.pos plane

ballToMovingRect :: Ball -> Co.MovingRect
ballToMovingRect Ball { pos=pos', vel=vel' } =
    Co.MovingRect {
        Co.rect = Co.Rect {
            Co.a = (pos' + Vec2 0 sizeY),
            Co.b = pos',
            Co.c = (pos' + Vec2 sizeX 0),
            Co.d = (pos' + Vec2 sizeX sizeY )
        },
        Co.vel = vel'
    }
