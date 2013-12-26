module Paddle
    (
    Paddle(..),
    sizeX,
    sizeY,
    move,
    top,
    bottom,
    left,
    right
    ) where

import Data.Vect.Float ( Vec2(..) )
import Data.Vect.Float.Instances () -- For Num Vec2
import Data.Default ( Default, def )

import qualified Plane as Pl

data Paddle = Paddle { pos :: Vec2, vel :: Vec2 }

instance Default Paddle where
    def = Paddle { pos=Vec2 50 420, vel=Vec2 0 0 }

size :: Num a => (a, a)
size = (60, 10)
sizeX :: Num a => a
sizeX = fst size
sizeY :: Num a => a
sizeY = snd size

move :: Int -> Int -> Paddle -> Paddle
move xlim ylim dot@Paddle { pos=Vec2 x y, vel=Vec2 dx dy } = dot { pos=Vec2 x'' y'' } 
 where
    x'  = x + dx
    y'  = y + dy
    x'' = if x' < 0 || (x' + sizeX) > (realToFrac xlim) then x else x' 
    y'' = if y' < 0 || (y' + sizeY) > (realToFrac ylim) then y else y'

top :: Paddle -> Pl.Plane
top paddle = Pl.Plane (pos paddle) $ Vec2 0 (-1)

bottom :: Paddle -> Pl.Plane
bottom paddle = Pl.Plane (pos paddle + (Vec2 0 sizeY)) $ Vec2 0 1

left :: Paddle -> Pl.Plane
left paddle = Pl.Plane (pos paddle) $ Vec2 (-1) 0

right :: Paddle -> Pl.Plane
right paddle = Pl.Plane (pos paddle + (Vec2 sizeX 0)) $ Vec2 1 0

