module Ball
    (
    Ball(..),
    sizeX,
    sizeY,
    move
    ) where

import Data.Default ( Default, def )

data Ball = Ball { pos :: (Int, Int), vel :: (Int, Int) }

instance Default Ball where
    def = Ball { pos=(0,0), vel=(1,2) }

size :: Num a => (a, a)
size = (5, 5)
sizeX :: Num a => a
sizeX = fst size
sizeY :: Num a => a
sizeY = snd size

move :: Ball -> Ball
move ball@Ball { pos=(px,py), vel=(vx,vy) } = ball { pos=(px+vx, py+vy) }

