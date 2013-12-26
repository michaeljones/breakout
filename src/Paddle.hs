module Paddle
    (
    Paddle(..),
    sizeX,
    sizeY,
    move
    ) where

import Data.Default ( Default, def )

data Paddle = Paddle { pos :: (Int, Int), vel :: (Int, Int) }

instance Default Paddle where
    def = Paddle { pos=(0,0), vel=(0,0) }

size :: Num a => (a, a)
size = (60, 10)
sizeX :: Num a => a
sizeX = fst size
sizeY :: Num a => a
sizeY = snd size

move :: Int -> Int -> Paddle -> Paddle
move xlim ylim dot@Paddle { pos=(x,y), vel=(dx,dy) } = dot { pos=(x'', y'') } 
 where
    x'  = x + dx
    y'  = y + dy
    x'' = if x' < 0 || (x' + sizeX) > xlim then x else x' 
    y'' = if y' < 0 || (y' + sizeY) > ylim then y else y'

