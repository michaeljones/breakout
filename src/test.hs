module Main where

import Test.Tasty ( defaultMain, TestTree, testGroup )
import Test.Tasty.HUnit ( testCase, (@?=) )

import Data.Vect.Float ( Vec2(..), _1, _2 )

import qualified Breakout.Collision as Co
import           Breakout.Collision ( Ray(..), Side(..), Collision(..), intersect )

import qualified Breakout.Brick as Br
import           Breakout.Brick ( Brick )

main = defaultMain tests

tests :: TestTree
tests = testGroup "Collision Unit Tests"
    [ testCase "Line intersection" $
        (intersect 
            Ray { rayStart=Vec2 0 0, rayVector=Vec2 10 0 }
            Side { sideStart=Vec2 5 (-5), sideVector=Vec2 0 10, sideNormal=Vec2 1 0 } [])
        @?= [Collision { time=0.5, pos=Vec2 0 0, normal=Vec2 1 0, brick=Br.Brick { Br.pos=Vec2 0 0, Br.size=Vec2 1 1 }}]
    ]


