module Main where

import Test.Tasty ( defaultMain, TestTree, testGroup )
import Test.Tasty.HUnit ( testCase, (@?=) )

import Data.Vect.Float ( Vec2(..), _1, _2 )

import qualified Collision as Co
import Collision ( Line(..), Collision(..), intersect )

import qualified Brick as Br
import Brick ( Brick )

main = defaultMain tests

tests :: TestTree
tests = testGroup "Collision Unit Tests"
    [ testCase "Line intersection" $
        (intersect 
            Line { start=Vec2 0 0, vector=Vec2 10 0 }
            Line { start=Vec2 5 (-5), vector=Vec2 0 10 } [])
        @?= [Collision { time=0.5, pos=Vec2 0 0, normal=Vec2 1 0, brick=Br.Brick { Br.pos=Vec2 0 0, Br.size=Vec2 1 1 }}]
    ]


