{-# LANGUAGE FlexibleContexts, DisambiguateRecordFields #-}

module Main where

import Control.Monad ( liftM, unless, when )
import Control.Monad.IO.Class ( liftIO, MonadIO )
import Control.Monad.State ( StateT, MonadState, modify, evalStateT, get )
import Control.Monad.Reader ( ReaderT, MonadReader, ask, runReaderT )

import           Data.Default ( def )

import qualified Graphics.UI.SDL as Sdl
import           Graphics.UI.SDL ( Event(..), Keysym(..), SDLKey(..) )

import qualified Graphics.Rendering.OpenGL as Gl
import           Graphics.Rendering.OpenGL ( ($=) )

import qualified Ball as B
import qualified Paddle as Pd
import           Paddle ( Paddle(..) )
import           Plane ( fromTuples )
import           Timer ( Timer, start, defaultTimer, getTimerTicks )

import Data.Vect.Float ( Vec2(..) )
import Data.Vect.Float.Instances () -- For Num Vec2

screenWidth :: Int
screenWidth = 640
screenHeight :: Int
screenHeight = 480
screenBpp :: Int
screenBpp = 32

paddleWidth :: Fractional a => a
paddleWidth = 20
paddleHeight :: Fractional a => a
paddleHeight = 20

halfPaddleWidth :: Fractional a => a
halfPaddleWidth = paddleWidth / 2
halfPaddleHeight :: Fractional a => a
halfPaddleHeight = paddleHeight / 2
 
handleInput :: Event -> Paddle -> Paddle
handleInput (KeyDown (Keysym SDLK_LEFT _ _)) pd@Paddle { Pd.vel=v }  = pd { Pd.vel=v - (Vec2 halfPaddleWidth 0) }
handleInput (KeyDown (Keysym SDLK_RIGHT _ _)) pd@Paddle { Pd.vel=v } = pd { Pd.vel=v + (Vec2 halfPaddleWidth 0) }

handleInput (KeyUp (Keysym SDLK_LEFT _ _)) pd = pd { Pd.vel=Vec2 0 0 }
handleInput (KeyUp (Keysym SDLK_RIGHT _ _)) pd = pd { Pd.vel=Vec2 0 0 }

handleInput _ d = d

data AppData = AppData {
    paddle :: Paddle,
    ball :: B.Ball,
    fps :: Timer
}

data AppConfig = AppConfig {
    screen :: Sdl.Surface
}

type AppState = StateT AppData IO
type AppEnv = ReaderT AppConfig AppState

getFPS :: MonadState AppData m => m Timer
getFPS = liftM fps get

putFPS :: MonadState AppData m => Timer -> m ()
putFPS t = modify $ \s -> s { fps = t }

modifyFPSM :: MonadState AppData m => (Timer -> m Timer) -> m ()
modifyFPSM act = getFPS >>= act >>= putFPS

getPaddle :: MonadState AppData m => m Paddle
getPaddle = liftM paddle get

getBall :: MonadState AppData m => m B.Ball
getBall = liftM ball get

putPaddle :: MonadState AppData m => Paddle -> m ()
putPaddle t = modify $ \s -> s { paddle = t }

putBall :: MonadState AppData m => B.Ball -> m ()
putBall t = modify $ \s -> s { ball = t }

modifyPaddle :: MonadState AppData m => (Paddle -> Paddle) -> m ()
modifyPaddle fn = fn `liftM` getPaddle >>= putPaddle

modifyBall :: MonadState AppData m => (B.Ball -> B.Ball) -> m ()
modifyBall fn = fn `liftM` getBall >>= putBall

getScreen :: MonadReader AppConfig m => m Sdl.Surface
getScreen = liftM screen ask

initGL :: IO ()
initGL = do
    Gl.clearColor $= Gl.Color4 (0 :: Gl.GLfloat) 0 0 0
    Gl.matrixMode $= Gl.Projection
    Gl.loadIdentity    
    Gl.ortho 0 (fromIntegral screenWidth) (fromIntegral screenHeight) 0 (-1) 1

    Gl.matrixMode $= Gl.Modelview 0
    Gl.loadIdentity

initEnv :: IO (AppConfig, AppData)
initEnv = do
    Sdl.glSetAttribute Sdl.glDoubleBuffer 1
    screen_ <- Sdl.setVideoMode screenWidth screenHeight screenBpp [Sdl.OpenGL]
    Sdl.setCaption "OpenGL Test" []

    initGL

    fps_ <- start defaultTimer
    return (AppConfig screen_, AppData { paddle=def, ball=def, fps=fps_ }) 


showBall :: B.Ball -> IO ()
showBall b = do

    let x = B.posX b
        y = B.posY b

    -- Move to offset
    -- Gl.translate $ Gl.Vector3 (x :: Gl.GLfloat) y 0
    Gl.translate $ Gl.Vector3 (realToFrac x :: Gl.GLfloat) (realToFrac y) 0

    -- Start ball
    Gl.renderPrimitive Gl.Quads $ do

        -- Set color to white
        Gl.color $ Gl.Color4 (1 :: Gl.GLfloat) 1 1 1

        -- Draw paddle
        Gl.vertex $ Gl.Vertex3 (0 :: Gl.GLfloat) 0 0
        Gl.vertex $ Gl.Vertex3 (B.sizeX :: Gl.GLfloat) 0 0
        Gl.vertex $ Gl.Vertex3 (B.sizeX :: Gl.GLfloat) B.sizeY 0
        Gl.vertex $ Gl.Vertex3 (0 :: Gl.GLfloat) B.sizeY 0

    Gl.loadIdentity

showPaddle :: Paddle -> IO ()
showPaddle Paddle { pos=Vec2 x y } = do

    -- Move to offset
    Gl.translate $ Gl.Vector3 (realToFrac x :: Gl.GLfloat) (realToFrac y) 0

    -- Start paddle
    Gl.renderPrimitive Gl.Quads $ do

        -- Set color to white
        Gl.color $ Gl.Color4 (1 :: Gl.GLfloat) 1 1 1

        -- Draw paddle
        Gl.vertex $ Gl.Vertex3 (0 :: Gl.GLfloat) 0 0
        Gl.vertex $ Gl.Vertex3 (Pd.sizeX :: Gl.GLfloat) 0 0
        Gl.vertex $ Gl.Vertex3 (Pd.sizeX :: Gl.GLfloat) Pd.sizeY 0
        Gl.vertex $ Gl.Vertex3 (0 :: Gl.GLfloat) Pd.sizeY 0

    Gl.loadIdentity

loop :: AppEnv ()
loop = do

    modifyFPSM $ liftIO . start
    quit_ <- whileEvents $ modifyPaddle . handleInput

    modifyPaddle $ Pd.move screenWidth screenHeight

    modifyBall B.move

    -- Collide with walls
    modifyBall $ B.collide $ fromTuples (20,20) (1,0)
    modifyBall $ B.collide $ fromTuples (20,20) (0,1)
    modifyBall $ B.collide $ fromTuples (fromIntegral screenWidth - 20,20) (-1,0)
    modifyBall $ B.collide $ fromTuples (20,fromIntegral screenHeight - 20) (0,-1)

    fps_   <- getFPS
    paddle_ <- getPaddle
    ball_   <- getBall

    -- Collide with paddle
    modifyBall $ B.bat paddle_

    liftIO $ do
        Gl.clear [Gl.ColorBuffer]

        showPaddle paddle_

        showBall ball_

        Sdl.glSwapBuffers

        ticks <- getTimerTicks fps_
        when (ticks < secsPerFrame) $ do
            Sdl.delay $ secsPerFrame - ticks

    unless quit_ loop
 where
    framesPerSecond = 60
    secsPerFrame    = 1000 `div` framesPerSecond

whileEvents :: MonadIO m => (Event -> m ()) -> m Bool
whileEvents act = do
    event <- liftIO Sdl.pollEvent
    case event of
        Quit -> return True
        NoEvent -> return False
        _       ->  do
            act event
            whileEvents act

runLoop :: AppConfig -> AppData -> IO ()
runLoop = evalStateT . runReaderT loop

main :: IO ()
main = Sdl.withInit [Sdl.InitEverything] $ do -- withInit calls quit for us.
    (env, state_) <- initEnv
    runLoop env state_


