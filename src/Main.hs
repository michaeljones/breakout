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
import           Ball ( Ball(..) )
import qualified Paddle as P
import           Paddle ( Paddle(..) )
import           Timer ( Timer, start, defaultTimer, getTimerTicks )

screenWidth :: Int
screenWidth = 640
screenHeight :: Int
screenHeight = 480
screenBpp :: Int
screenBpp = 32

paddleWidth :: Int
paddleWidth = 20
paddleHeight :: Int
paddleHeight = 20

halfPaddleWidth :: Int
halfPaddleWidth = paddleWidth `div` 2
halfPaddleHeight :: Int
halfPaddleHeight = paddleHeight `div` 2
 
handleInput :: Event -> Paddle -> Paddle
handleInput (KeyDown (Keysym SDLK_LEFT _ _)) dot_@Paddle { P.vel=(dx,dy) }  = dot_ { P.vel=(dx - halfPaddleWidth, dy) }
handleInput (KeyDown (Keysym SDLK_RIGHT _ _)) dot_@Paddle { P.vel=(dx,dy) } = dot_ { P.vel=(dx + halfPaddleWidth, dy) }

handleInput (KeyUp (Keysym SDLK_LEFT _ _)) dot_@Paddle { P.vel=(dx,dy) }  = dot_ { P.vel=(dx + halfPaddleWidth, dy) }
handleInput (KeyUp (Keysym SDLK_RIGHT _ _)) dot_@Paddle { P.vel=(dx,dy) } = dot_ { P.vel=(dx - halfPaddleWidth, dy) }

handleInput _ d = d

data AppData = AppData {
    paddle :: Paddle,
    ball :: Ball,
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

getBall :: MonadState AppData m => m Ball
getBall = liftM ball get

putPaddle :: MonadState AppData m => Paddle -> m ()
putPaddle t = modify $ \s -> s { paddle = t }

putBall :: MonadState AppData m => Ball -> m ()
putBall t = modify $ \s -> s { ball = t }

modifyPaddle :: MonadState AppData m => (Paddle -> Paddle) -> m ()
modifyPaddle fn = fn `liftM` getPaddle >>= putPaddle

modifyBall :: MonadState AppData m => (Ball -> Ball) -> m ()
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


showBall :: Ball -> IO ()
showBall Ball { pos=(x,y) } = do

    -- Move to offset
    Gl.translate $ Gl.Vector3 (fromIntegral x :: Gl.GLfloat)  (fromIntegral y) 0

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
showPaddle Paddle { pos=(x,y) } = do

    -- Move to offset
    Gl.translate $ Gl.Vector3 (fromIntegral x :: Gl.GLfloat)  (fromIntegral y) 0
    Gl.translate $ Gl.Vector3 (0 :: Gl.GLfloat) 420 0

    -- Start paddle
    Gl.renderPrimitive Gl.Quads $ do

        -- Set color to white
        Gl.color $ Gl.Color4 (1 :: Gl.GLfloat) 1 1 1

        -- Draw paddle
        Gl.vertex $ Gl.Vertex3 (0 :: Gl.GLfloat) 0 0
        Gl.vertex $ Gl.Vertex3 (P.sizeX :: Gl.GLfloat) 0 0
        Gl.vertex $ Gl.Vertex3 (P.sizeX :: Gl.GLfloat) P.sizeY 0
        Gl.vertex $ Gl.Vertex3 (0 :: Gl.GLfloat) P.sizeY 0

    Gl.loadIdentity

loop :: AppEnv ()
loop = do

    modifyFPSM $ liftIO . start
    quit_ <- whileEvents $ modifyPaddle . handleInput

    modifyPaddle $ P.move screenWidth screenHeight

    modifyBall B.move

    fps_   <- getFPS
    paddle_ <- getPaddle
    ball_   <- getBall

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


