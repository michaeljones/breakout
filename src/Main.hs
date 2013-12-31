{-# LANGUAGE FlexibleContexts, DisambiguateRecordFields #-}

module Main where

import Control.Monad ( liftM, unless, when, forM_ )
import Control.Monad.IO.Class ( liftIO, MonadIO )
import Control.Monad.State ( StateT, MonadState, modify, evalStateT, get )
import Control.Monad.Reader ( ReaderT, MonadReader, ask, runReaderT )

import           Data.Default ( def )

import qualified Graphics.UI.SDL as Sdl
import           Graphics.UI.SDL ( Event(..), Keysym(..), SDLKey(..) )

import qualified Graphics.Rendering.OpenGL as Gl
import           Graphics.Rendering.OpenGL ( ($=) )

import qualified Brick as Br
import qualified Ball as Ba
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
    ball :: Ba.Ball,
    bricks :: Br.BrickState,
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

getBall :: MonadState AppData m => m Ba.Ball
getBall = liftM ball get

getBricks :: MonadState AppData m => m Br.BrickState
getBricks = liftM bricks get

putPaddle :: MonadState AppData m => Paddle -> m ()
putPaddle t = modify $ \s -> s { paddle = t }

putBall :: MonadState AppData m => Ba.Ball -> m ()
putBall t = modify $ \s -> s { ball = t }

putBricks :: MonadState AppData m => Br.BrickState -> m ()
putBricks t = modify $ \s -> s { bricks = t }

modifyPaddle :: MonadState AppData m => (Paddle -> Paddle) -> m ()
modifyPaddle fn = fn `liftM` getPaddle >>= putPaddle

modifyBall :: MonadState AppData m => (Ba.Ball -> Ba.Ball) -> m ()
modifyBall fn = fn `liftM` getBall >>= putBall

modifyBricks :: MonadState AppData m => (Br.BrickState -> Br.BrickState) -> m ()
modifyBricks fn = fn `liftM` getBricks >>= putBricks

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
    Sdl.setCaption "Breakout" []

    initGL

    fps_ <- start defaultTimer

    let rows = 10
        columns = 8
        bricks_ = concat $ flip map [1..rows] $ (\n -> map (createBrick n) [1..columns])

        appData = AppData {
            paddle=def,
            ball=def,
            bricks=Br.BrickState { current=bricks_, dying=[] },
            fps=fps_ }

    return (AppConfig screen_, appData)

  where

    createBrick y n = Br.Brick (Vec2 (20 + 60 * n) (50 + 20 * y)) (Vec2 50 10)

showBricks :: Br.BrickState -> IO ()
showBricks bs = do
    -- Render dead bricks first so that they are under the active ones
    forM_ (Br.dying bs) $ renderDead
    forM_ (Br.current bs) $ render (Gl.Color4 (1 :: Gl.GLfloat) 1 1 1)
  where
    renderDead db = let c = realToFrac $ Br.fraction db in render (Gl.Color4 (c :: Gl.GLfloat) c c 1) $ Br.brick db
    render colour brick = do

        let x = Br.posX brick
            y = Br.posY brick
            sx = Br.sizeX brick
            sy = Br.sizeY brick

        -- Move to offset
        -- Gl.translate $ Gl.Vector3 (x :: Gl.GLfloat) y 0
        Gl.translate $ Gl.Vector3 (realToFrac x :: Gl.GLfloat) (realToFrac y) 0

        -- Start ball
        Gl.renderPrimitive Gl.Quads $ do

            -- Set color to white
            Gl.color colour

            -- Draw paddle
            Gl.vertex $ Gl.Vertex3 (0 :: Gl.GLfloat) 0 0
            Gl.vertex $ Gl.Vertex3 (realToFrac sx :: Gl.GLfloat) 0 0
            Gl.vertex $ Gl.Vertex3 (realToFrac sx :: Gl.GLfloat) (realToFrac sy) 0
            Gl.vertex $ Gl.Vertex3 (0 :: Gl.GLfloat) (realToFrac sy) 0

        Gl.loadIdentity


showBall :: Ba.Ball -> IO ()
showBall b = do

    let x = Ba.posX b
        y = Ba.posY b

    -- Move to offset
    -- Gl.translate $ Gl.Vector3 (x :: Gl.GLfloat) y 0
    Gl.translate $ Gl.Vector3 (realToFrac x :: Gl.GLfloat) (realToFrac y) 0

    -- Start ball
    Gl.renderPrimitive Gl.Quads $ do

        -- Set color to white
        Gl.color $ Gl.Color4 (1 :: Gl.GLfloat) 1 1 1

        -- Draw paddle
        Gl.vertex $ Gl.Vertex3 (0 :: Gl.GLfloat) 0 0
        Gl.vertex $ Gl.Vertex3 (Ba.sizeX :: Gl.GLfloat) 0 0
        Gl.vertex $ Gl.Vertex3 (Ba.sizeX :: Gl.GLfloat) Ba.sizeY 0
        Gl.vertex $ Gl.Vertex3 (0 :: Gl.GLfloat) Ba.sizeY 0

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
    quit_ <- whileEvents (modifyPaddle . handleInput) (modifyBall . handleTrigger)

    -- Check if the ball is out of bounds
    modifyBall $ Ba.out $ fromTuples (20,fromIntegral screenHeight - 20) (0,-1)

    -- Update the paddle based on user movement
    modifyPaddle $ Pd.move screenWidth screenHeight
    paddle' <- getPaddle

    -- Move the ball either by its velocity or to stick with the paddle
    modifyBall $ Ba.move paddle'

    -- Collide with walls
    modifyBall $ Ba.collide $ fromTuples (20,20) (1,0)
    modifyBall $ Ba.collide $ fromTuples (20,20) (0,1)
    modifyBall $ Ba.collide $ fromTuples (fromIntegral screenWidth - 20,20) (-1,0)

    fps_   <- getFPS

    -- Collide with paddle
    modifyBall $ Ba.bat paddle'

    ball'  <- getBall
    brickState <- getBricks

    -- Collide with bricks
    let (ball'', brickState') = Ba.bounce brickState ball'
        brickState'' = Br.deathSequence brickState'

    modifyBall $ const ball''
    modifyBricks $ const brickState''

    liftIO $ do
        Gl.clear [Gl.ColorBuffer]

        showPaddle paddle'

        showBricks brickState'

        -- Draw ball after bricks so that it appears on top of dead ones
        showBall ball''

        Sdl.glSwapBuffers

        ticks <- getTimerTicks fps_
        when (ticks < secsPerFrame) $ do
            Sdl.delay $ secsPerFrame - ticks

    unless quit_ loop

 where
    framesPerSecond = 60
    secsPerFrame    = 1000 `div` framesPerSecond

whileEvents :: MonadIO m => (Event -> m ()) -> (Event -> m ()) -> m Bool
whileEvents movement trigger = do
    event <- liftIO Sdl.pollEvent
    case event of
        Quit -> return True
        NoEvent -> return False
        _       ->  do
            _ <- handleQuit event
            movement event
            trigger event
            whileEvents movement trigger

{- Handle Space bar to free ball from the paddle. Reset ball velocity to
   default on release -}
handleTrigger :: Event -> Ba.Ball -> Ba.Ball
handleTrigger (KeyDown (Keysym SDLK_SPACE _ _)) ba = ba { Ba.mode=Ba.Free, Ba.vel=Ba.vel def }
handleTrigger _ ba = ba

-- Event handler for 'q' to quit application
handleQuit :: MonadIO m => Event -> m Bool
handleQuit (KeyDown (Keysym SDLK_q _ _)) = liftIO $ Sdl.tryPushEvent Quit
handleQuit _ = return True

runLoop :: AppConfig -> AppData -> IO ()
runLoop = evalStateT . runReaderT loop

main :: IO ()
main = Sdl.withInit [Sdl.InitEverything] $ do -- withInit calls quit for us.
    (env, state_) <- initEnv
    runLoop env state_


