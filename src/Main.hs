{-# LANGUAGE FlexibleContexts #-}

module Main where

import Data.Word ()
import Control.Monad ( liftM, unless, when )
import Control.Monad.IO.Class ( liftIO, MonadIO )
import Control.Monad.State ( StateT, MonadState, modify, evalStateT, get )
import Control.Monad.Reader ( ReaderT, MonadReader, ask, runReaderT )

import qualified Graphics.UI.SDL as Sdl
import           Graphics.UI.SDL ( Event(..), Keysym(..), SDLKey(..) )

import qualified Graphics.Rendering.OpenGL as Gl
import           Graphics.Rendering.OpenGL ( ($=) )

import           Timer ( Timer, start, defaultTimer, getTimerTicks )

screenWidth :: Int
screenWidth = 640
screenHeight :: Int
screenHeight = 480
screenBpp :: Int
screenBpp = 32

squareWidth :: Int
squareWidth = 20
squareHeight :: Int
squareHeight = 20

halfSquareWidth :: Int
halfSquareWidth = squareWidth `div` 2
halfSquareHeight :: Int
halfSquareHeight = squareHeight `div` 2
 
data Square = Square { pos :: (Int, Int), vel :: (Int, Int) }

defaultSquare :: Square
defaultSquare = Square { pos=(0,0), vel=(0,0) }

handleInput :: Event -> Square -> Square
handleInput (KeyDown (Keysym SDLK_UP _ _)) dot_@Square { vel=(dx,dy) }    = dot_ { vel=(dx, dy - halfSquareHeight) }
handleInput (KeyDown (Keysym SDLK_DOWN _ _)) dot_@Square { vel=(dx,dy) }  = dot_ { vel=(dx, dy + halfSquareHeight) }
handleInput (KeyDown (Keysym SDLK_LEFT _ _)) dot_@Square { vel=(dx,dy) }  = dot_ { vel=(dx - halfSquareWidth, dy) }
handleInput (KeyDown (Keysym SDLK_RIGHT _ _)) dot_@Square { vel=(dx,dy) } = dot_ { vel=(dx + halfSquareWidth, dy) }

handleInput (KeyUp (Keysym SDLK_UP _ _)) dot_@Square { vel=(dx,dy) }    = dot_ { vel=(dx, dy + halfSquareHeight) }
handleInput (KeyUp (Keysym SDLK_DOWN _ _)) dot_@Square { vel=(dx,dy) }  = dot_ { vel=(dx, dy - halfSquareHeight) }
handleInput (KeyUp (Keysym SDLK_LEFT _ _)) dot_@Square { vel=(dx,dy) }  = dot_ { vel=(dx + halfSquareWidth, dy) }
handleInput (KeyUp (Keysym SDLK_RIGHT _ _)) dot_@Square { vel=(dx,dy) } = dot_ { vel=(dx - halfSquareWidth, dy) }

handleInput _ d = d

move :: Square -> Square
move dot_@Square { pos=(x,y), vel=(dx,dy) } = dot_ { pos=(x'', y'') } 
 where
    x'  = x + dx
    y'  = y + dy
    x'' = if x' < 0 || (x' + squareWidth) > screenWidth then x else x' 
    y'' = if y' < 0 || (y' + squareHeight) > screenHeight then y else y'

data AppData = AppData {
    dot :: Square,
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

getSquare :: MonadState AppData m => m Square
getSquare = liftM dot get

putSquare :: MonadState AppData m => Square -> m ()
putSquare t = modify $ \s -> s { dot = t }

modifySquareM :: MonadState AppData m => (Square -> m Square) -> m ()
modifySquareM act = getSquare >>= act >>= putSquare

modifySquare :: MonadState AppData m => (Square -> Square) -> m ()
modifySquare fn = fn `liftM` getSquare >>= putSquare

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
    return (AppConfig screen_, AppData defaultSquare fps_) 

showSquare :: Square -> IO ()
showSquare Square { pos=(x,y) } = do
    -- Move to offset
    Gl.translate $ Gl.Vector3 (fromIntegral x :: Gl.GLfloat)  (fromIntegral y) 0
    -- Start quad
    Gl.renderPrimitive Gl.Quads $ do
        -- Set color to white
        Gl.color $ Gl.Color4 (1 :: Gl.GLfloat) 1 1 1

        -- Draw square
        Gl.vertex $ Gl.Vertex3 (0 :: Gl.GLfloat) 0 0
        Gl.vertex $ Gl.Vertex3 screenWidth' 0 0
        Gl.vertex $ Gl.Vertex3 screenWidth' screenHeight' 0
        Gl.vertex $ Gl.Vertex3 0 screenHeight' 0

    Gl.loadIdentity
 where  screenWidth', screenHeight' :: Gl.GLfloat
        screenWidth'  = fromIntegral screenWidth
        screenHeight' = fromIntegral screenHeight

loop :: AppEnv ()
loop = do

    modifyFPSM $ liftIO . start
    quit_ <- whileEvents $ modifySquare . handleInput

    modifySquare move

    fps_     <- getFPS
    square  <- getSquare

    liftIO $ do
        Gl.clear [Gl.ColorBuffer]

        showSquare square

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


