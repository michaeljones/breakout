
module Timer (
    Timer(), defaultTimer,
    start,
    stop,
    getTimerTicks,
    pause,
    unpause,
    isStarted,
    isPaused
    ) where

import qualified Graphics.UI.SDL.Time as SdlTime 
import Data.Word

data Timer = Timer { startTicks :: Word32, pausedTicks :: Word32, paused :: Bool, started :: Bool }

defaultTimer = Timer { startTicks=0, pausedTicks=0, paused=False, started=False }

start :: Timer -> IO Timer
start timer = SdlTime.getTicks >>= \ticks -> return $ timer { startTicks=ticks, started=True,paused=False }

stop :: Timer -> Timer
stop timer = timer { paused=False, started=False }

getTimerTicks :: Timer -> IO Word32
getTimerTicks Timer { started=False } = return 0
getTimerTicks Timer { started=True, paused=True, pausedTicks=pausedTicks' } = return pausedTicks'
getTimerTicks Timer { started=True, paused=False, startTicks=st } = SdlTime.getTicks >>= \currTicks -> return $ currTicks - st

pause :: Timer -> IO Timer
pause timer@Timer { started=True, paused=False, startTicks=st } = SdlTime.getTicks >>= \currTicks ->    return $ timer { pausedTicks=(currTicks - st), paused=True, started=True }
pause timer = return timer

unpause :: Timer -> IO Timer
unpause timer@Timer { paused=False } = return timer
unpause timer@Timer { paused=True, pausedTicks=pausedTicks' } =
     SdlTime.getTicks >>= \currTicks -> return $ timer { startTicks=(currTicks - pausedTicks'), pausedTicks=0, paused=False }

isStarted :: Timer -> Bool
isStarted Timer { started=s } = s

isPaused :: Timer -> Bool
isPaused Timer { paused=p } = p

