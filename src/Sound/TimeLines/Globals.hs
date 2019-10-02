module Sound.TimeLines.Globals where

import Data.IORef
import Sound.TimeLines.Types
import System.IO.Unsafe (unsafePerformIO)
--import Data.Global


-- | Global reference of the time Window over which
-- | to render each TimeLine
{-# NOINLINE globalWindowRef #-}
-- the "NOINLINE" statement makes sure to never
-- replace globalWindowRef with its body, thus creating
-- another IORef
globalWindowRef :: IORef Window
globalWindowRef = unsafePerformIO $ newIORef (0, 1)

-- | A global reference to the current session
{-# NOINLINE globalSessionRef #-}
globalSessionRef :: IORef Session
globalSessionRef = unsafePerformIO $ newIORef defaultSession
