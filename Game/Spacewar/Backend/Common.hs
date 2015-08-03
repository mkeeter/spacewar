module Game.Spacewar.Backend.Common where

import Data.Bits (setBit, clearBit)
import Data.IORef (modifyIORef)
import Data.List (elemIndex)
import Data.IORef (IORef, readIORef, writeIORef)

import Control.Monad
import System.Exit (exitSuccess)

import Graphics.UI.GLUT (KeyboardMouseCallback, KeyState(..), Key(..),
                         postRedisplay, IdleCallback)

import Game.Spacewar.Controls
import Game.Spacewar.State (State(..), nextState)

keyboard :: [KeyMap] -> IORef [Control] -> KeyboardMouseCallback
keyboard kms ctrls key change _ _ = do
    ctrls' <- readIORef ctrls
    writeIORef ctrls $ zipWith processOne kms ctrls'
    when (key == Char 'q') exitSuccess
    where
        processOne :: KeyMap -> Control -> Control
        processOne km ctrl =
            let op = if change == Down then setBit else clearBit
            in maybe ctrl (op ctrl) $ elemIndex key km

idle :: IORef State -> IORef [Control] -> IdleCallback
idle s' c' = do
    c <- readIORef c'
    modifyIORef s' (nextState c)
    postRedisplay Nothing
