module Game.Spacewar.Backend.Common where

import Data.Bits (setBit, clearBit)
import Data.List (elemIndex)
import Data.IORef (IORef, readIORef, writeIORef)

import Control.Monad
import System.Exit (exitSuccess)

import SDL

import Game.Spacewar.Controls
import Game.Spacewar.State (State(..), nextState)

keyboard :: [KeyMap] -> IORef [Control] -> KeyboardEventData -> IO ()
keyboard kms ctrls kbEvent = do
    ctrls' <- readIORef ctrls
    writeIORef ctrls $ zipWith processOne kms ctrls'
    when (keyboardEventKeyMotion kbEvent == Pressed &&
          keysymKeycode (keyboardEventKeysym kbEvent) == KeycodeQ)
            exitSuccess
    where
        processOne :: KeyMap -> Control -> Control
        processOne km ctrl =
            let op = if (keyboardEventKeyMotion kbEvent == Pressed)
                     then setBit else clearBit
            in maybe ctrl (op ctrl) $ elemIndex (keysymKeycode $ keyboardEventKeysym kbEvent) km
