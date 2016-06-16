module Game.Spacewar.Backend.Common where

import Data.Bits (setBit, clearBit, (.&.))
import Data.List (elemIndex)
import Data.IORef (IORef, readIORef, writeIORef)

import Control.Monad
import System.Exit (exitSuccess)

import SDL

import Game.Spacewar.Controls

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

joystickAxis :: IORef [Control] -> JoyAxisEventData -> IO ()
joystickAxis ctrls event = do
    ctrls' <- readIORef ctrls
    writeIORef ctrls $
        [if index == joyAxisEventWhich event
         then (if axis == 0 then
                    (let c' = c .&. 8 in
                     if      value < -10000     then setBit c' 1
                     else if value >  10000     then setBit c' 0
                     else c')
               else if axis == 1 then
                    (let c' = clearBit c 2 in
                     if value < -10000 then setBit c' 2
                     else c')
               else c)
         else c
         | (c, index) <- zip ctrls' [0..]]
    where value = joyAxisEventValue event
          axis = joyAxisEventAxis event


joystickButton :: IORef [Control] -> JoyButtonEventData -> IO ()
joystickButton ctrls event = do
    ctrls' <- readIORef ctrls
    writeIORef ctrls $
        [if index == joyButtonEventWhich event
         then (if joyButtonEventState event == 1
               then setBit c 3
               else clearBit c 3)
         else c
         | (c, index) <- zip ctrls' [0..]]
