module Game.Spacewar.Backend.GL where

import Data.IORef

import Linear.V2

import Graphics.UI.GLUT

import Game.Spacewar.State
import Game.Spacewar.Controls
import Game.Spacewar.Physics

import Game.Spacewar.Backend.Common (keyboard)

runBackend :: [KeyMap] -> IO ()
runBackend players = do
    (_progName, _args) <- getArgsAndInitialize
    initialWindowSize $= (Size 400 400)
    initialDisplayMode $= [WithSamplesPerPixel 32]
    _window <- createWindow "spacewΛr"

    state <- newIORef $ startState players
    controls <- newIORef $ replicate (length players) 0

    lineSmooth $= Enabled
    displayCallback $= display state
    idleCallback $= Just (idle state controls)
    keyboardMouseCallback $= Just (keyboard players controls)
    mainLoop

drawLine :: [V2f] -> V2f -> IO ()
drawLine line (V2 dx dy) =
    renderPrimitive LineStrip $
        mapM_ (\(V2 x y) -> vertex $
            Vertex3 (realToFrac (x + dx) :: GLfloat)
                    (realToFrac (y + dy) :: GLfloat)
                    0)
    line

drawLineWrapped :: [V2f] -> IO ()
drawLineWrapped a = mapM_ (drawLine a)
    [V2 x y | x <- [-2, 0, 2], y <- [-2, 0, 2]]

display :: IORef State -> DisplayCallback
display s' = do
    s <- readIORef s'
    clear [ColorBuffer]
    mapM_ drawLineWrapped $ draw s
    flush

idle :: IORef State -> IORef [Control] -> IdleCallback
idle s' c' = do
    c <- readIORef c'
    modifyIORef s' (nextState c)
    postRedisplay Nothing
