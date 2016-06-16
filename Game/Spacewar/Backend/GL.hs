module Game.Spacewar.Backend.GL where

import Data.IORef
import Data.Vector.Storable (Vector, fromList)

import Linear.V2
import Linear.V4
import Linear.Affine (Point(..))
import Foreign.C.Types (CInt)
import Data.Text (pack)

import SDL

import Game.Spacewar.State
import Game.Spacewar.Controls
import Game.Spacewar.Physics

import Game.Spacewar.Backend.Common (keyboard, joystickAxis, joystickButton)

runBackend :: [KeyMap] -> IO ()
runBackend players = do
    initializeAll
    window <- createWindow (pack "Spacewar")
                           (defaultWindow {windowInitialSize = V2 400 400,
                                           windowResizable = False})
    renderer <- createRenderer window (-1) defaultRenderer

    state <- newIORef $ startState players
    controls <- newIORef $ replicate (length players) 0

    joysticks <- availableJoysticks
    mapM_ openJoystick joysticks
    appLoop renderer players controls state

processEvent :: [KeyMap] -> IORef [Control] -> Event -> IO ()
processEvent players controls event = do
    print (show event)
    case eventPayload event of
        KeyboardEvent kb -> keyboard players controls kb
        JoyAxisEvent js -> joystickAxis controls js
        JoyButtonEvent js -> joystickButton controls js
        _ -> return ()

appLoop :: Renderer -> [KeyMap] -> IORef [Control] -> IORef State -> IO ()
appLoop renderer players controls state = do
    -- Handle all events (which update control bitfields and quits as needed)
    mapEvents (processEvent players controls)

    -- Get the next state from the current state
    controls' <- readIORef controls
    modifyIORef state $ nextState controls'

    clear renderer
    display renderer state
    present renderer

    delay 20

    appLoop renderer players controls state

display :: Renderer -> IORef State -> IO ()
display renderer s' = do
    s <- readIORef s'
    rendererDrawColor renderer $= V4 0 0 0 0
    clear renderer
    mapM_ (\pts -> drawLines renderer (convertLine pts)) $ draw s

convertLine :: [V2f] -> Vector (Point V2 CInt)
convertLine pts =
    fromList [P $ V2 (round $ (x + 1) * 200)
                     (round $ (y + 1) * 200) | V2 x y <- pts]

{-
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
-}
