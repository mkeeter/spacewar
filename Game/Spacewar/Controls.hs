module Game.Spacewar.Controls where

import Data.Word
import SDL

data Command = TurnLeft | TurnRight | Accel | Shoot deriving (Show, Enum)
type KeyMap = [Keycode]
type Control = Word8

player1 :: KeyMap
player1 = [KeycodeLeft, KeycodeRight, KeycodeUp, KeycodeSpace]

player2 :: KeyMap
player2 = [KeycodeA, KeycodeD, KeycodeW, KeycodeS]

playernum :: KeyMap
playernum = [Keycode4, Keycode6, Keycode8, Keycode0]
