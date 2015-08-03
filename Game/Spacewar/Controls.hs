module Game.Spacewar.Controls where

import Data.Word
import Graphics.UI.GLUT

data Command = TurnLeft | TurnRight | Accel | Shoot deriving (Show, Enum)
type KeyMap = [Key]
type Control = Word8

player1 :: KeyMap
player1 = [SpecialKey KeyLeft, SpecialKey KeyRight,
           SpecialKey KeyUp, Char ' ']

player2 :: KeyMap
player2 = [Char 'a', Char 'd', Char 'w', Char 's']

playernum :: KeyMap
playernum = [Char '4', Char '6', Char '8', Char '0']
