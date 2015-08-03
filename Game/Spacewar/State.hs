module Game.Spacewar.State where

import Data.Maybe

import Game.Spacewar.Physics
import Game.Spacewar.Actors
import Game.Spacewar.Controls

--------------------------------------------------------------------------------

data State = Play [Ship] [Shot]

startState :: [a] -> State
startState players =
    let j = fromIntegral $ length players :: Float
    in Play [startShip $ 1/j + 2*i/j - 1 | i <- [0..j-1]] []

--------------------------------------------------------------------------------

nextState :: [Control] -> State -> State
nextState cmds (Play ships shots) =
    if (length $ filter isPresent ships') > 1 || length ships == 1
    then Play ships' $ mapMaybe nextShot shots ++ catMaybes new_shots
    else startState ships'
    where (ships', new_shots) = unzip $ zipWith (nextShip shots) cmds ships

draw :: State -> [[V2f]]
draw (Play ships shots) =
    (concat $ map drawShip ships) ++ (map drawShot shots)
